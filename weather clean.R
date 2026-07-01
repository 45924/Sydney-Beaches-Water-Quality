library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(patchwork)
library(viridis)

weather_path <- "weather.csv"
out_dir      <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

set.seed(42)
theme_set(theme_minimal(base_size = 12))


# ============================================================================
# 1. IMPORT
# ============================================================================

weather_raw <- read_csv(weather_path, show_col_types = FALSE)
# glimpse(weather_raw)
# skim(weather_raw)


# ============================================================================
# 2. CLEANING
# ----------------------------------------------------------------------------
# This file is very clean (no NAs, no duplicate dates, no gaps, plausible
# ranges). We still apply the same disciplined pipeline so the method is
# consistent and any future, messier weather extract is handled the same way:
#   - parse the date
#   - drop exact duplicates
#   - flag physically impossible values as NA (typing/sensor errors):
#       * temperatures outside a generous Sydney envelope
#       * a day where max_temp < min_temp (logically impossible)
#       * negative rainfall
# ============================================================================

TEMP_LOW <- -5; TEMP_HIGH <- 50    # generous Sydney air-temp envelope (C)

weather_clean <- weather_raw |>
  distinct() |>
  mutate(date = ymd(date)) |>
  arrange(date) |>
  mutate(
    max_temp_C = if_else(max_temp_C < TEMP_LOW | max_temp_C > TEMP_HIGH,
                         NA_real_, max_temp_C),
    min_temp_C = if_else(min_temp_C < TEMP_LOW | min_temp_C > TEMP_HIGH,
                         NA_real_, min_temp_C),
    # logical impossibility: max colder than min -> null both for that day
    bad_pair = !is.na(max_temp_C) & !is.na(min_temp_C) & max_temp_C < min_temp_C,
    max_temp_C = if_else(bad_pair, NA_real_, max_temp_C),
    min_temp_C = if_else(bad_pair, NA_real_, min_temp_C),
    precipitation_mm = if_else(precipitation_mm < 0, NA_real_, precipitation_mm)
  ) |>
  select(-bad_pair)

# Check for missing calendar days (none expected, but worth verifying)
full_dates <- tibble(date = seq(min(weather_clean$date),
                                max(weather_clean$date), by = "day"))
n_gaps <- nrow(anti_join(full_dates, weather_clean, by = "date"))
message(sprintf("Calendar gaps in daily series: %d", n_gaps))

# ============================================================================
# 3. MISSING-VALUE IMPUTATION (same skewness rule as the water-quality script)
# ----------------------------------------------------------------------------
# Rule: skew > 1 -> MEDIAN, else MEAN. Computed on cleaned, non-missing values.
# (Here it typically fills 0 cells, but the pipeline is identical by design.)
# ============================================================================

calc_skew <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^(3/2)
}

impute_by_skew <- function(x, name = "") {
  sk <- calc_skew(x)
  use_median <- !is.na(sk) && sk > 1
  fill <- if (use_median) median(x, na.rm = TRUE) else mean(x, na.rm = TRUE)
  message(sprintf("  %-18s skew = %6.2f  -> %-6s (filled %d)",
                  name, sk, if (use_median) "median" else "mean", sum(is.na(x))))
  replace(x, is.na(x), fill)
}

num_cols <- c("max_temp_C", "min_temp_C", "precipitation_mm")

message("Imputation summary (skew > 1 -> median, else mean):")
weather_clean <- weather_clean |>
  mutate(across(all_of(num_cols), \(x) impute_by_skew(x, cur_column())))


# ============================================================================
# 4. FEATURE ENGINEERING
# ----------------------------------------------------------------------------
# Calendar parts, derived temps, rainfall bands, and — most importantly —
# ANTECEDENT RAINFALL (lag-1 and rolling 3-day). Bacterial spikes in the water
# data follow rain by 1–2 days, so these are the columns that let you join
# weather to water quality and tell the "pollution after rain" story in Tableau.
# Rolling sums use explicit lags (no extra package needed); the series has no
# date gaps, so lag() correctly means "previous calendar day".
# ============================================================================

weather <- weather_clean |>
  arrange(date) |>
  mutate(
    year   = year(date),
    month  = month(date, label = TRUE),
    season = case_when(
      month(date) %in% c(12, 1, 2) ~ "Summer",
      month(date) %in% c(3, 4, 5)  ~ "Autumn",
      month(date) %in% c(6, 7, 8)  ~ "Winter",
      TRUE                         ~ "Spring"
    ) |> factor(levels = c("Summer", "Autumn", "Winter", "Spring")),
    yday = yday(date),
    
    # derived temperatures
    avg_temp_c   = (max_temp_C + min_temp_C) / 2,
    temp_range_c = max_temp_C - min_temp_C,
    hot_day_30   = max_temp_C >= 30,
    hot_day_35   = max_temp_C >= 35,
    
    # rainfall: wet day = >= 1 mm (BOM "rain day" is >=0.2 mm; 1 mm is a common
    # threshold that ignores trace amounts — adjust if you prefer 0.2)
    is_wet_day = precipitation_mm >= 1,
    rain_band = case_when(
      precipitation_mm < 1   ~ "Dry",
      precipitation_mm < 10  ~ "Light",
      precipitation_mm < 50  ~ "Moderate",
      TRUE                   ~ "Heavy"
    ) |> factor(levels = c("Dry", "Light", "Moderate", "Heavy")),
    
    # antecedent rainfall — the join-relevant features
    rain_lag1    = lag(precipitation_mm, 1),
    rain_lag2    = lag(precipitation_mm, 2),
    rain_3day    = precipitation_mm + lag(precipitation_mm, 1) + lag(precipitation_mm, 2),
    wet_recently = coalesce(rain_3day, 0) >= 10   # Beachwatch flags spikes after ~10 mm
  )


# ============================================================================
# 5. EXPLORATORY VISUALISATIONS
# ============================================================================

# 5.1 Temperature seasonality — monthly climatology (mean max & min)
clim <- weather |>
  group_by(month) |>
  summarise(across(c(max_temp_C, min_temp_C, avg_temp_c), mean), .groups = "drop")

p_clim <- clim |>
  ggplot(aes(month)) +
  geom_ribbon(aes(ymin = min_temp_C, ymax = max_temp_C, group = 1),
              fill = "#fdae61", alpha = 0.4) +
  geom_line(aes(y = max_temp_C, group = 1), colour = "#d7191c", linewidth = 1) +
  geom_line(aes(y = min_temp_C, group = 1), colour = "#2c7bb6", linewidth = 1) +
  geom_point(aes(y = max_temp_C), colour = "#d7191c") +
  geom_point(aes(y = min_temp_C), colour = "#2c7bb6") +
  labs(title = "Sydney temperature seasonality",
       subtitle = "Monthly mean daily max (red) and min (blue), 1991–2025",
       x = NULL, y = "Temperature (\u00B0C)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p_clim

# 5.2 Long-run warming signal — annual mean max temp (complete years only)
yr_temp <- weather |>
  group_by(year) |>
  summarise(mean_max = mean(max_temp_C), n = n(), .groups = "drop") |>
  filter(n >= 350)   # drop the partial final year (data ends April 2025)

p_warm <- yr_temp |>
  ggplot(aes(year, mean_max)) +
  geom_line(colour = "grey70") +
  geom_point(colour = "#d7191c") +
  geom_smooth(method = "lm", se = TRUE, colour = "#d7191c", fill = "#fdae61") +
  labs(title = "Annual mean daily-maximum temperature",
       subtitle = "Linear trend shown; complete years only",
       x = NULL, y = "Mean daily max (\u00B0C)")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p_warm

# 5.3 Rainfall distribution (wet days only; log scale for the long right tail)
p_rain_dist <- weather |>
  filter(precipitation_mm >= 1) |>
  ggplot(aes(precipitation_mm)) +
  geom_histogram(bins = 40, fill = "#2c7bb6", colour = "white") +
  scale_x_log10(labels = comma) +
  labs(title = "Daily rainfall is heavily right-skewed",
       subtitle = "Wet days only (>= 1 mm); log scale",
       x = "Rainfall (mm)", y = "Number of days") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p_rain_dist

# 5.4 Annual rainfall totals — interannual variability
p_rain_yr <- weather |>
  group_by(year) |>
  summarise(total_rain = sum(precipitation_mm), n = n(), .groups = "drop") |>
  filter(n >= 350) |>
  ggplot(aes(year, total_rain)) +
  geom_col(fill = "#2c7bb6") +
  scale_y_continuous(labels = comma, expand = expansion(c(0, 0.05))) +
  labs(title = "Total annual rainfall", subtitle = "Complete years only",
       x = NULL, y = "Rainfall (mm)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p_rain_yr

# 5.5 Monthly rainfall climatology — which months are wettest?
p_rain_mon <- weather |>
  group_by(year, month) |>
  summarise(monthly = sum(precipitation_mm), .groups = "drop") |>
  group_by(month) |>
  summarise(mean_monthly = mean(monthly), .groups = "drop") |>
  ggplot(aes(month, mean_monthly, fill = mean_monthly)) +
  geom_col(show.legend = FALSE) +
  scale_fill_viridis(option = "mako", direction = -1) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  labs(title = "Average monthly rainfall",
       subtitle = "Sydney's wettest months cluster in late summer / autumn",
       x = NULL, y = "Mean rainfall (mm)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p_rain_mon

# 5.6 Calendar heatmap — daily max temp across every year (the map's stand-in)
p_heat <- weather |>
  ggplot(aes(yday, factor(year), fill = max_temp_C)) +
  geom_tile() +
  scale_fill_viridis(option = "inferno", name = "Max \u00B0C") +
  scale_x_continuous(
    breaks = c(1, 60, 121, 182, 244, 305, 365),
    labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"),
    expand = c(0, 0)) +
  scale_y_discrete(limits = rev) +
  labs(title = "Daily maximum temperature — 1991 to 2025",
       subtitle = "Each row is a year; the summer band runs Dec–Feb",
       x = NULL, y = NULL) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p_heat

# 5.7 Hot-day frequency trend — heat extremes over time
p_hot <- weather |>
  group_by(year) |>
  summarise(d30 = sum(hot_day_30), d35 = sum(hot_day_35), n = n(), .groups = "drop") |>
  filter(n >= 350) |>
  pivot_longer(c(d30, d35), names_to = "thr", values_to = "days") |>
  mutate(thr = recode(thr, d30 = "\u2265 30 \u00B0C", d35 = "\u2265 35 \u00B0C")) |>
  ggplot(aes(year, days, colour = thr)) +
  geom_line(linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.6) +
  scale_colour_manual(values = c("#FF7F50", "#DC143C"), name = "Hot days") +
  labs(title = "Frequency of hot days per year",
       x = NULL, y = "Number of days") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
p_hot

# ---- Save EDA plots --------------------------------------------------------
ggsave(file.path(out_dir, "w01_climatology.png"),  p_clim,      width = 8, height = 5, dpi = 150)
ggsave(file.path(out_dir, "w02_warming.png"),      p_warm,      width = 8, height = 5, dpi = 150)
ggsave(file.path(out_dir, "w03_rain_dist.png"),    p_rain_dist, width = 8, height = 5, dpi = 150)
ggsave(file.path(out_dir, "w04_rain_annual.png"),  p_rain_yr,   width = 8, height = 5, dpi = 150)
ggsave(file.path(out_dir, "w05_rain_monthly.png"), p_rain_mon,  width = 8, height = 5, dpi = 150)
ggsave(file.path(out_dir, "w06_heatmap.png"),      p_heat,      width = 10, height = 7, dpi = 150)
ggsave(file.path(out_dir, "w07_hot_days.png"),     p_hot,       width = 8, height = 5, dpi = 150)

overview <- (p_clim | p_rain_mon) / (p_warm | p_rain_yr)
ggsave(file.path(out_dir, "w00_overview.png"), overview, width = 14, height = 9, dpi = 150)

# ============================================================================
# 6. EXPORT TIDY DATA FOR TABLEAU
# ============================================================================
weather_export <- weather |>
  select(date, year, month, season, yday,
         max_temp_C, min_temp_C, avg_temp_c, temp_range_c,
         hot_day_30, hot_day_35,
         precipitation_mm, is_wet_day, rain_band,
         rain_lag1, rain_lag2, rain_3day, wet_recently,
         latitude, longitude)

write_csv(weather_export, file.path(out_dir, "weather_clean.csv"))
