library(tidyverse)   # readr, dplyr, ggplot2, tidyr, stringr, forcats
library(lubridate)   # date/time parsing
library(janitor)     # clean_names(), tabyl()
library(scales)      # axis formatting (log, percent, comma)
library(patchwork)   # combine ggplots
library(viridis)     # colour-blind-safe palettes
library(leaflet)     # interactive Beachwatch-style map
library(htmlwidgets) # save the leaflet map as standalone HTML

# Read files
wq_path      <- "water_quality.csv"
weather_path <- "weather.csv"
out_dir      <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Reproducibility
set.seed(42)
theme_set(theme_minimal(base_size = 12))


# ============================================================================
# 1. IMPORT
# ============================================================================
wq_raw <- read_csv(wq_path, show_col_types = FALSE) |> clean_names()
weather_raw <- read_csv(weather_path, show_col_types = FALSE) |> clean_names()
colSums(is.na(wq_raw))

# ============================================================================
# 2. CLEANING
# ----------------------------------------------------------------------------
# Issues found during exploration:
#  (a) Typing errors in water_temperature_c: 21+ values > 40 C, up to 1040 C
#      (a Sydney swim site is never ~1000 C — almost certainly a digit/decimal
#       entry error). We null physically implausible values so they don't
#       poison the skewness calculation, then impute them in step 3.
#  (b) conductivity_ms_cm: a 0 reading (sensor error) and a handful of absurd
#      highs (> 80,000). NOTE: the column is *labelled* mS/cm but the values
#      (~53,800 at marine sites) are really micro-S/cm (uS/cm). Seawater is
#      ~50,000 uS/cm, so we treat that scale as the truth and flag outliers.
#      Freshwater sites (e.g. Windsor Beach ~248) are genuinely low — keep them.
#  (c) enterococci_cfu_100ml: extreme highs are REAL pollution spikes (post-rain
#      bacterial blooms), so we keep them — they are the signal, not noise.
#  (d) 20 exact duplicate rows; text columns trimmed for safety.
# ============================================================================
# Plausibility bounds (defensible, documented)
TEMP_MIN <- 8;   TEMP_MAX <- 32      # realistic Sydney water temp range (C)
COND_MIN <- 0;   COND_MAX <- 80000   # uS/cm; 0 = sensor error, >80k implausible

wq_clean <- wq_raw |>
  # 2.1 drop exact duplicate records
  distinct() |>
  # 2.2 standardize text (guards against stray white space / case drift)
  mutate(across(c(region, council, swim_site), \(x) str_squish(x))) |>
  # 2.3 parse date & time properly
  mutate(
    date = ymd(date),
    time = hms::as_hms(time),          # NA if missing/unparseable
    datetime = as_datetime(paste(date, time))
  ) |>
  # 2.4 null out physically impossible numeric values (typing / sensor errors)
  mutate(
    water_temperature_c = if_else(
      water_temperature_c < TEMP_MIN | water_temperature_c > TEMP_MAX,
      NA_real_, water_temperature_c),
    conductivity_ms_cm = if_else(
      conductivity_ms_cm <= COND_MIN | conductivity_ms_cm > COND_MAX,
      NA_real_, conductivity_ms_cm),
    enterococci_cfu_100ml = if_else(
      enterococci_cfu_100ml < 0, NA_real_, enterococci_cfu_100ml)  # none, but safe
  )

# ============================================================================
# 3. MISSING-VALUE IMPUTATION (skewness rule)
# ----------------------------------------------------------------------------
# Rule (as specified): if skewness > 1  -> impute with MEDIAN (robust to skew)
#                      else             -> impute with MEAN
# ============================================================================
calc_skew <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^(3/2)
}

# Impute one vector by the skew rule; report what it did
impute_by_skew <- function(x, name = "") {
  sk <- calc_skew(x)
  use_median <- !is.na(sk) && sk > 1
  fill <- if (use_median) median(x, na.rm = TRUE) else mean(x, na.rm = TRUE)
  n_missing <- sum(is.na(x))
  message(sprintf("  %-22s skew = %6.2f  -> %-6s (filled %d)",
                  name, sk, if (use_median) "median" else "mean", n_missing))
  replace(x, is.na(x), fill)
}

# Impute by the skew rule — enterococci ONLY
num_cols <- c("enterococci_cfu_100ml")
wq_clean <- wq_clean |>
  mutate(across(all_of(num_cols),
                \(x) impute_by_skew(x, cur_column())))

# water_temperature_c and conductivity were ~60% missing.Therefore, we keep  
# the missing values as NA in those columns.

# ============================================================================
# 4. FEATURE ENGINEERING
# ----------------------------------------------------------------------------
# Beachwatch classification (per single sample), aligned to NSW guidance:
#   Good  (green) : enterococci <= 40   ("safe swimming" band, <41 cfu/100mL)
#   Fair  (amber) : 41 – 200            (elevated, caution)
#   Poor  (red)   : > 200               (alert / closure level)
# ============================================================================
wq <- wq_clean |>
  mutate(
    year    = year(date),
    month   = month(date, label = TRUE),
    # Austral seasons
    season = case_when(
      month(date) %in% c(12, 1, 2)  ~ "Summer",
      month(date) %in% c(3, 4, 5)   ~ "Autumn",
      month(date) %in% c(6, 7, 8)   ~ "Winter",
      TRUE                          ~ "Spring"
    ) |> factor(levels = c("Summer", "Autumn", "Winter", "Spring")),
    water_quality = case_when(
      enterococci_cfu_100ml <= 40  ~ "Good",
      enterococci_cfu_100ml <= 200 ~ "Fair",
      TRUE                         ~ "Poor"
    ) |> factor(levels = c("Good", "Fair", "Poor")),
    is_safe = enterococci_cfu_100ml <= 40
  )

# Traffic-light palette reused everywhere (matches the Beachwatch map image)
qual_cols <- c(Good = "#2e8b57", Fair = "#f0ad4e", Poor = "#d9534f")

# ============================================================================
# 5. EXPLORATORY VISUALISATIONS
# ============================================================================
# 5.1 Distribution of enterococci (log10 scale reveals the right skew & spikes)
p_dist <- wq |>
  filter(enterococci_cfu_100ml > 0) |>
  ggplot(aes(enterococci_cfu_100ml)) +
  geom_histogram(bins = 50, fill = "blue", colour = "white") +
  geom_vline(xintercept = c(40, 200), linetype = "dashed", colour = "grey") +
  scale_x_log10(labels = comma) +
  annotate("text", x = 40, y = Inf, label = "Good \u2264 40", vjust = 2,
           hjust = 1.1, size = 3) +
  annotate("text", x = 200, y = Inf, label = "Poor > 200", vjust = 2,
           hjust = -0.1, size = 3) +
  labs(title = "Enterococci are heavily right-skewed",
       subtitle = "Log scale; dashed lines = Beachwatch safe / alert thresholds",
       x = "Enterococci (cfu/100 mL, log10)", y = "Number of samples")
p_dist

# 5.2 Overall sample breakdown by water-quality category
p_cat <- wq |>
  count(water_quality) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(water_quality, pct, fill = water_quality)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = percent(pct, accuracy = 0.1)), vjust = -0.3) +
  scale_fill_manual(values = qual_cols, guide = "none") +
  scale_y_continuous(labels = percent, expand = expansion(c(0, 0.1))) +
  labs(title = "Most samples are swim-safe, but a meaningful tail is not",
       x = NULL, y = "Share of all samples")
p_cat

# 5.3 Ranking: which beaches are safest / riskiest? (% of samples in "Good")
site_summary <- wq |>
  group_by(region, swim_site, latitude, longitude) |>
  summarise(
    n_samples   = n(),
    median_ent  = median(enterococci_cfu_100ml),
    pct_safe    = mean(is_safe),
    pct_poor    = mean(water_quality == "Poor"),
    .groups = "drop"
  ) |>
  filter(n_samples >= 30) # drop sparsely sampled sites for a fair ranking

p_rank <- site_summary |>
  slice_max(pct_poor, n = 15) |>
  mutate(swim_site = fct_reorder(swim_site, pct_poor)) |>
  ggplot(aes(pct_poor, swim_site, fill = pct_poor)) +
  geom_col() +
  scale_fill_viridis(option = "rocket", direction = -1, guide = "none") +
  scale_x_continuous(labels = percent, expand = expansion(c(0, 0.05))) +
  labs(title = "15 swim sites with the highest share of 'Poor' samples",
       subtitle = "Sites with >= 30 samples",
       x = "Share of samples > 200 cfu/100 mL", y = NULL)
p_rank

# 5.4 Long-run trend: yearly exceed rate (% of samples NOT safe)
p_trend <- wq |>
  group_by(year) |>
  summarise(exceed_rate = mean(!is_safe), n = n(), .groups = "drop") |>
  filter(n >= 100) |>
  ggplot(aes(year, exceed_rate)) +
  geom_line(colour = "#d9534f", linewidth = 1) +
  geom_point(colour = "#d9534f") +
  geom_smooth(method = "loess", se = FALSE, colour = "grey40",
              linetype = "dashed", linewidth = 0.6) +
  scale_y_continuous(labels = percent) +
  labs(title = "Share of unsafe samples over time (1991–2025)",
       x = NULL, y = "% samples > 40 cfu/100 mL")
p_trend

# 5.5 Seasonality — pollution clusters in the wetter/warmer months
p_season <- wq |>
  group_by(season) |>
  summarise(exceed_rate = mean(!is_safe), .groups = "drop") |>
  ggplot(aes(season, exceed_rate, fill = season)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = percent(exceed_rate, accuracy = 0.1)), vjust = -0.3) +
  scale_fill_viridis_d(option = "mako", end = 0.85) +
  scale_y_continuous(labels = percent, expand = expansion(c(0, 0.1))) +
  labs(title = "Unsafe-sample rate by season", x = NULL,
       y = "% samples > 40 cfu/100 mL")
p_season

# 5.6 Region comparison (log scale to handle the skew)
p_region <- wq |>
  filter(enterococci_cfu_100ml > 0) |>
  mutate(region = fct_reorder(region, enterococci_cfu_100ml, median)) |>
  ggplot(aes(enterococci_cfu_100ml, region, fill = region)) +
  geom_boxplot(outlier.alpha = 0.1, show.legend = FALSE) +
  geom_vline(xintercept = 40, linetype = "dashed", colour = "grey30") +
  scale_x_log10(labels = comma) +
  scale_fill_viridis_d(option = "viridis", end = 0.9) +
  labs(title = "Enterococci distribution by region",
       subtitle = "Dashed line = safe-swimming threshold (40 cfu/100 mL)",
       x = "Enterococci (cfu/100 mL, log10)", y = NULL)
p_region

# ---- Save EDA plots --------------------------------------------------------
ggsave(file.path(out_dir, "01_distribution.png"), p_dist,   width = 8, height = 5, dpi = 150)
ggsave(file.path(out_dir, "02_category.png"),     p_cat,    width = 7, height = 5, dpi = 150)
ggsave(file.path(out_dir, "03_ranking.png"),      p_rank,   width = 8, height = 6, dpi = 150)
ggsave(file.path(out_dir, "04_trend.png"),        p_trend,  width = 8, height = 5, dpi = 150)
ggsave(file.path(out_dir, "05_season.png"),       p_season, width = 7, height = 5, dpi = 150)
ggsave(file.path(out_dir, "06_region.png"),       p_region, width = 8, height = 5, dpi = 150)

# Combined overview panel
overview <- (p_dist | p_cat) / (p_season | p_trend)
ggsave(file.path(out_dir, "00_overview.png"), overview, width = 14, height = 9, dpi = 150)

# ============================================================================
# 6. BEACHWATCH INTERACTIVE MAP
# ----------------------------------------------------------------------------
# Each swim site gets ONE marker, coloured by its typical status:
#   green  = mostly safe (median enterococci <= 40)
#   amber  = elevated    (median 41–200)
#   red    = poor        (median > 200)
# ============================================================================

map_data <- site_summary |>
  mutate(
    status = case_when(
      median_ent <= 40  ~ "Good",
      median_ent <= 200 ~ "Fair",
      TRUE              ~ "Poor"
    ) |> factor(levels = c("Good", "Fair", "Poor")),
    popup = sprintf(
      "<b>%s</b><br/>Region: %s<br/>Samples: %s<br/>Median enterococci: %s cfu/100 mL<br/>Safe samples: %s",
      swim_site, region, comma(n_samples), comma(round(median_ent)),
      percent(pct_safe, accuracy = 1))
  )

pal <- colorFactor(unname(qual_cols), levels = c("Good", "Fair", "Poor"))

beach_map <- leaflet(map_data) |>
  addProviderTiles(providers$CartoDB.Positron) |> # clean light basemap, like the image
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    color = "white", weight = 1,
    fillColor = ~pal(status), fillOpacity = 0.9,
    radius = ~scales::rescale(sqrt(n_samples), to = c(5, 12)),
    popup = ~popup, label = ~swim_site
  ) |>
  addLegend("bottomright", pal = pal, values = ~status,
            title = "Typical water quality", opacity = 1) |>
  setView(lng = 151.21, lat = -33.85, zoom = 11) # central Sydney, like the image
beach_map

# View interactively in RStudio, and save a shareable standalone file
saveWidget(beach_map, file.path(out_dir, "beachwatch_map.html"),
           selfcontained = TRUE)

# ============================================================================
# 7. EXPORT TIDY DATA FOR TABLEAU
# ============================================================================
wq_export <- wq |>
  select(region, council, swim_site, latitude, longitude,
         date, year, month, season,
         enterococci_cfu_100ml, water_temperature_c, conductivity_ms_cm,
         water_quality, is_safe)

write_csv(wq_export, file.path(out_dir, "water_quality_clean.csv"))
write_csv(site_summary, file.path(out_dir, "site_summary.csv"))