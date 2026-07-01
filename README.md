# 🌊 Sydney Beachwatch — Water Quality & Rainfall Analytics (1991–2025)

An end-to-end data-analytics project examining **34 years of water quality across 79 Sydney swim sites**, joined to daily weather data, to understand how clean Sydney's beaches are — and what drives contamination.

**Stack:** R (data cleaning, EDA, visualisation) → Tableau (interactive dashboard)

### 🔗 [**View the live interactive dashboard on Tableau Public →**](https://public.tableau.com/app/profile/john.phung3553/viz/SydneyBeachesWaterQualityandWeatherRecords/SydneyBeachwatchwaterqualityrainfall19912025)

<!-- Add a screenshot of your dashboard here once exported: -->
<!-- ![Dashboard preview](docs/dashboard_preview.png) -->

---

## 📌 Highlights

| | |
|---|---|
| 🧪 **123,510** water samples analysed | 📍 **79** swim sites · 5 regions · 16 councils |
| 🟢 **84.56%** of samples safe to swim | 🔴 **4.68%** breached the pollution-alert level |
| 📅 **1991–2025** (34 years) | 🌧️ Rainfall identified as the key driver of contamination |

---

## ❓ The question

NSW's Beachwatch program measures *enterococci* bacteria to flag faecal contamination at beaches. This project asks: **how safe is the water at Sydney's beaches, how does it vary by location and season, and how strongly does rainfall push it into the danger zone?**

Samples are classified using NSW-aligned traffic-light thresholds:

| 🟢 Good | 🟡 Fair | 🔴 Poor |
|---------|---------|---------|
| ≤ 40 cfu/100 mL | 41 – 200 | > 200 (alert) |

---

## 🗂️ Repository structure

```
sydney-beachwatch/
├── data/                       # raw inputs
│   ├── water_quality.csv
│   └── weather.csv
├── R/                          # analysis scripts
│   ├── 01_water_quality_eda.R  # clean + impute + EDA + Beachwatch map
│   └── 02_weather_eda.R        # clean + feature-engineer + EDA
├── output/                     # generated artefacts (Tableau-ready)
│   ├── water_quality_clean.csv
│   ├── weather_clean.csv
│   ├── site_summary.csv
│   ├── beachwatch_map.html
│   └── *.png                   # EDA charts
├── tableau/
│   └── sydney_beachwatch.twbx  # packaged workbook
├── docs/
│   └── report.md               # full project report
└── README.md
```

---

## 🔬 Method

**1. Cleaning (R).** The water-quality data carried real defects — impossible temperatures (up to 1040 °C), sensor-error conductivity values, and duplicate rows — each nulled or removed deliberately, *before* computing any summary statistic. Extreme enterococci readings were kept because they represent genuine pollution spikes. The weather data, by contrast, was pristine: complete daily coverage with no gaps across 34 years.

**2. Missing values.** A skewness rule (skew > 1 → median, else mean) was applied to **enterococci only** (0.25% missing). Temperature and conductivity (~60% missing) were **intentionally left as `NA`** — imputing that proportion with a constant would fabricate data and flatten variance. They serve as context only.

**3. Feature engineering.** Added traffic-light water-quality categories, an `is_safe` flag, calendar parts, and — the bridge between datasets — **antecedent-rainfall features** (`rain_lag1`, `rain_lag2`, `rain_3day`) that link each sample to recent rain.

**4. Dashboard (Tableau).** The two clean tables are joined on `date` via a **relationship** (avoids double-counting). Site classification uses a `{FIXED [Swim Site]}` LOD; safety metrics are ratio measures. The dashboard features a Beachwatch-style map, a riskiest-beaches ranking, and trend / seasonal / rainfall panels, all cross-filtered.

---

## ▶️ Reproduce it

**Requirements:** R (≥ 4.1) and RStudio.

```r
# install packages (first run only)
install.packages(c("tidyverse","lubridate","janitor","skimr","scales",
                   "patchwork","viridis","leaflet","htmlwidgets"))

# then, from the project root, run in order:
source("R/01_water_quality_eda.R")   # -> output/water_quality_clean.csv, map, plots
source("R/02_weather_eda.R")         # -> output/weather_clean.csv, plots
```

Open Tableau, connect to the two files in `output/`, relate them on `date`, and rebuild — or open `tableau/sydney_beachwatch.twbx` directly.

---

## 📊 Key findings

- **Sydney's water is usually safe (84.56%)** — but the aggregate hides real variation between sites and conditions.
- **Enclosed harbour/estuary sites** carry a disproportionate share of "Poor" samples versus open surf beaches.
- **Rainfall is the strongest predictor of contamination** — exceedance rates climb steadily with antecedent (multi-day) rainfall, the project's central insight.
- **Seasonal clustering** of exceedances aligns with Sydney's wetter months.

---

## ⚠️ Limitations

A single weather station is applied city-wide (rainfall link is directional, not site-exact); temperature and conductivity are ~60% missing and un-imputed; per-sample traffic-light bands are used for interpretability rather than official seasonal-percentile beach grades.

---

## 🛠️ Tools

`R` · `tidyverse` · `ggplot2` · `leaflet` · `janitor` · `Tableau Public`

## 👤 Author

**John Phung** — [Tableau Public profile](https://public.tableau.com/app/profile/john.phung3553/vizzes)

*Data: NSW Beachwatch (water quality) and daily Sydney weather observations, 1991–2025.*
