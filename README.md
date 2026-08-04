# NBA Player Performance Modeling & Betting-Market Analysis

A statistical modeling project (in R) that predicts NBA outcomes and evaluates those
predictions against betting-market lines. The system generates predictions, logs them
with timestamps to an auditable track-record file, and evaluates whether the model
shows any real edge — with rigorous, honest methodology throughout.

> **Framing:** This is a data-science / quantitative-modeling project about market
> efficiency and prediction, not a "betting bot." The goal is to build a clean data
> pipeline, a predictive model, and — most importantly — an *honest evaluation* of
> whether the model has measurable edge. "No edge" is a valid, informative result.

---

## What this project does

1. Pulls NBA game/player data and historical betting lines (free sources).
2. Builds a predictive model (e.g., predicting game totals or a player stat).
3. **Backtests** the model against historical lines to measure edge (fast, on past data).
4. **Forward-tests** by generating predictions for upcoming games, logging them with
   timestamps to a CSV, and recording results as games happen (slow, but fully honest).
5. Evaluates performance with proper metrics (ROI incl. vig, closing-line value,
   sample size / confidence).

---

## Guiding principles (read first — these keep the analysis honest)

1. **No look-ahead bias.** Predictions use ONLY information available before tip-off.
2. **Include the vig.** Every simulated bet accounts for the sportsbook margin and the
   actual odds offered (e.g., -110, -115). Winning 52% at -115 can still lose money.
3. **Beating the line is the test.** Predicting correctly isn't enough — the model must
   beat the market's line to have edge. Closing-line value (CLV) is the gold standard.
4. **Large samples only.** Dozens of bets is noise. Need 100+ before results mean anything.
5. **Flat betting.** Consistent unit size — no betting more when "feeling good."
6. **Timestamped predictions.** Forward-test predictions are logged by code BEFORE games,
   so the track record is auditable and can't be edited in hindsight.
7. **Human-in-the-loop learning.** When the model fails, WE analyze why and adjust —
   no autonomous self-learning agent (it overfits to noise).

---

## Prerequisites & Setup

### 1. Install R and RStudio
- **R** (the language): download from https://cran.r-project.org — pick your OS, install.
- **RStudio** (the editor/IDE that makes R much easier): download the free version from
  https://posit.co/download/rstudio-desktop/ — install after R.
- Open RStudio. You'll write and run code here.

### 2. Install the R packages this project uses
In the RStudio console, run:
```r
install.packages(c("tidyverse", "hoopR", "lubridate", "readr"))
```
- **tidyverse** — data manipulation (dplyr) + visualization (ggplot2). The core toolkit.
- **hoopR** — pulls free NBA data (schedules, box scores, player stats).
- **lubridate** — handling dates (rest days, scheduling).
- **readr** — reading/writing CSV files (for the track-record log).

### 3. Get the data (free)
**Game lines + results (historical, free):**
- Kaggle: search "NBA Betting Data 2007–2026" or "MGM Grand NBA betting data".
  Download the CSV(s). These include spreads, totals, moneylines, and results.
- Place the file(s) in the `data/` folder of this project.

**Game/player stats (free, via code):**
- `hoopR` pulls these directly in R — no manual download needed.

**Current lines for forward-testing (free):**
- The Odds API free tier (https://the-odds-api.com) gives current odds — get a free
  API key. (Historical/player-prop line data is paid; this project uses free game-line
  history for backtesting and free current lines for forward-testing.)

---

## Suggested project structure
```
nba-model/
├── README.md
├── data/                # raw data (Kaggle CSVs) — gitignored if large
│   └── nba_betting.csv
├── R/
│   ├── 01_load_data.R   # load & clean data into a tidy dataset
│   ├── 02_features.R    # engineer pre-game features (see factors below)
│   ├── 03_model.R       # build the predictive model
│   ├── 04_backtest.R    # evaluate vs. historical lines (incl. vig, CLV)
│   └── 05_forward.R     # generate predictions, log to track-record CSV
├── output/
│   └── track_record.csv # timestamped predictions + results (auditable log)
└── .gitignore
```

---

## Build roadmap (do in order)

### Phase 1 — Data (start here)
- [ ] Install R/RStudio + packages
- [ ] Download a free Kaggle NBA betting dataset into `data/`
- [ ] Load it in R (`readr::read_csv`), inspect it (`glimpse`, `head`)
- [ ] Clean into a tidy table: one row per game, pre-game info + line + result

### Phase 2 — Baseline model
- [ ] Engineer a small set of pre-game features (start simple: team strength + rest)
- [ ] Build a simple, explainable model (`lm`/`glm`) to predict the total (or spread)
- [ ] Predict on a held-out set the model never trained on

### Phase 3 — Honest backtest
- [ ] Compare predictions to the historical line; simulate bets (with vig)
- [ ] Track win rate, ROI, and closing-line value over a LARGE sample
- [ ] Visualize with ggplot2 (profit over time, calibration); add confidence intervals

### Phase 4 — Forward-test logging
- [ ] Script that generates predictions for upcoming games
- [ ] Appends each prediction (predicted value, line, odds, timestamp) to
      `output/track_record.csv`
- [ ] Second step fills in actual results after games; computes running units

### Phase 5 — Iterate (human-in-the-loop)
- [ ] Analyze where the model fails; form a hypothesis (a factor to add)
- [ ] Add it, re-test honestly; resist overfitting (more features ≠ more edge)

---

## Factors to consider (feature ideas)
Start with a small subset; let NBA knowledge guide which signals the market may
underweight.
- **Team strength/form:** record, point differential, off/def efficiency, pace, home/away
- **Rest & schedule:** days rest, back-to-backs, schedule density, travel
- **Injuries/availability:** key players out, load management, return-from-injury rust
- **Matchup:** head-to-head, stylistic matchups, pace mismatch
- **Situational (your edge):** trap games, revenge games, tanking, motivation, coaching
- **Market:** line movement (opening vs current); closing line (evaluation only, never input)

---

## Track-record CSV columns (the auditable log)
| date | game | (player) | stat | prediction | line | odds | bet | result | win_loss | units | timestamp |

- `timestamp` proves the prediction was made before the game.
- `odds` is required to compute true profit (vig).
- `units` tracks flat-bet profit over time.

---

## Evaluation metrics
- **ROI** (incl. vig)
- **Win rate** vs. break-even (~52.4% at -110)
- **Closing-Line Value (CLV)** — did we beat the closing line? (key edge signal)
- **Sample size + confidence interval** — is the result meaningful or noise?
- **Calibration** — when the model says 60%, does it happen ~60% of the time?

---

## What this project is / isn't
- **IS:** a rigorous data pipeline + predictive model + honest market-efficiency
  evaluation; a genuine quant/data-science skill demonstration.
- **IS NOT:** a guaranteed money-maker or an autonomous self-learning agent. If there's
  no edge, "no edge" is the honest, valuable finding.

## Notes on learning R
Built in R (tidyverse for data, ggplot2 for viz, lm/glm for modeling). Each phase
introduces concepts as needed — data wrangling first, then modeling, then evaluation.
Understanding each step matters more than speed; the goal is to be able to explain
every part of the pipeline.