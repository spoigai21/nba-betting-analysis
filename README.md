# nba-market-model

A statistical modeling project (in R) that predicts NBA outcomes and evaluates those
predictions against betting-market lines. The system builds a predictive model,
incorporates injury/lineup and news signals, generates predictions, logs them with
timestamps to an auditable track record, and honestly evaluates whether the model
shows any real edge.

> **Framing:** This is a data-science / quantitative-modeling project about market
> efficiency and prediction — not a "betting bot." The goal is a clean data pipeline,
> a sound predictive model, and an *honest evaluation* of edge. "No edge" is a valid,
> informative result.

---

## What this project does

1. Pulls NBA game/player data and historical betting lines (free sources).
2. Builds a predictive model (e.g., predicting game totals or a player stat).
3. Incorporates **injury / lineup / news signals** (via open-source APIs) — including
   coach-revealed strategy like resting a starter or changing a rotation.
4. **Backtests** against historical lines to measure edge (fast, on past data).
5. **Forward-tests** by generating timestamped predictions for upcoming games, logging
   them to a CSV, and recording results as games happen (slow, but fully honest).
6. Evaluates with proper metrics (ROI incl. vig, closing-line value, sample size).

---

## Guiding principles (read first — these keep the analysis honest)

1. **No look-ahead bias.** Predictions use ONLY information available before tip-off.
2. **Include the vig.** Every simulated bet accounts for the sportsbook margin and the
   actual odds offered (e.g., -110, -115).
3. **Beating the line is the test.** Predicting correctly isn't enough — the model must
   beat the market's line. Closing-line value (CLV) is the gold standard.
4. **Large samples only.** Dozens of bets is noise. Need 100+ before results mean anything.
5. **Flat betting.** Consistent unit size — no betting more when "feeling good."
6. **Timestamped predictions.** Forward-test predictions are logged by code BEFORE games,
   so the track record is auditable and can't be edited in hindsight.
7. **Human-in-the-loop learning.** When the model fails, WE analyze why and adjust —
   no autonomous self-learning agent (it overfits to noise).

---

## Data sources (all free)

- **Game lines + results (historical):** Kaggle — "MGM Grand NBA betting data"
  (closing lines, 2021–2026) or "NBA Betting Data 2007–2026" (more history).
- **Game / player stats:** `hoopR` R package (pulls from official/ESPN endpoints).
- **Injury / lineup / news signals:** open-source APIs and libraries that wrap ESPN and
  other sources (e.g., `hoopR` for injuries/rosters) — **use these, do not raw-scrape
  ESPN's site.** Treat news as a *secondary* signal.
- **Current lines (forward-testing):** The Odds API free tier (current odds; historical
  and player-prop line data are paid — this project uses free game-line history for
  backtesting and free current lines for forward-testing).

---

## Project structure

```
nba-market-model/
├── README.md
├── INSTRUCTIONS.md         # step-by-step beginner setup + build guide
├── data/                   # raw data (Kaggle CSVs) — gitignored if large
│   └── nba_betting.csv
├── R/
│   ├── 01_load_data.R      # load & clean data into a tidy dataset
│   ├── 02_features.R       # engineer pre-game features (see factors below)
│   ├── 03_model.R          # build the predictive model
│   ├── 04_backtest.R       # evaluate vs. historical lines (incl. vig, CLV)
│   ├── 05_forward.R        # generate predictions, log to track-record CSV
│   └── 06_news_signals.R   # (advanced) pull injury/lineup/news signals
├── output/
│   └── track_record.csv    # timestamped predictions + results (auditable log)
└── .gitignore
```

---

## Build roadmap (do in order)

### Phase 1 — Data
- Install R/RStudio + packages; download a free Kaggle dataset into `data/`
- Load in R; clean into a tidy table (one row per game: pre-game info + line + result)

### Phase 2 — Baseline model
- Engineer a small set of pre-game features (start simple: team strength + rest)
- Build a simple, explainable model (`lm`/`glm`) to predict the total (or spread)
- Predict on a held-out set the model never trained on

### Phase 3 — Honest backtest
- Compare predictions to the historical line; simulate bets (with vig)
- Track win rate, ROI, and closing-line value over a LARGE sample
- Visualize (ggplot2): profit over time, calibration; add confidence intervals

### Phase 4 — Forward-test logging
- Script generates predictions for upcoming games
- Appends each (predicted value, line, odds, timestamp) to `output/track_record.csv`
- A second step fills in actual results after games; computes running units

### Phase 5 — Iterate (human-in-the-loop)
- Analyze where the model fails; form a hypothesis (a factor to add)
- Add it, re-test honestly; resist overfitting (more features ≠ more edge)

### Phase 6 — News & availability signals (advanced; your edge)
- Pull **injury / lineup / availability** data via open-source APIs (structured,
  high-value: "is a star sitting tonight?").
- Incorporate **coach-revealed strategy / rotation news** (e.g., benching a starter,
  resting on a back-to-back) as a pre-game signal.
- **Use going-forward (forward-testing):** this news breaks hours before tip-off, so it's
  capturable and honest live, but very hard to backtest cleanly. Early on, apply it
  manually using your own NBA knowledge as you log predictions; automate later.
- Turning free-text news into features is an NLP sub-project — a genuine differentiator,
  but add it only after the base model works.

---

## Factors to consider (feature ideas)

Start with a small subset; let NBA knowledge guide which signals the market underweights.

- **Team strength/form:** record, point differential, off/def efficiency, pace, home/away
- **Rest & schedule:** days rest, back-to-backs, schedule density, travel
- **Injuries / availability:** key players out, load management, return-from-injury rust
- **Matchup:** head-to-head, stylistic matchups, pace mismatch
- **Situational (your edge):** trap games, revenge games, tanking, motivation, coaching
- **News / lineup signals (your edge):** benched starters, rotation changes, rest days
  revealed pre-game
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

---

## Getting started

See **INSTRUCTIONS.md** for a click-by-click beginner setup guide (install R, load your
first dataset, and explore it). Built in R (tidyverse, hoopR, ggplot2, lm/glm).
Understanding each step matters more than speed.
