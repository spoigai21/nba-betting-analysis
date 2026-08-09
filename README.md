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
├── instructions.md         # step-by-step beginner setup + build guide
├── run_all.R               # runs the whole backtest pipeline (01 → 04)
├── data/                   # raw data (Kaggle CSVs) — gitignored if large
│   ├── nba_betting.csv
│   └── processed/          # tidy tables written by 01/02
├── R/
│   ├── config.R            # every tunable setting, in one place
│   ├── utils.R             # odds maths, CLV signs, rolling helpers, ML selection
│   ├── track_record.R      # the auditable log: schema + append-only writes
│   ├── 00_setup.R          # packages, .env, project options
│   ├── 01_load_data.R      # load & clean data into a tidy dataset
│   ├── 02_features.R       # engineer pre-game features (see factors below)
│   ├── 03_model.R          # build the predictive model (walk-forward)
│   ├── 04_backtest.R       # evaluate vs. historical lines (incl. vig, CLV)
│   ├── 05_forward.R        # generate predictions, log to track-record CSV
│   ├── 06_news_signals.R   # (advanced) injury/lineup news + NLP extraction
│   ├── 07_usage_model.R    # who absorbs usage when a player sits -> props
│   ├── 08_diagnostics.R    # where does it fail? (Phase 5 hypothesis generator)
│   └── 99_make_sample_data.R  # synthetic dataset, for running without Kaggle
├── tests/
│   ├── run_tests.R         # odds maths, settlement signs, no-look-ahead, schemas
│   └── fixtures/           # a real-world-shaped CSV the loader is run against
├── output/
│   ├── track_record.csv    # timestamped predictions + results (auditable log)
│   └── figures/            # backtest charts
└── .gitignore
```

### Tests

```r
Rscript tests/run_tests.R
```

177 checks over the arithmetic that would be invisible if it were wrong: odds
conversion, settlement sign conventions, CLV signs, the no-look-ahead property
of the rolling features, column detection against real-world header shapes,
best-price selection across books, magnitude-plus-favourite spread rebuilding,
and the append-only guarantees of the track record. They run in about a second and
need no data. Run them before trusting a number.

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

### Phase 5 — Iterate (human-in-the-loop)  → `R/08_diagnostics.R`
- Analyze where the model fails; form a hypothesis (a factor to add)
- Add it, re-test honestly; resist overfitting (more features ≠ more edge)
- Segment bias, model-vs-market skill, and ROI-by-segment with the
  multiple-comparison count stated every time

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

Then provenance columns, so a number can always be traced back to how it was
made: `market`, `edge`, `model_version`, `event_id`, `line_close`, `clv_points`,
and the pair that separates the model from the human —

- `prediction_raw` — the model's number **before** any hand-entered news read
- `news_adj` — how far your read moved it

Keeping both is what lets `evaluate_news_contribution()` grade your reads apart
from the model. Without it, "did my injury judgement help?" is unanswerable.

Two rules make the file an audit trail rather than a spreadsheet, both enforced
in `R/track_record.R`: predictions are only ever **appended** (re-logging the
same game/market/model is dropped, so the first timestamp stands), and results
are only ever written into **blank** cells.

`clv_points` is recorded in each market's own unit — points for totals and
spreads, probability for the moneyline. Anything averaging it must group by
market.

---

## Markets

Three, all bet flat and all priced with the real vig:

| market | the model's view | selected when | CLV measurable? |
|---|---|---|---|
| **total** | predicted total points | disagrees with the line by ≥ 3 pts | yes (bet the open, graded vs the close) |
| **spread** | predicted margin | disagrees by ≥ 2 pts | yes |
| **moneyline** | win probability | expected value ≥ +2% after guards | only forward (datasets carry no opening moneyline) |

The moneyline deserves a warning, because it is the market that most readily
manufactures a fake edge. Expected value is `decimal_odds` times as sensitive to
an error in probability as it is to the size of the edge, so on a +1500 underdog
a three-point probability error reads as a forty-five-point edge. A linear
model's probabilities are also *compressed* relative to the market's, so it
judges the underdog underpriced in every heavy-favourite game. Put those two
together with a naive EV rule and it will bet every longshot on the board and
report a 30% expected return.

Three guards stop it, and all three are sized from measurements that
`08_diagnostics.R` prints for your own data rather than from taste:

1. **Shrink toward the market** — believe only part of the disagreement, because
   the market's de-vigged probabilities are measurably the more accurate of the
   two estimates.
2. **Haircut** — require the bet to survive the model's typical calibration error.
3. **Price cap** — *derived*, not chosen: `max_ev_error / prob_error`, which at
   the current settings is +233. Past it, the EV estimate mostly restates the
   model's own error bar.

On the synthetic data these turn a headline "+10% ROI" into an honest −13%,
which is the correct answer there — that market is generated from the true
probability, so the profit was leverage on model error and nothing else.

## Evaluation metrics

- **ROI** (incl. vig)
- **Win rate** vs. break-even (~52.4% at -110)
- **Closing-Line Value (CLV)** — did we beat the closing line? (key edge signal)
- **Sample size + confidence interval** — is the result meaningful or noise?
- **Calibration** — when the model says 60%, does it happen ~60% of the time?
- **Placebo control** — a random bettor on the same games at the same prices,
  flipping side *and* price together, so the comparison is to a bet someone
  could actually have placed.

---

## What this project is / isn't

- **IS:** a rigorous data pipeline + predictive model + honest market-efficiency
  evaluation; a genuine quant/data-science skill demonstration.
- **IS NOT:** a guaranteed money-maker or an autonomous self-learning agent. If there's
  no edge, "no edge" is the honest, valuable finding.

---

## Getting started

See **instructions.md** for a click-by-click beginner setup guide (install R, load your
first dataset, and explore it). Built in R (tidyverse, hoopR, ggplot2, lm/glm).
Understanding each step matters more than speed.

### Running the backtest

```r
source("run_all.R")     # 01 → 04: load, features, model, backtest
```

`run_all.R` looks for a CSV in `data/`. If there isn't one, it generates a synthetic
season set (`R/99_make_sample_data.R`) so the whole pipeline runs before you download
anything. That synthetic market is efficient *by construction* — the correct result on
it is **no edge**, which is exactly what it is there to demonstrate.

When you drop in a real Kaggle CSV, `01_load_data.R` infers which of its columns hold
the date, teams, scores, spread, total and prices, then prints the mapping it chose.
If a guess is wrong, correct it in `R/config.R`:

```r
column_overrides = list(spread_close = "home_line_close", total_close = "ou")
```

It also checks the spread's sign against actual results and flips it if the file quotes
the away side — the kind of silent error that otherwise invalidates a whole backtest.

A nastier variant gets first-class handling: many public files store the spread as
a positive **magnitude** and name the favourite in a separate column
(`whos_favored`). Read naively, every away-favourite game comes out inverted, and
a blanket sign flip cannot repair it — on a real 24,000-game file the raw
correlation with margin is +0.21, flipping the column moves it to −0.21, which is
wrong but close enough to the expected −0.45 to pass unchallenged. The loader
detects the favourite column, rebuilds a signed home-side line from it, and
**refuses to continue** if it finds a single-signed spread it cannot resolve.

### Forward-testing

```r
source("R/05_forward.R")
log_predictions(dry_run = TRUE)   # preview tonight's bets, write nothing
log_predictions()                 # append timestamped predictions
record_closing_lines()            # just before tip-off, for CLV
update_results()                  # next morning: grade and report
```

Needs a free key from [the-odds-api.com](https://the-odds-api.com) in `.env`:

```
ODDS_API_KEY=your_key_here
```

`log_predictions()` only ever appends, never rewrites a logged prediction, and
`update_results()` only fills blank cells. That is what makes the record auditable.

**Fair value and execution price are separate things.** The median across books
is the best estimate of what a game is worth, and it is what a bet is *selected*
against — disagreeing with the middle of the market is the honest test. But you
cannot bet the median. Each bet is *executed and logged* at the best number and
price actually on offer, with the book recorded in `notes`. Collapsing the two,
as this code originally did, understates every result by the spread between the
middle of the market and its best shop.

It logs all three markets, and it applies your news reads from
`data/manual_news.csv` on the way through (`use_news = FALSE` to turn that off
for a run). The adjustment moves the expected margin *and* the win probability
together — via a probit shift on the margin model's own residual spread — so the
spread bet and the moneyline bet on the same game never act on contradictory
views of who is going to win. The raw model number is kept in `prediction_raw`
either way.

Moneyline bets are chosen by the same `utils.R::moneyline_pick()` the backtest
was measured on. A forward test that selects differently is not a test of the
backtest.

Results come from `hoopR` by default; `update_results(results_source = "local")` grades
against `data/processed/games.rds` instead, which is what you want when the predictions
were logged against synthetic data.

### News and availability signals

```r
source("R/06_news_signals.R")
injuries_to_news()      # draft rows for anyone not listed Active
                        # then open data/manual_news.csv and set the impacts
```

hoopR has no injury endpoint, so this reads ESPN's per-team roster status. Automation
finds *who*; you decide *how much*, because a points estimate is a judgement call.
Adjustments are forward-test only and are kept in separate columns from the raw model
number, so `evaluate_news_contribution()` can later tell you whether your reads helped.

Reading the news as text:

```r
news <- news_before(fetch_news(50))   # look-ahead gate: published < now
extract_player_status(news)           # player, status, confidence, source sentence
```

ESPN's news API tags each article with athlete IDs that match the box scores, so entity
resolution is exact and no page scraping is involved. The NLP job is then narrowed to
classifying *status* from the sentence containing the mention — ordered pattern rules,
negation- and hedge-aware, and every extraction keeps the sentence it came from.

`extract_player_status()` also takes an optional `gazetteer` from
`07_usage_model.R::player_gazetteer()`, which lets it catch players ESPN did not
tag in a given article.

Did the reads pay?

```r
evaluate_news_contribution()
```

This now asks the sharp version of the question. Comparing adjusted bets against
un-adjusted ones confounds your judgement with whatever made those games
newsworthy in the first place; comparing `prediction_raw` against `prediction` on
the *same* bet does not. If the adjustments are making predictions worse on
average, it says so plainly — that is the finding, and it is the reason the two
columns are stored separately.

### Usage model: who benefits when a player sits

```r
source("R/07_usage_model.R")
pg <- load_player_games()
validate_usage_model(pg)              # do this FIRST
tr <- estimate_usage_transfer(pg)
project_props(absent_ids, tr, player_baselines(pg))
```

Or the whole chain — news to absences to projections to the log — in two calls:

```r
proj <- project_from_news(pg)   # reads the news, resolves who is out, projects
log_props(proj)                 # append to the same auditable track record
```

`project_from_news()` enforces the look-ahead gate on the news timestamps and is
meant for live use. Projections logged without a `line` are exactly that —
timestamped projections, not bets. They are kept on the record so their accuracy
can be graded later, and excluded from ROI because there was no price. Pass your
book's numbers as `log_props(proj, lines = ...)` to turn them into graded bets.

"Who's out" is an NLP question; "who benefits, and by how much" is not — it's measurable.
For every teammate pair, this compares production in games the player missed against
games he played, shrunk toward zero for small samples, and restricted to each player's
actual tenure window with that team (without which a February trade reads as being
"absent" all autumn, and the estimates come out backwards).

`validate_usage_model()` tests it out-of-sample with bootstrap CIs on the RMSE gain,
then **re-tests within each season** and reports whether the pooled result replicates.
That second step matters more than the first.

The honest current result, on seasons 2025 and 2026:

| stat | 2024-25 | 2025-26 |
|---|---|---|
| minutes | routed **+2.29%**, interval clear of zero | −1.07% |
| points | routed **+1.09%**, interval clear of zero | +0.23%, not significant |
| assists | −0.47% | −0.12% |
| rebounds | +0.47% | −0.71% |

**The 2024-25 finding did not replicate.** On 2025-26 alone the function's verdict is
"Neither scheme reliably beats the plain baseline. Do not bet this." The pooled
two-season run still reports a win on points (+0.43%), but it is carried entirely by
the older season — which is exactly why the replication check exists.

Read that as: an effect worth +2% one season and −1% the next, on ~4,500 observations
each, was probably never there. Adding each stat's own delta directly (`method =
"direct"`) is *reliably worse than doing nothing* in both seasons, so `"routed"`
remains the default of the two — but the current evidence says the honest choice is to
apply no adjustment at all until it replicates.

Re-run it on your own data before trusting any of this, and note it measures accuracy,
not edge: the free odds tier carries no prop lines to beat.

### Diagnostics: where does it fail?

The headline test is **forecast encompassing** — regressing the actual result on
the closing line *and* the model together:

```
actual ~ market_prediction + model_prediction
```

RMSE asks "is our number closer?", and the answer is no on both targets. That is
the wrong question to stop on. A forecast can be worse overall and still be
useful if it errs in different places than the market; it can also be nearly as
accurate and still be worthless, because it is only re-deriving the line. The
encompassing coefficient separates the two. If the market encompasses the model,
that coefficient collapses to zero.

Two caveats travel with it. Independent information is *necessary* for an edge,
not sufficient — it still has to beat the vig, which `04_backtest.R` decides. And
the two predictors are heavily collinear by construction, so a small coefficient
with a wide interval is inconclusive rather than evidence of absence.

Runs as part of `run_all.R`, or on its own:

```r
source("R/08_diagnostics.R")
```

This is Phase 5 — it produces *hypotheses*, and changes nothing. Slice a
backtest twenty ways and two slices look profitable at the 5% level even if the
model is worthless; that is what "5% level" means. Three defences:

1. **Lead with bias, not ROI.** Whether the market's number is systematically
   wrong in a segment is a far more stable question than whether we happened to
   profit there — bias is measured on every game in the segment, ROI only on the
   few we chose to bet, with a coin flip on top.
2. **Test the market, not ourselves.** "Our model is biased here" is a bug
   report. "The *market* is biased here" is the only kind of finding that can
   become an edge, and it is much rarer.
3. **Count the comparisons out loud.** Every table reports how many tests it ran,
   how many hits chance alone predicts, and Holm-adjusted p-values next to the
   raw ones.

On the sample data nothing survives correction — the correct result against a
market that is efficient by construction. What it does surface is *our own*
bias: the totals model is off by −7.0 in the lowest-total band and +1.8 in the
high band. That is the same under-dispersion that makes the raw moneyline rule
bet longshots, showing up in a second place.

The calibration table it prints is also where `ml_prob_error` and
`ml_prob_range` in `config.R` come from. If that table changes on your data,
those settings must change with it, and 08 will tell you so.

### Configuration

Every threshold, path and modelling choice lives in `R/config.R` — including
`use_market_features`, which decides whether the model may see the *opening* line.
The closing line is never an input anywhere; it is evaluation only.

The moneyline guards (`ml_shrink_to_market`, `ml_prob_error`, `ml_max_ev_error`,
`ml_prob_range`) are the one group that should **not** be treated as taste. Each
is documented in place with the measurement it came from, and
`08_diagnostics.R` re-derives those measurements on whatever data you point it
at. Change them when the evidence changes, not when the ROI disappoints.

`news$max_abs_impact` caps a hand-entered read; anything past it is clamped with
a warning, on the theory that if you are writing −9 you are guessing.
