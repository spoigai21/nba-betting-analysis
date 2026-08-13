# Runbook — the in-season forward test

Everything before this file was a backtest, and a backtest can be re-run until
it looks good. This is the half that cannot: predictions are written to disk,
timestamped, before tip-off, and whatever happens next goes in the same file.

The whole point is that the record is honest. That makes the operational
discipline part of the method, not overhead around it.

---

## When to start

**Not on opening night.** Two things have to be true first, and both fail
quietly if you ignore them, which is why `log_predictions()` refuses rather
than warns.

| | why |
|---|---|
| `games.rds` holds the **current** season | `latest_team_state()` takes the newest season in the file and calls it "now". Run it on opening night against a stale file and it silently uses *last* season's form — 88 games per team, rosters that have since changed — and logs bets against it. |
| Every team has **≥ 10 games** | `02_features.R` drops any game where either team is below that, on the grounds its form numbers mean nothing yet. Betting them live would forward-test a strategy the backtest never measured. |

At roughly 3.5 games a week, ten games is **about three weeks** — so realistically
**mid-November**, not late October. Run the morning step daily from opening
night anyway: that is what accumulates the results the gate is waiting on.
Individual games below the floor are dropped automatically in the meantime, so
you can also just let the gate open on its own.

---

## The daily loop

```sh
Rscript daily.R morning    # after last night's games have finished
Rscript daily.R log        # a few hours before tip-off
Rscript daily.R close      # ~15 minutes before tip-off
```

Order is load-bearing:

1. **morning** — pulls last night's results into `games.rds`, rebuilds team
   form on them, grades any bets those results settle, prints the running
   record, and reports whether pre-flight passes. Do this *before* predicting,
   or tonight's numbers are built on stale form.
2. **log** — dry-runs, then appends timestamped predictions. Runs its own
   pre-flight and refuses if it fails.
3. **close** — captures the closing number for CLV. **This one is unrecoverable
   if missed.** `record_closing_lines()` only fills rows dated today or later,
   so a skipped night is a permanent hole in the CLV column, and CLV is the
   metric that tells you something long before ROI does.

---

## Reading the output

| you see | it means |
|---|---|
| `model agrees with the market on every game today` | Normal and healthy. A model with an opinion on every game has no opinion at all. |
| `skipping N game(s) where a team has fewer than 10 games` | Early season. Expected. It shrinks to zero on its own. |
| `N prediction(s) already logged earlier; keeping the original` | The append guard working. The first timestamp is the honest one. |
| `Pre-flight failed -- nothing logged` | Read every line before doing anything. See below. |
| `no pending prediction matched a finished game` | Usually a name or date mismatch, or the games have not finished. |

---

## When pre-flight refuses

| message | what to do |
|---|---|
| `results file holds season X but today is season Y` | `Rscript daily.R morning`. If hoopR has nothing yet, the season has not started. |
| `most recent result is N days old` | Same. If it persists, hoopR is failing — check it directly before overriding. |
| `N team(s) have fewer than 10 games` | Wait. This is the gate doing its job. |
| `no ODDS_API_KEY in .env` | Add it. Free tier at the-odds-api.com. |

`force = TRUE` exists on `log_predictions()`. It is there for the case where
you have read every item and still mean it — not as a way past a red light. A
row logged under conditions the backtest never measured is worse than no row,
because it dilutes the record you are keeping precisely to be trustworthy.

---

## Budget

The Odds API free tier is **500 requests/month**. The loop uses two a day —
one to log, one to capture the close — so about **60 a month**. Ample. If you
add intraday polling, count it: running out mid-month costs you the closing
lines, which is the expensive half.

Gemini's free tier is 1,500 requests/day against a need of ~50, and every
extraction is cached, so `09` costs nothing ongoing.

---

## What to check weekly

```r
source("R/05_forward.R")
report_track_record()                 # units, ROI, CLV by market
evaluate_news_contribution()          # did your reads help, on the same bets
```

Watch **CLV before ROI**. It is measured against a number rather than a coin
flip, so it separates signal from luck in a fraction of the sample. Consistently
beating the close is the earliest honest evidence of an edge; ROI over a few
dozen bets is noise wearing a percentage sign.

---

## When any of it means anything

**~100 settled bets.** At the rates the backtest selected, that is roughly
**mid-January**. Before then the record is a check that the machinery works,
not evidence about the strategy.

Two things not to do while waiting:

- **Do not change the rules mid-stream.** Thresholds, features, model version —
  changing any of them restarts the sample. If you must, bump `MODEL_VERSION`
  so the record stays interpretable across the break; the report already
  segments by it.
- **Do not read the running ROI as feedback.** It will swing wildly and it will
  be tempting. That is exactly what a large-sample requirement is for.

The honest prior, earned across this project: the backtest returned **−4.87%
[−6.1%, −3.6%]** on 24,195 bets, indistinguishable from betting at random and
paying the vig, and the encompassing test showed the model carries no
information the closing line lacks. The forward test's most likely outcome is
to confirm that. **"No edge" is the finding, not the failure** — and the news
and availability signals in `06`/`09`, which cannot be backtested at all, are
the one part of this that has never been measured.
