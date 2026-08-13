# ---------------------------------------------------------------------------
# tests/run_tests.R -- the arithmetic this project cannot afford to get wrong
# ---------------------------------------------------------------------------
# Run:  Rscript tests/run_tests.R          (from the project root)
#
# No testing framework on purpose. These tests are part of the argument that
# the results are trustworthy, so they should be readable by someone who has
# never used testthat -- and the project should not grow a dependency to check
# whether -110 pays 0.909.
#
# WHAT IS TESTED, AND WHY THESE THINGS
#   Every number this project reports passes through four places where a silent
#   error would not look like an error:
#     * odds maths      -- a wrong decimal conversion misprices every bet
#     * settlement      -- a flipped sign turns losses into wins
#     * CLV signs       -- likewise, and CLV is the headline edge claim
#     * no-look-ahead   -- a rolling mean that includes the current game
#                          manufactures a 60% win rate out of nothing
#   Plus column detection, which is the one piece that meets a file this
#   project has never seen.
# ---------------------------------------------------------------------------

suppressMessages(suppressWarnings(source("R/00_setup.R")))
# Import the column-detection and parsing functions WITHOUT loading any data.
options(nba.schema_only = TRUE)
suppressMessages(suppressWarnings(source("R/01_load_data.R")))

.n_pass <- 0L; .n_fail <- 0L; .failures <- character(); .current <- ""

section <- function(x) { .current <<- x; cat("\n", x, "\n", sep = "") }

ok <- function(cond, what) {
  passed <- isTRUE(all(cond)) && !any(is.na(cond))
  if (passed) { .n_pass <<- .n_pass + 1L; cat("  ok   ", what, "\n", sep = "") }
  else {
    .n_fail <<- .n_fail + 1L
    .failures <<- c(.failures, paste0(.current, ": ", what))
    cat("  FAIL ", what, "\n", sep = "")
  }
}

eq <- function(actual, expected, what, tol = 1e-8) {
  same <- length(actual) == length(expected) &&
    all((is.na(actual) & is.na(expected)) |
        (!is.na(actual) & !is.na(expected) &
           (if (is.numeric(actual) && is.numeric(expected))
              abs(actual - expected) < tol else actual == expected)))
  if (!isTRUE(same))
    cat("       expected: ", paste(format(expected), collapse = ", "),
        "\n       actual:   ", paste(format(actual), collapse = ", "), "\n", sep = "")
  ok(same, what)
}

# ===========================================================================
section("Odds maths")
# ===========================================================================

eq(american_to_decimal(-110), 1 + 100 / 110, "-110 is 1.909 in decimal")
eq(american_to_decimal(150), 2.5, "+150 is 2.5 in decimal")
eq(american_to_decimal(100), 2.0, "+100 is an even-money 2.0")
eq(american_to_prob(-110), 110 / 210, "-110 implies 52.38% including vig")
eq(american_to_prob(150), 0.4, "+150 implies 40%")
eq(break_even_prob(-110), 0.523809523, "break-even at -110 is 52.38%", tol = 1e-6)

eq(prob_to_american(0.5), -100, "an even chance prices at -100")
p <- c(0.2, 0.35, 0.5, 0.62, 0.9)
eq(american_to_prob(prob_to_american(p)), p, "prob -> american -> prob round-trips")
d <- c(1.2, 1.5, 2.0, 3.4, 11)
eq(american_to_decimal(decimal_to_american(d)), d, "decimal -> american -> decimal round-trips")

dv <- devig_two_way(-110, -110)
eq(dv$a + dv$b, 1, "de-vigged probabilities sum to exactly 1")
eq(dv$a, 0.5, "a symmetric market de-vigs to a coin flip")
eq(dv$hold, 210 / 210 * (110 / 210 * 2) - 1, "hold at -110/-110 is 4.76%", tol = 1e-6)
dv2 <- devig_two_way(-200, 170)
ok(dv2$a > dv2$b, "the favourite keeps the larger de-vigged probability")
eq(dv2$a + dv2$b, 1, "asymmetric market also de-vigs to 1")

eq(bet_units(-110, "win"),  100 / 110, "a -110 winner returns 0.909 units")
eq(bet_units(-110, "loss"), -1,        "a loser costs exactly the stake")
eq(bet_units(-110, "push"),  0,        "a push returns the stake")
eq(bet_units(150, "win"),    1.5,      "a +150 winner returns 1.5 units")
eq(bet_units(-110, NA_character_), NA_real_, "an ungraded bet has no units")
eq(bet_units(-110, "pending"), NA_real_, "a pending bet has no units")

eq(bet_ev(0.5, 100), 0, "an even bet at +100 has zero EV")
eq(bet_ev(110 / 210, -110), 0, "betting at exactly the break-even price is zero EV")
ok(bet_ev(0.6, -110) > 0, "a 60% shot at -110 is +EV")
ok(bet_ev(0.4, -110) < 0, "a 40% shot at -110 is -EV")

# The bug this function exists to prevent: the American scale has a hole
# between -100 and +100, so a plain median of two books lands in the hole.
eq(median_american(c(-110, -110)), -110, "median of identical prices is that price")
ok(abs(median_american(c(-105, 105))) > 99,
   "median of -105 and +105 is near even money, NOT the plain median of 0")
eq(median_american(c(NA, -110)), -110, "missing prices are ignored")
eq(median_american(c(NA_real_, NA_real_)), NA_real_, "all-missing yields NA")

# ===========================================================================
section("Settlement -- sign conventions")
# ===========================================================================

eq(settle_total(220, 218, "over"),  "win",  "over wins when the game goes higher")
eq(settle_total(210, 218, "over"),  "loss", "over loses when the game goes lower")
eq(settle_total(210, 218, "under"), "win",  "under wins when the game goes lower")
eq(settle_total(218, 218, "over"),  "push", "landing on the number is a push")
eq(settle_total(NA, 218, "over"), NA_character_, "an unplayed game does not settle")

# spread_close is quoted from the home side: -6 means home must win by 7+.
eq(settle_spread(8,  -6, "home"), "win",  "home -6 covers when it wins by 8")
eq(settle_spread(4,  -6, "home"), "loss", "home -6 fails when it wins by only 4")
eq(settle_spread(6,  -6, "home"), "push", "winning by exactly the spread pushes")
eq(settle_spread(4,  -6, "away"), "win",  "the away side wins that same game")
eq(settle_spread(-3,  2, "home"), "loss", "home +2 losing by 3 does not cover")
eq(settle_spread(-1,  2, "home"), "win",  "home +2 losing by 1 does cover")

eq(settle_moneyline(5,  "home"), "win",  "home moneyline wins on a positive margin")
eq(settle_moneyline(-5, "home"), "loss", "home moneyline loses on a negative margin")
eq(settle_moneyline(-5, "away"), "win",  "away moneyline wins on a negative margin")
eq(settle_moneyline(0,  "home"), NA_character_,
   "a zero margin is impossible in the NBA and must not silently settle")

# Vectorised, because that is how they are actually called.
eq(settle_total(c(220, 210, 218), 218, c("over", "over", "under")),
   c("win", "loss", "push"), "settle_total vectorises over value and side")

# ===========================================================================
section("Closing-line value signs")
# ===========================================================================
# Positive CLV must always mean "we took the better number". A flipped sign
# here would invert the project's headline edge claim with no other symptom.

eq(clv_of("total", "over", 218, 220), 2, "over 218 into a 220 close is +2 CLV")
eq(clv_of("total", "over", 218, 216), -2, "over 218 into a 216 close is -2 CLV")
eq(clv_of("total", "under", 218, 216), 2, "under 218 into a 216 close is +2 CLV")
eq(clv_of("spread", "home", 6.5, 5), 1.5, "home +6.5 into a +5 close is +1.5 CLV")
eq(clv_of("spread", "home", -6.5, -8), 1.5, "home -6.5 into a -8 close is +1.5 CLV")
eq(clv_of("spread", "away", -6.5, -5), 1.5, "away -6.5 into a -5 close is +1.5 CLV")
eq(clv_of("moneyline", "home", 0.55, 0.60), 0.05,
   "backing home at 55% into a 60% close is +5 points of CLV")
eq(clv_of("moneyline", "away", 0.55, 0.50), 0.05,
   "backing away and watching home drift down is also positive")
eq(clv_of("total", "over", 218, NA), NA_real_, "no close means no CLV, not zero CLV")
eq(clv_of("prop", "over", 20, 21), NA_real_, "unknown markets do not invent a CLV")

# ===========================================================================
section("No look-ahead")
# ===========================================================================
# The single most important property in the project: a feature for game N may
# use games 1..N-1 and nothing else.

eq(roll_mean_prior(c(1, 2, 3, 4)), c(NA, 1, 1.5, 2),
   "expanding mean uses only prior values, and is NA for the first game")
eq(roll_mean_prior(c(1, 2, 3, 4), 2)[4], 2.5,
   "a 2-game window at game 4 averages games 2 and 3")
eq(roll_mean_prior(numeric(0)), numeric(0), "an empty season is handled")
eq(roll_mean_prior(c(5)), NA_real_, "a single game has no prior to average")

# Direct proof of the property: changing the LAST value must not disturb any
# earlier output. If it does, the current game is leaking into its own feature.
x1 <- c(3, 1, 4, 1, 5, 9); x2 <- x1; x2[6] <- 999
eq(roll_mean_prior(x1)[1:6], roll_mean_prior(x2)[1:6],
   "changing game 6's value cannot change any feature up to game 6")
eq(roll_mean_prior(x1, 3)[1:6], roll_mean_prior(x2, 3)[1:6],
   "same holds for the rolling window")

# NAs are skipped rather than treated as zero.
eq(roll_mean_prior(c(2, NA, 4))[3], 2, "a missing game is skipped, not counted as 0")

dts <- as.Date(c("2025-01-01", "2025-01-03", "2025-01-05", "2025-01-20"))
eq(games_in_prior_days(dts, 7), c(0, 1, 2, 0),
   "schedule density counts only games inside the window, never the current one")

eq(season_of(as.Date("2025-01-15")), 2025, "January 2025 is the 2025 season")
eq(season_of(as.Date("2024-10-20")), 2025, "October 2024 is also the 2025 season")
eq(season_of(as.Date("2024-07-31")), 2024, "July stays with the season just ended")
eq(season_of(as.Date("2024-08-01")), 2025, "August starts the new season")

# ===========================================================================
section("Team names")
# ===========================================================================

eq(canonical_team("Los Angeles Lakers"), "LAL", "full name maps")
eq(canonical_team("LA Lakers"), "LAL", "common short form maps")
eq(canonical_team("lakers"), "LAL", "bare nickname maps, case-insensitively")
eq(canonical_team("LAL"), "LAL", "an existing code passes through")
eq(canonical_team("Seattle SuperSonics"), "OKC", "relocated franchises map to the current code")
eq(canonical_team("New Jersey Nets"), "BKN", "so history stays continuous")
eq(canonical_team("TEAM CHUCK"), "TEAM CHUCK",
   "an unknown name is passed through, not silently dropped")
eq(canonical_team(c("Boston Celtics", "heat")), c("BOS", "MIA"), "vectorises")

eq(norm_key("Home Team (Close)"), "home_team_close", "header normalisation")
eq(names(clean_names(tibble(`A B` = 1, `A-B` = 2))), c("a_b", "a_b_1"),
   "colliding cleaned names are made unique")

eq(round_half(220.3), 220.5, "lines snap to the half-point grid")
eq(round_half(220.1), 220.0, "and round to the nearest half")

# ===========================================================================
section("Statistics helpers")
# ===========================================================================

ci <- wilson_ci(50, 100)
ok(ci[["lower"]] < 0.5 && ci[["upper"]] > 0.5, "Wilson interval brackets the estimate")
ok(wilson_ci(0, 10)[["lower"]] >= 0, "Wilson interval cannot go below zero")
ok(wilson_ci(10, 10)[["upper"]] <= 1, "Wilson interval cannot exceed one")
ok(diff(wilson_ci(500, 1000)) < diff(wilson_ci(50, 100)),
   "a larger sample gives a tighter interval")
eq(unname(wilson_ci(0, 0)), c(NA_real_, NA_real_), "no bets means no interval")

eq(rmse(c(1, 2, 3), c(1, 2, 3)), 0, "a perfect prediction has zero RMSE")
eq(mae(c(1, 2), c(2, 4)), 1.5, "mean absolute error")

# ===========================================================================
section("Betting-file discovery")
# ===========================================================================

tmp <- file.path(tempdir(), "datadir"); dir.create(tmp, showWarnings = FALSE)
writeLines(strrep("x", 500), file.path(tmp, "big_dataset.csv"))
writeLines("a", file.path(tmp, "manual_news.csv"))
found <- betting_csvs(tmp)
ok(length(found) == 1 && basename(found) == "big_dataset.csv",
   "manual_news.csv is never mistaken for the raw dataset")
unlink(tmp, recursive = TRUE)

# ===========================================================================
section("Moneyline selection guards")
# ===========================================================================
# These guards are the reason the moneyline market reports no edge instead of a
# fabricated one. Each test pins one of them.

# A model that agrees with the market has no bet to make.
fair_p <- devig_two_way(-150, 130)$a
pick_agree <- moneyline_pick(fair_p, -150, 130)
eq(pick_agree$side, NA_character_, "agreeing with the market produces no bet")

# The headline failure mode: a big edge on a huge underdog must be REFUSED,
# because EV there is dominated by the model's own probability error.
pick_dog <- moneyline_pick(0.70, -2000, 1500)
eq(pick_dog$side, NA_character_,
   "a 30%-model underdog at +1500 is declined -- the price cap bites")

# A LARGE disagreement at a sane price is allowed through. Note how big it has
# to be: the market here implies 52%, and a 62% model view is still declined
# once shrink and haircut are applied. Only a genuinely wide gap survives.
ok(is.na(moneyline_pick(0.62, -120, 100)$side),
   "a 10-point edge at a normal price is still declined after shrink and haircut")
pick_ok <- moneyline_pick(0.70, -120, 100)
ok(!is.na(pick_ok$side), "a 18-point edge at a normal price is allowed through")

# Out-of-band probabilities are refused however attractive the price looks.
pick_band <- moneyline_pick(0.97, -110, -110)
eq(pick_band$side, NA_character_,
   "a 97% model probability is outside the calibrated band and is declined")

# Shrinkage must reduce the claimed edge relative to the raw disagreement.
raw_gap <- 0.70 - devig_two_way(-120, 100)$a
ok(pick_ok$ev < raw_gap * american_to_decimal(-120),
   "shrink and haircut make the acted-on EV smaller than the raw disagreement")
ok(pick_ok$price == -120 && pick_ok$price_alt == 100,
   "the chosen side's price and its opposite are both recorded")

# Missing prices must not produce a bet.
eq(moneyline_pick(0.62, NA, NA)$side, NA_character_, "no price means no bet")

# ===========================================================================
section("Line shopping -- fair value vs execution price")
# ===========================================================================
# The market's median is the right estimate of fair value and the wrong thing
# to log as the price paid: you cannot bet the median. These pin the split.

suppressMessages(suppressWarnings(source("R/05_forward.R")))

mkq <- function(book, market, name, raw, price, point) tibble(
  event_id = "ev1", commence = as.POSIXct("2026-11-01 23:00:00", tz = "UTC"),
  home_team = "BOS", away_team = "LAL", book = book, market = market,
  name = name, raw_name = raw, price = price, point = point)

od_fix <- bind_rows(
  mkq("A","spreads","BOS","BOS",-110,-4.5), mkq("A","spreads","LAL","LAL",-110, 4.5),
  mkq("B","spreads","BOS","BOS",-115,-4.0), mkq("B","spreads","LAL","LAL",-105, 4.0),
  mkq("C","spreads","BOS","BOS",-105,-4.5), mkq("C","spreads","LAL","LAL",-115, 4.5),
  mkq("A","totals","Over","Over",-110,224.5), mkq("A","totals","Under","Under",-110,224.5),
  mkq("B","totals","Over","Over",-108,225.0), mkq("B","totals","Under","Under",-112,225.0),
  mkq("C","totals","Over","Over",-120,224.5), mkq("C","totals","Under","Under", 100,224.5),
  mkq("A","h2h","BOS","BOS",-190,NA), mkq("A","h2h","LAL","LAL", 160,NA),
  mkq("B","h2h","BOS","BOS",-175,NA), mkq("B","h2h","LAL","LAL", 155,NA),
  mkq("C","h2h","BOS","BOS",-200,NA), mkq("C","h2h","LAL","LAL", 172,NA))

cl <- suppressMessages(consensus_lines(od_fix))

eq(cl$spread_current, -4.5, "consensus spread is the median across books")
eq(cl$total_current, 224.5, "consensus total is the median across books")

# Best NUMBER first. A home bet wants the most points it can get.
eq(cl$best_spread_home, -4.0, "home spread shops to the best number (-4.0, not -4.5)")
eq(cl$best_book_spread_home, "B", "and names the book offering it")
eq(cl$best_spread_away, 4.5, "away spread shops to the best number (+4.5)")
# Both A and C offer +4.5; A is -110 and C is -115, so price breaks the tie.
eq(cl$best_book_spread_away, "A", "price breaks a tie on equal numbers (-110 beats -115)")

# Totals run in opposite directions by side.
eq(cl$best_total_over, 224.5, "an over wants the LOWEST total on the board")
eq(cl$best_book_over, "A", "and the best price among books at that total")
eq(cl$best_total_under, 225.0, "an under wants the HIGHEST total on the board")

# Moneyline has no number, only a price.
eq(cl$best_ml_home, -175, "moneyline shops to the best home price")
eq(cl$best_ml_away, 172, "moneyline shops to the best away price")
ok(american_to_decimal(cl$best_ml_home) > american_to_decimal(cl$ml_home),
   "the shopped price is strictly better than the consensus price")

# A single-book feed must still work, with best == consensus.
one <- suppressMessages(consensus_lines(od_fix %>% filter(.data$book == "A")))
eq(one$best_spread_home, one$spread_current, "one book: best number equals consensus")
eq(one$best_ml_home, one$ml_home, "one book: best price equals consensus")

# ===========================================================================
section("Season selection during the offseason")
# ===========================================================================
# Between June and October, season_of() already names the NEXT season, which
# has not played a game. Asking hoopR for it returns nothing.

suppressMessages(suppressWarnings(source("R/07_usage_model.R")))

eq(default_seasons(as.Date("2026-08-09")), c(2025L, 2026L),
   "in August, the two COMPLETED seasons are returned")
eq(default_seasons(as.Date("2026-11-01")), c(2026L, 2027L),
   "in November, the season under way is included")
eq(default_seasons(as.Date("2026-10-14")), c(2025L, 2026L),
   "just before tip-off the unstarted season is still excluded")
eq(default_seasons(as.Date("2027-03-01")), c(2026L, 2027L),
   "mid-season returns the current season and the one before it")

# ===========================================================================
section("Track record -- append-only guarantees")
# ===========================================================================

tp <- file.path(tempdir(), "tr_test.csv"); unlink(tp)
row1 <- tibble(date = as.Date("2026-01-01"), game = "BOS @ LAL", stat = "game_total",
               prediction = 220, line = 218, odds = -110, bet = "over",
               timestamp = "t1", market = "total", model_version = "lm-v1")

a <- append_track_rows(row1, path = tp, quiet = TRUE)
eq(nrow(a), 1L, "a new prediction is appended")
eq(ncol(read_track_record(tp)), length(TRACK_COLS), "the file carries the full schema")

b <- append_track_rows(row1, path = tp, quiet = TRUE)
eq(nrow(b), 0L, "logging the same prediction twice appends nothing")
eq(nrow(read_track_record(tp)), 1L, "and the file still holds exactly one row")
eq(read_track_record(tp)$timestamp, "t1", "the ORIGINAL timestamp survives")

# A re-log with a different prediction must not overwrite the logged one.
row1b <- row1; row1b$prediction <- 999; row1b$timestamp <- "t2"
invisible(append_track_rows(row1b, path = tp, quiet = TRUE))
eq(read_track_record(tp)$prediction, 220, "a restated prediction cannot overwrite the original")

# Same game, different market, is a genuinely different prediction.
row2 <- row1; row2$market <- "spread"; row2$stat <- "home_margin"
invisible(append_track_rows(row2, path = tp, quiet = TRUE))
eq(nrow(read_track_record(tp)), 2L, "a different market on the same game is a new row")

# Props key on player, so two players' points props do not collide.
pr <- tibble(date = as.Date("2026-01-01"), game = "BOS", stat = "points",
             player = c("A", "B"), prediction = c(20, 30), timestamp = "t3",
             market = "prop", model_version = "usage-v1")
invisible(append_track_rows(pr, path = tp, quiet = TRUE))
eq(nrow(read_track_record(tp)), 4L, "two players' props are two distinct rows")

# Dry run must not touch the file.
n_before <- nrow(read_track_record(tp))
row3 <- row1; row3$game <- "NYK @ MIA"
invisible(append_track_rows(row3, path = tp, dry_run = TRUE, quiet = TRUE))
eq(nrow(read_track_record(tp)), n_before, "a dry run writes nothing")

# Callers may supply a subset of columns; the rest are filled with typed NAs.
conf <- conform_track_rows(tibble(date = as.Date("2026-01-01"), game = "x",
                                  stat = "y", market = "total"))
eq(names(conf), TRACK_COLS, "conform puts columns in canonical order")
ok(is.numeric(conf$prediction) && is.character(conf$notes),
   "filled columns keep their declared types")

# Unpriced projections must not be counted as bets when computing ROI.
s <- summarise_bets_simple(tibble(win_loss = c("win", NA), units = c(0.9, NA),
                                  odds = c(-110, NA)))
eq(s$n_staked, 1L, "a projection with no odds is not a staked bet")
unlink(tp)

# ===========================================================================
section("Column detection against real-world schemas")
# ===========================================================================
# 01_load_data.R's whole job is meeting a file it has never seen. These are the
# header shapes public NBA betting datasets actually ship with.

check_map <- function(headers, expect, label) {
  df <- as_tibble(setNames(
    lapply(headers, function(h) if (grepl("team|date", h)) "x" else 1), headers))
  df <- clean_names(df)
  m <- suppressMessages(detect_columns(df, overrides = list()))
  bad <- character()
  for (fld in names(expect)) {
    got <- m[[fld]]
    want <- norm_key(expect[[fld]])
    if (is.na(got) || got != want)
      bad <- c(bad, sprintf("%s: got %s, want %s", fld, got, want))
  }
  if (length(bad)) cat("       ", paste(bad, collapse = "\n        "), "\n", sep = "")
  ok(length(bad) == 0, label)
}

check_map(
  c("game_date", "home_team", "away_team", "home_score", "away_score",
    "spread_home_close", "total_close", "moneyline_home", "moneyline_away"),
  list(date = "game_date", home_team = "home_team", away_team = "away_team",
       home_score = "home_score", away_score = "away_score",
       spread_close = "spread_home_close", total_close = "total_close",
       ml_home = "moneyline_home", ml_away = "moneyline_away"),
  "tidy snake_case schema")

check_map(
  c("Date", "Home Team", "Visitor Team", "Home Score", "Visitor Score",
    "Home Line Close", "OU Close", "Home Odds Close", "Away Odds Close"),
  list(date = "Date", home_team = "Home Team", away_team = "Visitor Team",
       home_score = "Home Score", away_score = "Visitor Score"),
  "spaced Title Case with Visitor naming")

# The important one: opening columns must not be captured as the close.
m_open <- suppressMessages(detect_columns(clean_names(tibble(
  date = "x", home_team = "x", away_team = "x", home_score = 1, away_score = 1,
  spread_open = 1, spread_close = 1, total_open = 1, total_close = 1)),
  overrides = list()))
ok(m_open[["spread_close"]] == "spread_close" && m_open[["spread_open"]] == "spread_open",
   "opening and closing spreads are told apart")
ok(m_open[["total_close"]] == "total_close" && m_open[["total_open"]] == "total_open",
   "opening and closing totals are told apart")

# Prices must not be mistaken for lines.
m_px <- suppressMessages(detect_columns(clean_names(tibble(
  date = "x", home_team = "x", away_team = "x", home_score = 1, away_score = 1,
  spread = 1, spread_price_home = 1, spread_price_away = 1,
  over_odds = 1, under_odds = 1, total = 1)), overrides = list()))
eq(m_px[["spread_close"]], "spread", "a bare 'spread' column is the closing spread")
eq(m_px[["price_spread_home"]], "spread_price_home", "the home spread price is found")
eq(m_px[["price_over"]], "over_odds", "the over price is found")
ok(m_px[["total_close"]] == "total", "a bare 'total' column is the closing total")

# An explicit override always wins.
m_ovr <- suppressMessages(detect_columns(clean_names(tibble(
  date = "x", home_team = "x", away_team = "x", home_score = 1, away_score = 1,
  weird_line_name = 1, spread = 1)),
  overrides = list(spread_close = "weird_line_name")))
eq(m_ovr[["spread_close"]], "weird_line_name", "a config override beats the guesser")

# ===========================================================================
section("Forward-test pre-flight")
# ===========================================================================
# Both conditions here fail SILENTLY in the live path: a stale results file
# makes last season's form look current, and a thin early season logs bets the
# backtest would have refused to evaluate. Warnings get scrolled past, so these
# are refusals -- and a refusal is only trustworthy if it is also precise.

suppressMessages(suppressWarnings(source("R/05_forward.R")))

.tg <- function(season, n_per_team, last_date) {
  teams <- head(TEAM_ALIASES$code, 30)
  expand.grid(team = teams, i = seq_len(n_per_team), stringsAsFactors = FALSE) |>
    as_tibble() |>
    mutate(season = season, date = as.Date(last_date) - .data$i)
}

.saved_key <- Sys.getenv("ODDS_API_KEY")
Sys.setenv(ODDS_API_KEY = "test-key")

# Healthy: current season, fresh, everyone past the floor.
.ok <- forward_preflight(.tg(2027L, 20L, "2026-12-01"), as_of = as.Date("2026-12-01"))
ok(.ok$ok, "a current, fresh, experienced season passes")

# Stale file: newest season is last season. This is the opening-night trap.
.stale <- forward_preflight(.tg(2026L, 80L, "2026-06-13"), as_of = as.Date("2026-10-21"))
ok(!.stale$ok, "a results file a season behind is refused")
ok(any(grepl("season 2026 but", .stale$issues)),
   "and says which season it has versus which it needs")

# Thin season: right season, current, but teams are 5 games in.
.thin <- forward_preflight(.tg(2027L, 5L, "2026-11-01"), as_of = as.Date("2026-11-01"))
ok(!.thin$ok, "a season where teams are below the games floor is refused")
ok(any(grepl("fewer than", .thin$issues)),
   "and explains that the backtest excludes those games")

# Stale-by-days: right season, enough games, but nothing recent.
.old <- forward_preflight(.tg(2027L, 20L, "2026-11-01"), as_of = as.Date("2026-12-01"))
ok(!.old$ok, "results that stopped updating are refused")
ok(any(grepl("days old", .old$issues)), "and report how far behind they are")

# The key check is independent of the data checks.
Sys.setenv(ODDS_API_KEY = "")
.nokey <- forward_preflight(.tg(2027L, 20L, "2026-12-01"), as_of = as.Date("2026-12-01"))
ok(!.nokey$ok, "a missing odds key is refused")
ok(any(grepl("ODDS_API_KEY", .nokey$issues)), "and named explicitly")
if (nzchar(.saved_key)) Sys.setenv(ODDS_API_KEY = .saved_key) else Sys.unsetenv("ODDS_API_KEY")

# ===========================================================================
section("Scoped absences are rest policies, not tonight's absence")
# ===========================================================================
# The ordered rules match "will not play" at 0.95 before anything looks at what
# qualifies it, so "will not play both ends of back-to-backs" read as OUT --
# treating a healthy star as absent, a multi-point error in the wrong direction
# on a game you would then bet. These pin the fix and, just as importantly, the
# cases that must KEEP reading as a real absence.

suppressMessages(suppressWarnings(source("R/06_news_signals.R")))
.cls <- function(x) classify_sentence(x)$status
.scp <- function(x) classify_sentence(x)$scoped

# Scoped to a recurring situation -> a workload plan.
eq(.cls("Wembanyama will not play both ends of back-to-backs."), "rest",
   "'both ends of back-to-backs' is a rest policy, not an absence")
eq(.cls("He will not play in back-to-backs the rest of the way."), "rest",
   "plural 'back-to-backs' marks a standing arrangement")
eq(.cls("Doncic will miss the second night of back-to-backs."), "rest",
   "'second night of back-to-backs' is a policy")
eq(.cls("Curry will sit one game of every back-to-back."), "rest",
   "'one game of every' is a policy")
ok(.scp("Curry will sit one game of every back-to-back."),
   "and the row is flagged scoped so callers can tell")

# Unscoped absences must be untouched -- this guard must not swallow real news.
eq(.cls("Embiid will not play tonight against Boston."), "out",
   "a plain absence still reads as out")
eq(.cls("Morant is out for the season with a knee injury."), "out",
   "a season-ending injury still reads as out")
eq(.cls("Tatum will miss the next four games."), "out",
   "a fixed number of games still reads as out")

# The two cases the pattern most easily gets wrong.
eq(.cls("Booker misses tonight, the second night of a back-to-back."), "out",
   "a SINGULAR back-to-back is one game, and stays an absence")
eq(.cls("Davis is out for the rest of the season."), "out",
   "'rest of the season' is a duration, not a rest policy")

# news_absences() must hold scoped rows back: they say a player misses SOME
# game, not that he misses THIS one.
.ext <- tibble(athlete_id = c("1", "2"), player = c("Policy Guy", "Real Out"),
               status = c("rest", "out"), confidence = c(0.95, 0.95),
               negated = FALSE, hedged = FALSE, scoped = c(TRUE, FALSE),
               sentence = "s", headline = "h",
               published = Sys.time(), url = "u")
.abs <- suppressMessages(news_absences(.ext, min_confidence = 0.7))
eq(nrow(.abs), 1L, "a scoped rest policy is not counted as tonight's absence")
eq(.abs$player, "Real Out", "while a genuine absence still comes through")

# ===========================================================================
section("LLM strategy extraction -- reproducibility and the look-ahead gate")
# ===========================================================================
# A model call is not reproducible; a committed cache of its answers is. These
# pin the properties that make an LLM admissible in an auditable pipeline at
# all: the gate holds, a re-run makes no calls, a changed prompt re-queries,
# and no failure mode ever invents a signal.

suppressMessages(suppressWarnings(source("R/09_llm_news.R")))
CFG$llm$cache <- file.path(tempdir(), "llm_test_cache.jsonl"); unlink(CFG$llm$cache)

.n_calls <- 0L
.stub <- function(body) {
  .n_calls <<- .n_calls + 1L
  # Read the user text out of whichever wire shape is active.
  usr <- if (identical(llm_provider(), "gemini")) body$contents[[1]]$parts[[1]]$text
         else body$messages[[1]]$content
  txt <- if (grepl("minutes cap", usr, fixed = TRUE))
    paste0('{"signals":[{"player":"A Player","signal_type":"minutes_restriction",',
           '"direction":"decrease","confidence":0.9,"evidence":"quoted sentence",',
           '"attributed_to":"coach"}]}')
  else '{"signals":[]}'
  # Return whichever wire shape the active provider expects.
  if (identical(llm_provider(), "gemini"))
    list(candidates = list(list(finishReason = "STOP",
                                content = list(role = "model",
                                               parts = list(list(text = txt))))))
  else
    list(model = "claude-opus-5", stop_reason = "end_turn",
         content = list(list(type = "text", text = txt)))
}
options(nba.llm_call = .stub)

.news <- tibble(
  article_id = c("t1", "t2"),
  headline = c("minutes cap for A Player", "routine recap"),
  description = c("Coach said A Player is on a minutes cap.", "Team won."),
  published = as.POSIXct(c("2026-11-01 12:00:00", "2026-11-03 12:00:00"), tz = "UTC"),
  url = c("u1", "u2"),
  athlete_ids = list("1", "2"), athlete_names = list("A Player", "B Player"))

# The request must actually carry the guards we think it does.
.gb <- gemini_body(.news[1, ])
eq(.gb$generationConfig$responseMimeType, "application/json",
   "gemini: the request asks for JSON back")
ok(nchar(.gb$systemInstruction$parts[[1]]$text) > 500,
   "gemini: the system instruction carries the binding rules")
ok(grepl("ONLY the article text", .gb$systemInstruction$parts[[1]]$text, fixed = TRUE),
   "the prompt forbids answering from the model's own knowledge")
ok(!inherits(try(jsonlite::toJSON(.gb, auto_unbox = TRUE), silent = TRUE), "try-error"),
   "gemini: the whole request body serialises to valid JSON")

# Gemini takes an OpenAPI subset, not full JSON Schema. Getting this wrong
# strips the constraints silently and leaves a request that validates nothing.
.gs <- gemini_schema(llm_news_schema())
eq(.gs$type, "OBJECT", "gemini schema: types are uppercased")
eq(.gs$properties$signals$type, "ARRAY", "gemini schema: nested types too")
ok(is.null(.gs$additionalProperties), "gemini schema: additionalProperties is dropped")
eq(length(.gs$properties$signals$items$properties$signal_type$enum), length(SIGNAL_TYPES),
   "gemini schema: enum values survive the conversion")
eq(length(.gs$properties$signals$items$required), 6L,
   "gemini schema: required field names survive the conversion")
eq(unlist(.gs$required), "signals", "gemini schema: top-level required survives")

# The Anthropic path must still build correctly, so the provider switch is real.
.saved_prov <- CFG$llm$provider
CFG$llm$provider <- "anthropic"
.ab <- llm_build_body(.news[1, ])
eq(.ab$output_config$format$type, "json_schema", "anthropic: schema-validated JSON")
eq(.ab$model, CFG$llm$anthropic$model, "anthropic: the configured model is sent")
ok(!is.null(.ab$system[[1]]$cache_control), "anthropic: stable prefix marked cacheable")
CFG$llm$provider <- .saved_prov

# The look-ahead gate: an article published after the cutoff is never sent.
.n_calls <- 0L
sig <- suppressMessages(
  extract_strategy_signals(.news, cutoff = as.POSIXct("2026-11-02 00:00:00", tz = "UTC")))
eq(.n_calls, 1L, "an article published after the cutoff is never sent to the model")
eq(nrow(sig), 1L, "the signal from the in-window article is returned")
eq(sig$signal_type, "minutes_restriction", "and carries its type")

# Reproducibility: the same batch again must cost nothing and change nothing.
.n_calls <- 0L
sig2 <- suppressMessages(
  extract_strategy_signals(.news, cutoff = as.POSIXct("2026-11-02 00:00:00", tz = "UTC")))
eq(.n_calls, 0L, "a re-run is served entirely from cache")
eq(sig2$evidence, sig$evidence, "and returns byte-identical extractions")

# The prompt version is part of the cache key, so changing instructions
# re-queries rather than silently mixing two prompts in one file.
.saved <- LLM_PROMPT_VERSION
LLM_PROMPT_VERSION <- "test-v2"
.n_calls <- 0L
invisible(suppressMessages(
  extract_strategy_signals(.news, cutoff = as.POSIXct("2026-11-02 00:00:00", tz = "UTC"))))
eq(.n_calls, 1L, "bumping the prompt version re-queries")
LLM_PROMPT_VERSION <- .saved

# No failure mode may fabricate a signal.
.gcand <- function(txt, fin = "STOP")
  list(candidates = list(list(finishReason = fin,
       content = list(role = "model", parts = list(list(text = txt))))))
for (case in list(
  list(nm = "safety-blocked", r = list(promptFeedback = list(blockReason = "SAFETY"))),
  list(nm = "truncated",      r = .gcand('{"signals":[', "MAX_TOKENS")),
  list(nm = "unparseable",    r = .gcand("sorry!")),
  list(nm = "empty",          r = list(candidates = list())),
  list(nm = "recitation",     r = .gcand("x", "RECITATION")))) {
  p <- llm_parse_response(case$r)
  ok(!p$ok && length(p$signals) == 0,
     paste0("gemini: a ", case$nm, " response yields no signals, not a guess"))
}
ok(llm_parse_response(.gcand('{"signals":[]}'))$ok,
   "a well-formed empty result is a success, not a failure")

# The cache is a readable audit artifact, not an opaque blob.
.recs <- llm_cache_read(CFG$llm$cache)
ok(length(.recs) >= 1, "the cache round-trips through NDJSON")
.one <- .recs[[1]]
ok(all(c("article_id","model","prompt_version","extracted_at","signals") %in% names(.one)),
   "each cached record carries full provenance")
eq(llm_cache_key("a", "v", "m"), "a|v|m", "the cache key is article, prompt version and model")

eq(nrow(strategy_signals_actionable(sig, 0.95)), 0L,
   "the confidence floor excludes signals below it")
eq(nrow(strategy_signals_actionable(sig, 0.5)), 1L, "and keeps those above it")

# One sentence can carry two signal types legitimately ("held to 28 minutes AND
# will not play back-to-backs"). The model is right to report both, but summing
# them downstream would double-count one piece of information about one player.
.dup <- tibble(article_id = "x", player = rep("A Player", 2),
               signal_type = c("minutes_restriction", "load_management"),
               direction = "decrease", confidence = c(1.0, 0.95),
               evidence = "same sentence", attributed_to = "coach",
               headline = "h", published = "p", model = "m",
               prompt_version = "v", extracted_at = "t")
.act <- strategy_signals_actionable(.dup)
eq(nrow(.act), 1L, "two signal types for one player collapse to one actionable row")
eq(.act$signal_type, "minutes_restriction", "the most confident type is kept")
eq(.act$also, "load_management", "and the folded-in type is recorded, not discarded")
eq(.act$n_raw, 2L, "with a count of what was collapsed")

# Opposite directions are genuinely different claims and must NOT be merged.
.opp <- .dup; .opp$direction <- c("decrease", "increase")
eq(nrow(strategy_signals_actionable(.opp)), 2L,
   "opposing directions stay separate rows")
options(nba.llm_call = NULL)
unlink(CFG$llm$cache)

# ===========================================================================
section("Magnitude-plus-favourite spreads")
# ===========================================================================
# The most dangerous shape a betting file takes: the spread stored as a
# positive magnitude with the favourite named in another column. Read naively
# every away-favourite game is inverted, and a blanket sign flip cannot repair
# it -- it lands on a plausible-looking correlation and hides the damage.

eq(resolve_favourite_home(c("home","away"), c("BOS","BOS"), c("LAL","LAL")),
   c(TRUE, FALSE), "side labels resolve")
eq(resolve_favourite_home(c("H","V"), c("BOS","BOS"), c("LAL","LAL")),
   c(TRUE, FALSE), "H/V abbreviations resolve")
eq(resolve_favourite_home(c("Boston Celtics","lakers"), c("BOS","BOS"), c("LAL","LAL")),
   c(TRUE, FALSE), "a team NAME resolves against the two sides")
eq(resolve_favourite_home("MIA", "BOS", "LAL"), NA,
   "a value matching neither side stays NA rather than guessing")

# home favoured by 13 -> home line -13; away favoured by 5 -> home line +5
eq(rebuild_home_spread(c(13, 5), c("home","away"), c("BOS","BOS"), c("LAL","LAL")),
   c(-13, 5), "magnitude + favourite rebuilds a signed home line")
eq(rebuild_home_spread(13, "home", "BOS", "LAL"), -13,
   "the home favourite gets a negative number")
eq(rebuild_home_spread(13, "away", "BOS", "LAL"), 13,
   "the away favourite gets a positive number")
eq(rebuild_home_spread(NA_real_, "home", "BOS", "LAL"), NA_real_,
   "a missing magnitude stays missing")

# The whole-column decision: rebuild only a column that never changes sign.
g_mag <- tibble(home_team = c("BOS","LAL","MIA"), away_team = c("LAL","BOS","NYK"),
                spread_close = c(13, 5, 2), spread_open = NA_real_,
                .fav = c("home","away","home"))
out <- suppressMessages(apply_favourite_orientation(g_mag))
eq(out$spread_close, c(-13, 5, -2), "a single-signed column is rebuilt")

# An already-signed column must be left alone.
g_signed <- tibble(home_team = c("BOS","LAL"), away_team = c("LAL","BOS"),
                   spread_close = c(-6.5, 3.5), spread_open = NA_real_,
                   .fav = c("home","away"))
eq(suppressMessages(apply_favourite_orientation(g_signed))$spread_close, c(-6.5, 3.5),
   "a column that already takes both signs is untouched")

# No favourite column at all -> unchanged.
g_nofav <- g_signed; g_nofav$.fav <- NA_character_
eq(suppressMessages(apply_favourite_orientation(g_nofav))$spread_close, c(-6.5, 3.5),
   "no favourite column means no rebuild")

# The detector must find the column, and must NOT mistake a numeric
# "spread_favorite" (a magnitude) for a side label.
m_fav <- suppressMessages(detect_columns(clean_names(tibble(
  date = "x", home_team = "x", away_team = "x", home_score = 1, away_score = 1,
  whos_favored = "home", spread = 1, total = 1)), overrides = list()))
eq(m_fav[["favourite"]], "whos_favored", "the favourite column is detected")
eq(m_fav[["spread_close"]], "spread", "and does not steal the spread column")

# ===========================================================================
section("Date and number parsing")
# ===========================================================================

eq(parse_dates(c("2025-01-15", "2025-02-20")), as.Date(c("2025-01-15", "2025-02-20")),
   "ISO dates parse")
eq(parse_dates(c(20250115, 20250220)), as.Date(c("2025-01-15", "2025-02-20")),
   "bare YYYYMMDD numbers parse")
eq(parse_dates("2025-01-15T23:00:00Z"), as.Date("2025-01-15"),
   "an ISO datetime drops its time component")

# Regression test for a real bug: a backslash inside a bracket expression is
# literal in R, so the original character class stripped every minus sign --
# silently turning favourites into underdogs and -110 into +110 on any dataset
# that stores these columns as text.
eq(parse_number(c("-3.5", "+3.5")), c(-3.5, 3.5), "signed numbers keep their sign")
eq(parse_number(c("-110", "+150")), c(-110, 150), "negative prices keep their sign")
eq(parse_number("-7"), -7, "a plain negative integer keeps its sign")
eq(parse_number("PK"), 0, "a pick'em is zero")
eq(parse_number("pick'em"), 0, "spelled-out pick'em is zero")
eq(parse_number("3½"), 3.5, "a half-point glyph parses")
eq(parse_number(c("", "-")), c(NA_real_, NA_real_), "empty placeholders become NA")


# ===========================================================================
section("End-to-end load of a real-world-shaped file")
# ===========================================================================
# The synthetic dataset is written by readr, so every numeric column arrives
# already typed and the string-parsing path is never exercised. Public Kaggle
# exports are not like that: they carry quoted text, Title Case headers,
# US-format dates, "PK", and a "Visitor" naming convention. This fixture is
# that shape, and it is the only test that runs the loader end to end.

fx <- "tests/fixtures/kaggle_style.csv"
if (file.exists(fx)) {
  raw <- suppressMessages(readr::read_csv(fx, show_col_types = FALSE)) |> clean_names()
  m <- suppressMessages(detect_columns(raw, overrides = list()))
  eq(m[["home_team"]],   "home_team",       "Title Case home team is found")
  eq(m[["away_team"]],   "visitor_team",    "a 'Visitor' column is recognised as away")
  eq(m[["spread_close"]],"home_line_close", "the closing spread is found")
  eq(m[["spread_open"]], "home_line_open",  "the opening spread is told apart from it")
  eq(m[["total_close"]], "ou_close",        "an OU column is the closing total")
  eq(m[["ml_home"]],     "home_odds_close", "the home moneyline is found")

  g <- suppressMessages(build_games(raw, m))
  eq(nrow(g), 5L, "all five games survive the build")
  # build_games() sorts by date then game_id, so compare as a set.
  eq(sort(g$home_team), sort(c("BOS","LAL","DET","GSW","CHA")),
     "teams map to canonical codes")

  # The signs are the point of this fixture.
  ok(all(g$spread_close[g$home_team %in% c("BOS","LAL","GSW")] < 0),
     "favourites keep their negative closing spread through text parsing")
  ok(all(g$spread_close[g$home_team %in% c("DET","CHA")] > 0),
     "underdogs keep their positive closing spread")
  ok(all(g$ml_home[g$home_team %in% c("BOS","GSW")] < 0),
     "favourite moneylines stay negative")
  ok(all(g$ml_away[g$home_team %in% c("BOS","GSW")] > 0),
     "the matching underdog moneylines stay positive")
  eq(g$spread_open[g$home_team == "DET"], 0, "a PK opening line parses as zero")

  eq(g$total_points, g$home_score + g$away_score, "totals are derived from scores")
  eq(g$margin[g$home_team == "BOS"], 23, "margin is home minus away")
  eq(g$home_win, as.integer(g$margin > 0), "home_win follows the margin")
  eq(g$season, rep(2025L, 5), "October 2024 games belong to the 2025 season")
  ok(all(g$completed), "games with both scores are marked completed")

  # The orientation check in validate_games() must NOT fire on a correct file.
  r <- cor(g$spread_close, g$margin)
  ok(r < 0, "spread and margin correlate negatively, as a correct file should")

  # And settlement on real parsed values must agree with the scores.
  eq(settle_spread(g$margin[g$home_team == "BOS"],
                   g$spread_close[g$home_team == "BOS"], "home"), "win",
     "BOS -7.5 winning by 23 is a cover")
  eq(settle_total(g$total_points[g$home_team == "BOS"],
                  g$total_close[g$home_team == "BOS"], "over"), "win",
     "241 points clears a 232.5 total")
} else {
  ok(FALSE, "fixture tests/fixtures/kaggle_style.csv is missing")
}

# ===========================================================================
cat("\n", strrep("-", 62), "\n", sep = "")
if (.n_fail == 0) {
  cat(sprintf("All %d checks passed.\n", .n_pass))
  quit(status = 0)
} else {
  cat(sprintf("%d passed, %d FAILED:\n", .n_pass, .n_fail))
  for (f in .failures) cat("  - ", f, "\n", sep = "")
  quit(status = 1)
}

# ===========================================================================