# ---------------------------------------------------------------------------
# 99_make_sample_data.R -- a synthetic NBA betting dataset
# ---------------------------------------------------------------------------
# Lets you run the whole pipeline before (or without) downloading anything from
# Kaggle. It writes data/sample_nba_betting.csv with deliberately Kaggle-ish
# column names and a few junk columns, so 01_load_data.R's column detection gets
# a real workout.
#
# WHAT IT SIMULATES
#   * 30 teams with persistent strength and pace, drifting season to season
#   * a real-ish schedule (rest days and back-to-backs emerge from it)
#   * scores drawn around the true expected margin/total
#   * a market where the CLOSING line sits on the true expectation plus small
#     noise, and the OPENING line is a noisier draw, priced with a normal hold
#
# READ THIS BEFORE TRUSTING A RESULT ON THIS DATA
#   The closing line is efficient BY CONSTRUCTION: nothing the model can compute
#   beats it, because the close already knows the true expectation. So a backtest
#   graded against the close will honestly report "no edge", which is the point --
#   it shows the pipeline reports no-edge correctly rather than inventing one.
#
#   The opening line is deliberately looser, exactly as real openers are. The
#   backtest bets the open, so expect a SMALL positive closing-line value and an
#   ROI whose confidence interval still straddles zero. That is a faithful
#   picture of what beating stale numbers looks like: detectable in CLV long
#   before it is detectable in profit.
#
#   What you should NOT see is the model beating the CLOSING line's accuracy in
#   03_model.R. If that ever happens on this file, it is a leak, not an edge.
#
# Run:  source("R/99_make_sample_data.R")
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")

SAMPLE_PATH <- "data/sample_nba_betting.csv"

TEAM_FULL_NAMES <- c(
  ATL = "Atlanta Hawks",       BOS = "Boston Celtics",      BKN = "Brooklyn Nets",
  CHA = "Charlotte Hornets",   CHI = "Chicago Bulls",       CLE = "Cleveland Cavaliers",
  DAL = "Dallas Mavericks",    DEN = "Denver Nuggets",      DET = "Detroit Pistons",
  GSW = "Golden State Warriors", HOU = "Houston Rockets",   IND = "Indiana Pacers",
  LAC = "Los Angeles Clippers", LAL = "Los Angeles Lakers", MEM = "Memphis Grizzlies",
  MIA = "Miami Heat",          MIL = "Milwaukee Bucks",     MIN = "Minnesota Timberwolves",
  NOP = "New Orleans Pelicans", NYK = "New York Knicks",    OKC = "Oklahoma City Thunder",
  ORL = "Orlando Magic",       PHI = "Philadelphia 76ers",  PHX = "Phoenix Suns",
  POR = "Portland Trail Blazers", SAC = "Sacramento Kings", SAS = "San Antonio Spurs",
  TOR = "Toronto Raptors",     UTA = "Utah Jazz",           WAS = "Washington Wizards"
)

# ===========================================================================
# Schedule
# ===========================================================================
# Every ordered pair of teams meets at least once, topped up at random to the
# requested game count, then greedily spread across the calendar: at most 8
# games a night, no team twice in a night, no team three nights running.

make_schedule <- function(season, n_games) {
  teams <- names(TEAM_FULL_NAMES)
  pairs <- expand.grid(home = teams, away = teams, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$home != pairs$away, ]
  if (n_games > nrow(pairs))
    pairs <- rbind(pairs, pairs[sample(nrow(pairs), n_games - nrow(pairs), TRUE), ])
  pool <- pairs[sample(nrow(pairs), n_games), ]

  dates <- seq(as.Date(sprintf("%d-10-20", season)),
               as.Date(sprintf("%d-04-12", season + 1)), by = "day")
  nd <- length(dates)
  used  <- matrix(FALSE, nrow = nd, ncol = length(teams), dimnames = list(NULL, teams))
  slots <- integer(nd)
  out   <- rep(NA_integer_, nrow(pool))

  free_on <- function(d, tm) {
    if (used[d, tm]) return(FALSE)
    if (d >= 3 && used[d - 1, tm] && used[d - 2, tm]) return(FALSE)  # no 3-in-3
    TRUE
  }

  for (i in seq_len(nrow(pool))) {
    h <- pool$home[i]; a <- pool$away[i]
    for (d in seq_len(nd)) {
      if (slots[d] >= 8) next
      if (free_on(d, h) && free_on(d, a)) {
        used[d, h] <- TRUE; used[d, a] <- TRUE; slots[d] <- slots[d] + 1L
        out[i] <- d
        break
      }
    }
  }

  tibble(home = pool$home, away = pool$away, date = dates[out]) %>%
    filter(!is.na(.data$date)) %>%
    arrange(.data$date) %>%
    mutate(season_start = season)
}

# Rest days per team, derived from the schedule we just built.
add_rest <- function(sched) {
  long <- bind_rows(
    sched %>% transmute(.data$date, row = row_number(), team = .data$home, side = "home"),
    sched %>% transmute(.data$date, row = row_number(), team = .data$away, side = "away")
  ) %>%
    arrange(.data$team, .data$date) %>%
    group_by(.data$team) %>%
    mutate(rest = pmin(as.numeric(.data$date - lag(.data$date)), 7)) %>%
    ungroup() %>%
    mutate(rest = coalesce(.data$rest, 7), b2b = as.integer(.data$rest <= 1))

  sched %>%
    mutate(row = row_number()) %>%
    left_join(long %>% filter(.data$side == "home") %>% select("row", h_b2b = "b2b"), by = "row") %>%
    left_join(long %>% filter(.data$side == "away") %>% select("row", a_b2b = "b2b"), by = "row") %>%
    select(-"row")
}

# ===========================================================================
# Team strength
# ===========================================================================

simulate_ratings <- function(seasons) {
  teams <- names(TEAM_FULL_NAMES)
  rating <- rnorm(length(teams), 0, 4.5)
  pace   <- rnorm(length(teams), 0, 2.5)
  map_dfr(seasons, function(s) {
    rating <<- 0.70 * rating + rnorm(length(teams), 0, 3.0)   # regression + churn
    pace   <<- 0.85 * pace   + rnorm(length(teams), 0, 1.2)
    tibble(season_start = s, team = teams, rating = rating, pace = pace)
  })
}

# ===========================================================================
# Games and market
# ===========================================================================

simulate_season <- function(season, ratings, cfg = CFG$sample) {
  sched <- make_schedule(season, cfg$games_per_season) %>% add_rest()
  r <- ratings %>% filter(.data$season_start == season)
  rate <- setNames(r$rating, r$team)
  pce  <- setNames(r$pace,   r$team)

  n <- nrow(sched)
  # True expectations. The rest penalty is real AND known to the market, so it
  # creates no exploitable edge -- exactly like the real world.
  exp_margin <- rate[sched$home] - rate[sched$away] + cfg$home_advantage -
    1.4 * sched$h_b2b + 1.4 * sched$a_b2b
  exp_total  <- 224 + pce[sched$home] + pce[sched$away] -
    1.5 * (sched$h_b2b + sched$a_b2b)
  exp_margin <- as.numeric(exp_margin); exp_total <- as.numeric(exp_total)

  margin <- exp_margin + rnorm(n, 0, cfg$margin_sd)
  total  <- exp_total  + rnorm(n, 0, cfg$total_sd)

  home_score <- round((total + margin) / 2)
  away_score <- round((total - margin) / 2)
  tie <- home_score == away_score
  home_score[tie] <- home_score[tie] + ifelse(margin[tie] >= 0, 1, 0)
  away_score[tie] <- away_score[tie] + ifelse(margin[tie] <  0, 1, 0)

  # --- the market ---------------------------------------------------------
  spread_close <- round_half(-(exp_margin + rnorm(n, 0, cfg$line_noise_sd)))
  total_close  <- round_half(exp_total + rnorm(n, 0, cfg$line_noise_sd * 2.5))
  spread_open  <- round_half(spread_close + rnorm(n, 0, cfg$open_noise_sd))
  total_open   <- round_half(total_close  + rnorm(n, 0, cfg$open_noise_sd * 1.5))

  # Two-way prices that add up to a normal ~4.5% hold.
  ph <- sample(c(-105, -108, -110, -112, -115), n, TRUE)
  pa <- -220 - ph
  po <- sample(c(-105, -108, -110, -112, -115), n, TRUE)
  pu <- -220 - po

  p_home <- pnorm(exp_margin / cfg$margin_sd)
  q_home <- pmin(pmax(p_home * 1.045, 0.01), 0.985)
  q_away <- pmin(pmax((1 - p_home) * 1.045, 0.01), 0.985)

  tibble(
    game_date         = sched$date,
    season            = season,
    arena             = paste(TEAM_FULL_NAMES[sched$home], "Arena"),   # decoy column
    attendance        = round(rnorm(n, 18000, 1500)),                  # decoy column
    home_team         = unname(TEAM_FULL_NAMES[sched$home]),
    away_team         = unname(TEAM_FULL_NAMES[sched$away]),
    home_score        = home_score,
    away_score        = away_score,
    spread_home_open  = spread_open,
    total_open        = total_open,
    spread_home_close = spread_close,
    total_close       = total_close,
    spread_price_home = ph,
    spread_price_away = pa,
    over_odds         = po,
    under_odds        = pu,
    moneyline_home    = round(prob_to_american(q_home) / 5) * 5,
    moneyline_away    = round(prob_to_american(q_away) / 5) * 5
  )
}

# ===========================================================================
# Run
# ===========================================================================

set.seed(20260805)
seasons <- CFG$sample$seasons - 1L        # config lists ending years; simulate start years
ratings <- simulate_ratings(seasons)

step("Simulating ", length(seasons), " seasons")
sample_data <- map_dfr(seasons, ~ simulate_season(.x, ratings))
info(nrow(sample_data), " games from ",
     format(min(sample_data$game_date)), " to ", format(max(sample_data$game_date)))

readr::write_csv(sample_data, SAMPLE_PATH)
step("Wrote ", SAMPLE_PATH)
info("Now run:  source(\"run_all.R\")")
info("Reminder: this market is efficient by construction. 'No edge' is the correct answer.")
