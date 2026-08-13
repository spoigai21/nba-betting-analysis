# ---------------------------------------------------------------------------
# 05_forward.R -- predictions for games that have not been played yet
# ---------------------------------------------------------------------------
# A backtest can always be nudged, re-run, and quietly re-specified until it
# looks good. A forward test cannot: the prediction is written to disk, with a
# timestamp, before tip-off, and whatever happens next goes in the same file.
# This script is the honest half of the project.
#
# Two commands:
#
#   log_predictions()   fetch today's games and lines, predict, APPEND to
#                       output/track_record.csv  (run before games start)
#   update_results()    fill in finished games and compute running units
#                       (run the next morning)
#
# Nothing here ever rewrites an existing prediction row. The append rules live
# in R/track_record.R; results are filled into empty cells only -- see the
# guard in update_results().
#
# Three markets are logged: totals and spreads against the live number, and
# moneyline against the live price. Moneyline selection is NOT re-implemented
# here -- it calls the same utils.R::moneyline_pick() the backtest measured,
# because a forward test that selects differently is not a test of anything.
#
# News and availability adjustments (06) are applied HERE and only here. They
# are hopeless to backtest honestly (historical injury feeds record who ended
# up playing, not what was known before tip-off), so the forward record is the
# only place they can be graded. The raw model number is kept alongside the
# adjusted one in every row, so evaluate_news_contribution() can separate the
# model's skill from the reader's.
#
# Needs a free key from https://the-odds-api.com in .env:   ODDS_API_KEY=...
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")
source("R/02_features.R")            # team_games + shared feature definitions
source("R/06_news_signals.R")        # apply_news() + manual_news.csv

MODEL_VERSION <- "lm-v1"             # bump when the model changes, so the track
                                     # record stays interpretable across versions

# ===========================================================================
# 1. Live lines from The Odds API
# ===========================================================================

odds_api_key <- function() {
  key <- Sys.getenv(CFG$odds_api$key_env, "")
  if (!nzchar(key))
    stop("No odds API key. Get a free one at https://the-odds-api.com and put\n",
         "      ODDS_API_KEY=your_key_here\n",
         "  in the .env file at the project root (it is gitignored).", call. = FALSE)
  key
}

fetch_odds_raw <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop('install.packages("jsonlite") to fetch live odds', call. = FALSE)
  url <- sprintf("%s/sports/%s/odds/?apiKey=%s&regions=%s&markets=%s&oddsFormat=%s",
                 CFG$odds_api$base, CFG$odds_api$sport, odds_api_key(),
                 CFG$odds_api$regions, CFG$odds_api$markets, CFG$odds_api$odds_fmt)
  info("fetching live odds ...")
  jsonlite::fromJSON(url, simplifyVector = FALSE)
}

# Flatten the nested JSON into one row per (game, book, market, side).
tidy_odds <- function(raw) {
  map_dfr(raw, function(ev) {
    map_dfr(ev$bookmakers %||% list(), function(bk) {
      map_dfr(bk$markets %||% list(), function(mk) {
        map_dfr(mk$outcomes %||% list(), function(oc) {
          tibble(
            event_id  = ev$id %||% NA_character_,
            commence  = as.POSIXct(ev$commence_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
            home_team = canonical_team(ev$home_team),
            away_team = canonical_team(ev$away_team),
            book      = bk$key,
            market    = mk$key,
            name      = canonical_team(oc$name),
            raw_name  = oc$name %||% NA_character_,
            price     = as.numeric(oc$price %||% NA),
            point     = as.numeric(oc$point %||% NA)
          )
        })
      })
    })
  })
}

# The single best quote a bettor could actually take, per event and side.
#
# `point_dir` says which direction is better for the side being quoted:
#   +1  more points is better -- either side of a spread (the feed quotes each
#       outcome its own number, so higher is always better for that outcome),
#       and the UNDER of a total
#   -1  fewer points is better -- the OVER of a total
#    0  there is no number at all, only a price -- the moneyline
#
# Ties on the number break on price. The result is one real, placeable bet at
# one book, which is the entire point: you cannot bet the median of the market.
#
# CONVENTION, and its limit: the best NUMBER wins first, and price only breaks
# ties. That deliberately does not solve the number-versus-juice trade-off --
# whether -4 at -115 beats -4.5 at -105 depends on how much probability mass
# sits on a 4-point margin, which needs a model this function does not have.
# Taking the number first is the standard convention and is the conservative
# half of the trade (a better number can only help; cheaper juice on a worse
# number can lose outright).
pick_best_quote <- function(d, point_dir) {
  out <- d %>%
    filter(!is.na(.data$price)) %>%
    mutate(.rank = if (point_dir == 0) 0 else point_dir * .data$point,
           .dec  = american_to_decimal(.data$price)) %>%
    group_by(.data$event_id) %>%
    arrange(desc(.data$.rank), desc(.data$.dec), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
  if (!nrow(out))
    return(tibble(event_id = character(), point = numeric(),
                  price = numeric(), book = character()))
  out %>% select("event_id", "point", "price", "book")
}

# One row per game, carrying TWO different views of the market:
#
#   consensus  the median number and price across books. This is the best
#              available estimate of fair value, and it is what a bet is
#              SELECTED against -- disagreeing with the middle of the market is
#              the honest test, and it keeps the selection rule conservative.
#
#   best       the single best number and price actually on offer, with the
#              book that offers it. This is what the bet is EXECUTED and LOGGED
#              at, because it is the only one you could really have taken.
#
# Collapsing these two into one number -- as this function used to, logging the
# median as though it were the price paid -- understates every result by the
# spread between the middle of the market and its best shop.
consensus_lines <- function(od) {
  if (!nrow(od)) return(tibble())
  if (!is.null(CFG$odds_api$bookmaker)) {
    od <- od %>% filter(.data$book == CFG$odds_api$bookmaker)
    if (!nrow(od)) stop("bookmaker '", CFG$odds_api$bookmaker, "' returned no lines",
                        call. = FALSE)
  }

  # Prices are consolidated with median_american(), not median(): the American
  # scale has a hole between -100 and +100, so the plain median of two books at
  # -105 and +105 is 0, which is not a price. See utils.R.
  spreads <- od %>%
    filter(.data$market == "spreads") %>%
    mutate(side = if_else(.data$name == .data$home_team, "home", "away")) %>%
    group_by(.data$event_id, .data$side) %>%
    summarise(point = median(.data$point, na.rm = TRUE),
              price = median_american(.data$price), .groups = "drop") %>%
    pivot_wider(names_from = "side", values_from = c("point", "price")) %>%
    transmute(.data$event_id,
              spread_current   = .data$point_home,
              price_spread_home = .data$price_home,
              price_spread_away = .data$price_away)

  totals <- od %>%
    filter(.data$market == "totals") %>%
    mutate(side = tolower(.data$raw_name)) %>%
    group_by(.data$event_id, .data$side) %>%
    summarise(point = median(.data$point, na.rm = TRUE),
              price = median_american(.data$price), .groups = "drop") %>%
    pivot_wider(names_from = "side", values_from = c("point", "price")) %>%
    transmute(.data$event_id,
              total_current = coalesce(.data$point_over, .data$point_under),
              price_over    = .data$price_over,
              price_under   = .data$price_under)

  # Moneyline ("h2h"). No point, only a price.
  h2h <- od %>%
    filter(.data$market == "h2h") %>%
    mutate(side = if_else(.data$name == .data$home_team, "home", "away")) %>%
    group_by(.data$event_id, .data$side) %>%
    summarise(price = median_american(.data$price), .groups = "drop") %>%
    pivot_wider(names_from = "side", values_from = "price", names_prefix = "ml_")

  # A feed that returned only one side of a market leaves the other column
  # absent entirely, which would break the joins below.
  for (cc in c("ml_home", "ml_away"))
    if (!cc %in% names(h2h)) h2h[[cc]] <- NA_real_
  h2h <- h2h %>% select("event_id", "ml_home", "ml_away")

  # --- best executable quote per side --------------------------------------
  sp_side <- od %>% filter(.data$market == "spreads") %>%
    mutate(side = if_else(.data$name == .data$home_team, "home", "away"))
  tot_side <- od %>% filter(.data$market == "totals") %>%
    mutate(side = tolower(.data$raw_name))
  ml_side <- od %>% filter(.data$market == "h2h") %>%
    mutate(side = if_else(.data$name == .data$home_team, "home", "away"))

  best <- list(
    pick_best_quote(sp_side %>% filter(.data$side == "home"), 1) %>%
      rename(best_spread_home = "point", best_price_spread_home = "price",
             best_book_spread_home = "book"),
    pick_best_quote(sp_side %>% filter(.data$side == "away"), 1) %>%
      rename(best_spread_away = "point", best_price_spread_away = "price",
             best_book_spread_away = "book"),
    # An over wants the lowest total on the board; an under the highest.
    pick_best_quote(tot_side %>% filter(.data$side == "over"), -1) %>%
      rename(best_total_over = "point", best_price_over = "price",
             best_book_over = "book"),
    pick_best_quote(tot_side %>% filter(.data$side == "under"), 1) %>%
      rename(best_total_under = "point", best_price_under = "price",
             best_book_under = "book"),
    pick_best_quote(ml_side %>% filter(.data$side == "home"), 0) %>%
      select(-"point") %>%
      rename(best_ml_home = "price", best_book_ml_home = "book"),
    pick_best_quote(ml_side %>% filter(.data$side == "away"), 0) %>%
      select(-"point") %>%
      rename(best_ml_away = "price", best_book_ml_away = "book")
  ) %>% reduce(full_join, by = "event_id")

  out <- od %>%
    distinct(.data$event_id, .data$commence, .data$home_team, .data$away_team) %>%
    left_join(spreads, by = "event_id") %>%
    left_join(totals,  by = "event_id") %>%
    left_join(h2h,     by = "event_id") %>%
    left_join(best,    by = "event_id") %>%
    mutate(date = as.Date(.data$commence, tz = Sys.timezone()))

  n_shopped <- sum(!is.na(out$best_book_spread_home) | !is.na(out$best_book_over),
                   na.rm = TRUE)
  if (n_shopped)
    info("best-price shopping across ", n_distinct(od$book), " book(s) on ",
         n_shopped, " game(s)")
  out
}

# ===========================================================================
# 2. Where each team stands right now
# ===========================================================================
# Same statistics as 02_features.R, but computed up to "today" instead of up to
# a given historical game.

latest_team_state <- function(tg, as_of = Sys.Date(),
                              form_window = CFG$model$form_window,
                              games = NULL) {
  season <- max(tg$season, na.rm = TRUE)
  cur <- tg %>% filter(.data$season == !!season, .data$date < as_of) %>%
    arrange(.data$team, .data$date)
  if (!nrow(cur))
    stop("No completed games in the latest season of data/processed/games.rds.\n",
         "  Refresh your results file before forward-testing -- predictions built\n",
         "  on stale team form are not honest predictions.", call. = FALSE)

  stale <- as.numeric(as_of - max(cur$date))
  if (stale > 5)
    warn("most recent result in the data is ", round(stale), " days old. ",
         "Team form is stale; refresh results before logging predictions.")

  cur %>%
    group_by(.data$team) %>%
    summarise(
      gp_prior        = n(),
      win_pct_prior   = mean(.data$win),
      pf_prior        = mean(.data$pts_for),
      pa_prior        = mean(.data$pts_against),
      pace_prior      = mean(.data$game_total),
      form_margin     = mean(tail(.data$margin, form_window)),
      form_total      = mean(tail(.data$game_total, form_window)),
      prev_season_net = first(.data$prev_season_net),
      last_game       = max(.data$date),
      games_last_7    = sum(.data$date > as_of - 7),
      .groups = "drop"
    ) %>%
    mutate(net_prior = .data$pf_prior - .data$pa_prior) %>%
    left_join(
      latest_ratings(if (is.null(games)) readRDS(CFG$paths$games_rds) else games,
                     as_of = as_of),
      by = "team")
}

STATE_COLS <- c("gp_prior", "win_pct_prior", "pf_prior", "pa_prior", "net_prior",
                "pace_prior", "form_margin", "form_total", "prev_season_net",
                "last_game", "games_last_7", "rating", "pace_rtg")

# Opponent-adjusted ratings as of today, fitted on the current season's
# completed games. Same ridge design as 02_features.R -- the live path and the
# backtest have to build this feature identically or the backtest is measuring
# a different model than the one placing bets.
latest_ratings <- function(g, as_of = Sys.Date(),
                           lambda    = CFG$model$rating_lambda,
                           min_games = CFG$model$rating_min_games) {
  season <- max(g$season, na.rm = TRUE)
  gs <- g %>% filter(.data$season == !!season, .data$completed, .data$date < as_of)
  teams <- sort(unique(c(g$home_team, g$away_team)))
  blank <- tibble(team = teams, rating = NA_real_, pace_rtg = NA_real_)
  if (nrow(gs) < min_games) {
    warn("only ", nrow(gs), " completed games this season -- ratings not fitted ",
         "(need ", min_games, ")")
    return(blank)
  }

  ti <- setNames(seq_along(teams), teams); p <- length(teams) + 1L
  hi <- ti[gs$home_team]; ai <- ti[gs$away_team]
  XtX_m <- matrix(0, p, p); Xty_m <- numeric(p)
  XtX_t <- matrix(0, p, p); Xty_t <- numeric(p)
  for (k in seq_len(nrow(gs))) {
    xm <- numeric(p); xm[hi[k]] <-  1; xm[ai[k]] <- -1; xm[p] <- 1
    xt <- numeric(p); xt[hi[k]] <-  1; xt[ai[k]] <-  1; xt[p] <- 1
    XtX_m <- XtX_m + tcrossprod(xm); Xty_m <- Xty_m + xm * gs$margin[k]
    XtX_t <- XtX_t + tcrossprod(xt); Xty_t <- Xty_t + xt * gs$total_points[k]
  }
  bm <- solve_ridge(XtX_m, Xty_m, lambda, p)
  bt <- solve_ridge(XtX_t, Xty_t, lambda, p)
  tibble(team = teams,
         rating   = if (is.null(bm)) NA_real_ else bm[seq_along(teams)],
         pace_rtg = if (is.null(bt)) NA_real_ else bt[seq_along(teams)])
}

build_upcoming <- function(lines, state) {
  side <- function(prefix) {
    state %>% select("team", all_of(STATE_COLS)) %>%
      rename_with(~ paste0(prefix, .x), everything())
  }
  missing_teams <- setdiff(c(lines$home_team, lines$away_team), state$team)
  if (length(missing_teams))
    warn("no current-season history for: ", paste(missing_teams, collapse = ", "),
         " -- those games are skipped")

  past <- sum(lines$date < Sys.Date(), na.rm = TRUE)
  if (past)
    warn(past, " of these games are dated in the past. Forward-testing means ",
         "predicting games that have not happened; check the odds feed.")

  up <- lines %>%
    inner_join(side("h_"), by = c("home_team" = "h_team")) %>%
    inner_join(side("a_"), by = c("away_team" = "a_team"))

  # The same floor 02_features.R applies to the backtest. Without it, early in
  # a season the live path bets games the backtest would have thrown away.
  thin <- up$h_gp_prior < CFG$model$min_prior_games |
          up$a_gp_prior < CFG$model$min_prior_games
  if (any(thin, na.rm = TRUE)) {
    info("skipping ", sum(thin, na.rm = TRUE), " game(s) where a team has fewer ",
         "than ", CFG$model$min_prior_games, " games -- the backtest excludes ",
         "these, so betting them would not be testing the same strategy")
    up <- up[!thin, ]
  }

  up %>%
    mutate(
      # Clamped to [0, 7]: same ceiling 02_features.R uses, and a floor so a
      # stale or mis-dated feed cannot produce a negative rest day.
      h_rest_days = pmin(pmax(as.numeric(.data$date - .data$h_last_game), 0), 7),
      a_rest_days = pmin(pmax(as.numeric(.data$date - .data$a_last_game), 0), 7),
      h_b2b = as.integer(.data$h_rest_days <= 1),
      a_b2b = as.integer(.data$a_rest_days <= 1)
    ) %>%
    derive_matchup_features()
}

# ===========================================================================
# 2b. Readiness -- the guard that stops a season boundary logging nonsense
# ===========================================================================
# latest_team_state() takes the newest season in games.rds and calls it "now".
# Run on opening night, before any new results have been loaded, that silently
# returns LAST season's form -- 88 games per team, a last_game five months old,
# ratings fitted on rosters that have since changed -- and logs bets against it.
# The only complaint is a staleness warning, which is easy to scroll past.
#
# The second trap is subtler. 02_features.R drops any game where either team has
# played fewer than min_prior_games, on the grounds that its form numbers mean
# nothing yet. The live path had no such rule, so for the first three weeks of a
# season it would log bets the backtest would have refused to evaluate -- a
# forward test of a strategy the backtest never measured.
#
# Both are refusals rather than warnings, because both fail quietly.

# Fold this season's completed games into games.rds.
#
# Without this the forward test can never start. games.rds is built from the
# Kaggle file, which will not carry the current season until whoever maintains
# it gets round to it -- so team form would sit frozen on last season and
# forward_preflight() would refuse forever, correctly.
#
# hoopR has results the morning after a game, which is all team form needs: it
# reads scores only. Those rows carry no betting lines, so they feed the model
# but cannot themselves be backtested. A later Kaggle refresh fills the lines
# in. Existing rows are never overwritten -- a game already carrying a line
# keeps it.
refresh_results <- function(season = season_of(Sys.Date()),
                            path = CFG$paths$games_rds) {
  if (!requireNamespace("hoopR", quietly = TRUE)) {
    warn('install.packages("hoopR") to refresh results'); return(invisible(NULL))
  }
  step("Refreshing results for season ", season)

  sched <- try(hoopR::load_nba_schedule(seasons = season), silent = TRUE)
  if (inherits(sched, "try-error") || !NROW(sched)) {
    warn("hoopR returned nothing for season ", season); return(invisible(NULL))
  }
  hcol <- intersect(c("home_display_name", "home_name", "home_location"), names(sched))[1]
  acol <- intersect(c("away_display_name", "away_name", "away_location"), names(sched))[1]
  if (is.na(hcol) || is.na(acol)) {
    warn("unrecognised hoopR schedule columns"); return(invisible(NULL))
  }

  fresh <- sched %>%
    filter(!is.na(.data$home_score), !is.na(.data$away_score)) %>%
    transmute(date = as.Date(.data$game_date),
              home_team = canonical_team(.data[[hcol]]),
              away_team = canonical_team(.data[[acol]]),
              home_score = as.numeric(.data$home_score),
              away_score = as.numeric(.data$away_score)) %>%
    # All-Star and Rising Stars rosters map to nothing real and would wreck
    # every pace and defence number they touched.
    filter(.data$home_team %in% TEAM_ALIASES$code,
           .data$away_team %in% TEAM_ALIASES$code) %>%
    mutate(season = as.integer(season),
           total_points = .data$home_score + .data$away_score,
           margin = .data$home_score - .data$away_score,
           home_win = as.integer(.data$margin > 0),
           completed = TRUE,
           game_id = paste0(format(.data$date, "%Y%m%d"), "_",
                            .data$away_team, "_at_", .data$home_team))

  old <- readRDS(path)
  add <- fresh %>% filter(!.data$game_id %in% old$game_id)
  if (!nrow(add)) { info("no new completed games"); return(invisible(old)) }

  out <- bind_rows(old, add) %>% arrange(.data$date, .data$game_id)
  saveRDS(out, path)
  info("added ", nrow(add), " completed game(s); ", nrow(out), " total, through ",
       format(max(out$date)))
  info("these rows carry no betting lines -- they inform form, not the backtest")
  invisible(out)
}

forward_preflight <- function(tg, as_of = Sys.Date(), games = NULL,
                              max_stale_days = 5) {
  issues <- character()

  have <- max(tg$season, na.rm = TRUE)
  want <- season_of(as_of)
  if (is.na(have) || have != want)
    issues <- c(issues, paste0(
      "results file holds season ", have, " but ", format(as_of), " is season ",
      want, ". Refresh data/processed/games.rds with this season's completed ",
      "games -- otherwise last season's form is used as though it were current."))

  cur <- tg %>% filter(.data$season == !!want, .data$date < as_of)
  if (!nrow(cur)) {
    issues <- c(issues, paste0("no completed games in season ", want, " yet."))
  } else {
    stale <- as.numeric(as_of - max(cur$date))
    if (stale > max_stale_days)
      issues <- c(issues, paste0("most recent result is ", round(stale),
                                 " days old (limit ", max_stale_days, ")."))
    gp <- cur %>% count(.data$team)
    thin <- sum(gp$n < CFG$model$min_prior_games)
    if (thin)
      issues <- c(issues, paste0(
        thin, " team(s) have fewer than ", CFG$model$min_prior_games,
        " games. The backtest excludes those games, so betting them now would ",
        "forward-test something it never measured. Wait for them to catch up ",
        "-- individual games are dropped automatically in the meantime."))
  }

  if (!nzchar(Sys.getenv(CFG$odds_api$key_env, "")))
    issues <- c(issues, paste0("no ", CFG$odds_api$key_env, " in .env."))

  list(ok = !length(issues), issues = issues)
}

report_preflight <- function(pf) {
  step("Pre-flight")
  if (pf$ok) { info("ready to log"); return(invisible(TRUE)) }
  for (i in pf$issues) warn(i)
  invisible(FALSE)
}

# ===========================================================================
# 3. Predict and decide
# ===========================================================================

predict_upcoming <- function(up) {
  saved <- readRDS(CFG$paths$models_rds)
  m <- saved$models
  info("using model trained through ", format(saved$trained_through))

  up %>%
    mutate(
      pred_total    = as.numeric(predict(m$total,  newdata = .)),
      pred_margin   = as.numeric(predict(m$margin, newdata = .)),
      pred_win_prob = as.numeric(predict(m$winp, newdata = ., type = "response")),
      # Residual spread of the margin model. Needed below to translate a news
      # adjustment expressed in points into a shift in win probability.
      margin_sd     = stats::sigma(m$margin)
    )
}

# Fold in the news adjustments, then derive everything the bet rules need.
#
# This is deliberately a separate step sitting BETWEEN the model's raw output
# and the comparison with the market. News moves the expected margin, and the
# win probability has to move with it -- otherwise the spread bet and the
# moneyline bet on the same game would be acting on two different opinions
# about who is going to win, which is incoherent rather than merely wrong.
finalise_predictions <- function(p) {
  p %>% mutate(
    news_margin_adj   = coalesce(.data$news_margin_adj, 0),
    news_total_adj    = coalesce(.data$news_total_adj, 0),
    pred_margin_final = .data$pred_margin + .data$news_margin_adj,
    pred_total_final  = .data$pred_total  + .data$news_total_adj,
    # Probit shift: move the z-score by the adjustment measured in residual
    # standard deviations. This is the same normal approximation a book uses
    # when it prices a moneyline off a spread, so the three markets stay
    # consistent with one another.
    win_prob_final = pnorm(qnorm(.data$pred_win_prob) +
                             .data$news_margin_adj / .data$margin_sd),
    model_spread = -.data$pred_margin_final,
    edge_total   = .data$pred_total_final - .data$total_current,
    edge_spread  = .data$spread_current - .data$model_spread
  )
}

# Turn predictions into the rows that go in the track record. One row per bet
# the rules actually trigger; games where the model agrees with the line produce
# no row, which is the correct behaviour.
to_track_rows <- function(p, ts = Sys.time()) {
  stamp <- format(as.POSIXct(ts), "%Y-%m-%d %H:%M:%S %Z")
  gname <- function(d) paste0(d$away_team, " @ ", d$home_team)

  # Every execution field falls back to the consensus quote, so a feed that
  # returns a single book -- or a test harness that supplies only consensus
  # columns -- still produces correct rows.
  for (cc in c("best_total_over", "best_total_under", "best_price_over",
               "best_price_under", "best_spread_home", "best_spread_away",
               "best_price_spread_home", "best_price_spread_away",
               "best_ml_home", "best_ml_away")) {
    if (!cc %in% names(p)) p[[cc]] <- NA_real_
  }
  for (cc in c("best_book_over", "best_book_under", "best_book_spread_home",
               "best_book_spread_away", "best_book_ml_home", "best_book_ml_away")) {
    if (!cc %in% names(p)) p[[cc]] <- NA_character_
  }

  # Append the executing book, and the consensus number when it differs from
  # the number taken, so the log shows what was shopped and by how much.
  exec_note <- function(base, book, taken, consensus) {
    tag <- if_else(is.na(book), NA_character_,
                   if_else(is.na(consensus) | abs(taken - consensus) < 1e-9,
                           paste0("@ ", book),
                           paste0("@ ", book, " (consensus ", consensus, ")")))
    if_else(is.na(base), tag,
            if_else(is.na(tag), base, paste0(base, " | ", tag)))
  }

  totals <- p %>%
    filter(!is.na(.data$edge_total),
           abs(.data$edge_total) >= CFG$backtest$total_edge_threshold) %>%
    mutate(
      # The SIDE is chosen against the consensus number -- the honest test of
      # whether we disagree with the middle of the market. Only then does
      # execution take the best number and price on offer for that side.
      side = if_else(.data$edge_total > 0, "over", "under"),
      exec_line = coalesce(if_else(.data$side == "over",
                                   .data$best_total_over, .data$best_total_under),
                           .data$total_current),
      exec_odds = coalesce(if_else(.data$side == "over",
                                   .data$best_price_over, .data$best_price_under),
                           if_else(.data$side == "over",
                                   .data$price_over, .data$price_under),
                           CFG$backtest$default_price),
      exec_book = if_else(.data$side == "over",
                          .data$best_book_over, .data$best_book_under),
      # The realised edge is against the number actually taken, not the median.
      exec_edge = .data$pred_total_final - .data$exec_line
    ) %>%
    transmute(
      .data$date,
      game = gname(.),
      player = NA_character_,
      stat = "game_total",
      # `prediction` is the number the bet was actually made on; prediction_raw
      # is the model before the reader touched it. Both are kept so the two can
      # be graded apart later.
      prediction     = round(.data$pred_total_final, 1),
      prediction_raw = round(.data$pred_total, 1),
      news_adj       = round(.data$news_total_adj, 2),
      line = .data$exec_line,
      odds = .data$exec_odds,
      bet  = .data$side,
      result = NA_real_, win_loss = NA_character_, units = NA_real_,
      timestamp = stamp,
      market = "total", edge = round(.data$exec_edge, 2),
      model_version = MODEL_VERSION, event_id = .data$event_id,
      line_close = NA_real_, clv_points = NA_real_,
      notes = exec_note(.data$news_note, .data$exec_book,
                        .data$exec_line, .data$total_current)
    )

  spreads <- p %>%
    filter(!is.na(.data$edge_spread),
           abs(.data$edge_spread) >= CFG$backtest$spread_edge_threshold) %>%
    mutate(
      side = if_else(.data$edge_spread > 0, "home", "away"),
      # A spread is logged from the home side throughout this project, so an
      # away bet taken at +5.5 is recorded as a home line of -5.5. Getting this
      # negation wrong would silently invert every away-side settlement.
      exec_line = coalesce(if_else(.data$side == "home",
                                   .data$best_spread_home, -.data$best_spread_away),
                           .data$spread_current),
      exec_odds = coalesce(if_else(.data$side == "home",
                                   .data$best_price_spread_home,
                                   .data$best_price_spread_away),
                           if_else(.data$side == "home",
                                   .data$price_spread_home, .data$price_spread_away),
                           CFG$backtest$default_price),
      exec_book = if_else(.data$side == "home",
                          .data$best_book_spread_home, .data$best_book_spread_away),
      exec_edge = .data$exec_line - .data$model_spread
    ) %>%
    transmute(
      .data$date,
      game = gname(.),
      player = NA_character_,
      stat = "home_margin",
      prediction     = round(.data$pred_margin_final, 1),
      prediction_raw = round(.data$pred_margin, 1),
      news_adj       = round(.data$news_margin_adj, 2),
      line = .data$exec_line,
      odds = .data$exec_odds,
      bet  = .data$side,
      result = NA_real_, win_loss = NA_character_, units = NA_real_,
      timestamp = stamp,
      market = "spread", edge = round(.data$exec_edge, 2),
      model_version = MODEL_VERSION, event_id = .data$event_id,
      line_close = NA_real_, clv_points = NA_real_,
      notes = exec_note(.data$news_note, .data$exec_book,
                        .data$exec_line, .data$spread_current)
    )

  # --- moneyline ----------------------------------------------------------
  # Chosen by the same utils.R::moneyline_pick() the backtest was measured on.
  # For this market `prediction` and `line` are PROBABILITIES: the model's
  # win probability against the market's de-vigged one. That keeps `edge` on a
  # scale where it means something, and gives record_closing_lines() a number
  # to measure closing-line value against.
  moneyline <- NULL
  if (all(c("ml_home", "ml_away") %in% names(p)) && nrow(p)) {
    # Selection runs on the CONSENSUS prices: the de-vigged fair probability
    # comes from the middle of the market, and the EV threshold is tested at
    # the middle of the market too. Executing at a better price can only raise
    # the true EV, so this keeps the guard conservative.
    mlp <- moneyline_pick(p$win_prob_final, p$ml_home, p$ml_away)
    moneyline <- p %>%
      mutate(ml_side = mlp$side, ml_edge = mlp$edge, ml_ev = mlp$ev,
             ml_price = mlp$price, ml_pmkt = mlp$p_market,
             ml_pshrunk = mlp$p_model_shrunk) %>%
      filter(!is.na(.data$ml_side)) %>%
      mutate(
        exec_odds = coalesce(if_else(.data$ml_side == "home",
                                     .data$best_ml_home, .data$best_ml_away),
                             .data$ml_price),
        exec_book = if_else(.data$ml_side == "home",
                            .data$best_book_ml_home, .data$best_book_ml_away),
        # EV restated at the price actually taken, under the same haircut the
        # selection rule used.
        exec_ev = bet_ev(if_else(.data$ml_side == "home",
                                 .data$ml_pshrunk, 1 - .data$ml_pshrunk) -
                           CFG$backtest$ml_prob_error,
                         .data$exec_odds)
      ) %>%
      transmute(
        .data$date,
        game = gname(.),
        player = NA_character_,
        stat = "home_win_prob",
        prediction     = round(.data$win_prob_final, 4),
        prediction_raw = round(.data$pred_win_prob, 4),
        news_adj       = round(.data$win_prob_final - .data$pred_win_prob, 4),
        # `line` stays the consensus de-vigged probability: it is the market's
        # fair view, and it is what record_closing_lines() measures CLV against.
        line = round(.data$ml_pmkt, 4),
        odds = .data$exec_odds,
        bet  = .data$ml_side,
        result = NA_real_, win_loss = NA_character_, units = NA_real_,
        timestamp = stamp,
        market = "moneyline", edge = round(.data$ml_edge, 4),
        model_version = MODEL_VERSION, event_id = .data$event_id,
        line_close = NA_real_, clv_points = NA_real_,
        # paste0() renders NA as the string "NA", which would write a literal
        # "NAev +0.03" into a permanent record. Branch instead of pasting.
        notes = {
          ev <- paste0("ev ", sprintf("%+.3f", .data$exec_ev))
          ev <- if_else(is.na(.data$exec_book), ev,
                        paste0(ev, " @ ", .data$exec_book))
          if_else(is.na(.data$news_note), ev, paste0(.data$news_note, " | ", ev))
        }
      )
  }

  bind_rows(totals, spreads, moneyline) %>%
    arrange(.data$date, .data$game, .data$market)
}

# ===========================================================================
# 4. Public commands
# ===========================================================================

log_predictions <- function(dry_run = FALSE,
                            use_news = CFG$news$apply_in_forward,
                            force = FALSE) {
  step("Forward test: logging predictions")

  # Refuse rather than warn. Both conditions this catches fail silently, and a
  # track record is only worth keeping if every row in it was logged under
  # conditions the backtest actually measured.
  pf <- forward_preflight(team_games)
  report_preflight(pf)
  if (!pf$ok && !dry_run && !force)
    stop("Pre-flight failed -- nothing logged. Fix the above, or pass ",
         "force = TRUE if you have read every item and still mean it.",
         call. = FALSE)
  if (!pf$ok && force) warn("pre-flight overridden with force = TRUE")

  lines <- consensus_lines(tidy_odds(fetch_odds_raw()))
  if (!nrow(lines)) { warn("no upcoming games returned by the odds API"); return(invisible(NULL)) }
  info(nrow(lines), " upcoming games with lines")

  state <- latest_team_state(team_games)
  up    <- build_upcoming(lines, state)
  p     <- predict_upcoming(up)

  # --- news adjustments, forward-test only --------------------------------
  # apply_news() returns zeroed adjustment columns when there is nothing in
  # manual_news.csv, so the un-adjusted path goes through exactly the same
  # arithmetic as the adjusted one. There is no separate "no news" code path
  # that could drift away from the real one.
  p <- if (isTRUE(use_news)) apply_news(p) else apply_news(p, news = tibble())
  p <- finalise_predictions(p)

  n_adj <- sum(p$news_margin_adj != 0 | p$news_total_adj != 0, na.rm = TRUE)
  if (isTRUE(use_news)) {
    info("news adjustments applied to ", n_adj, " of ", nrow(p), " games",
         if (!n_adj) "  (data/manual_news.csv is empty -- see manual_news_template())" else "")
  } else {
    warn("news adjustments DISABLED for this run (use_news = FALSE)")
  }

  rows <- to_track_rows(p)

  if (!nrow(rows)) {
    info("model agrees with the market on every game today -- nothing logged.")
    info("(This is normal and healthy. A model that has an opinion on every ",
         "game has no opinion at all.)")
    return(invisible(NULL))
  }

  print(rows %>% select("date", "game", "stat", "prediction", "prediction_raw",
                        "news_adj", "line", "bet", "odds", "edge"))

  append_track_rows(rows, dry_run = dry_run)
}

# --- results ---------------------------------------------------------------
# Prefers hoopR (an open-source wrapper, not a scraper). If hoopR is missing,
# falls back to any finished games already in data/processed/games.rds, and
# finally tells you which rows to fill in by hand.
fetch_finished_games <- function(dates, source = c("auto", "hoopR", "local")) {
  source <- match.arg(source)
  seasons <- unique(season_of(dates))

  if (source %in% c("auto", "hoopR") && requireNamespace("hoopR", quietly = TRUE)) {
    got <- try({
      sched <- hoopR::load_nba_schedule(seasons = seasons)
      # Pick the name columns that this hoopR version actually ships. Note that
      # `.data$x %||% .data$y` does NOT work here: .data$x errors on a missing
      # column rather than returning NULL, so the fallback never fires.
      hcol <- intersect(c("home_display_name", "home_name", "home_location"), names(sched))[1]
      acol <- intersect(c("away_display_name", "away_name", "away_location"), names(sched))[1]
      if (is.na(hcol) || is.na(acol)) stop("unrecognised schedule columns")
      sched %>%
        filter(!is.na(.data$home_score), !is.na(.data$away_score)) %>%
        transmute(date = as.Date(.data$game_date),
                  home_team = canonical_team(.data[[hcol]]),
                  away_team = canonical_team(.data[[acol]]),
                  home_score = as.numeric(.data$home_score),
                  away_score = as.numeric(.data$away_score)) %>%
        # ESPN's schedule includes All-Star and Rising Stars games, whose
        # "teams" (TEAM CHUCK, TEAM SHAQ, ...) map to nothing real.
        filter(.data$home_team %in% TEAM_ALIASES$code,
               .data$away_team %in% TEAM_ALIASES$code)
    }, silent = TRUE)
    if (!inherits(got, "try-error") && nrow(got)) {
      info("results from hoopR: ", nrow(got), " completed games")
      return(got)
    }
    if (source == "hoopR") { warn("hoopR lookup failed"); return(tibble()) }
    warn("hoopR lookup failed; falling back to the local results file")
  }
  info("results from the local file ", CFG$paths$games_rds)

  readRDS(CFG$paths$games_rds) %>%
    filter(.data$completed) %>%
    select("date", "home_team", "away_team", "home_score", "away_score")
}

update_results <- function(results_source = "auto") {
  step("Forward test: filling in results")
  tr <- read_track_record()
  if (!nrow(tr)) { info("track record is empty -- nothing to update"); return(invisible(NULL)) }

  pending <- is.na(tr$win_loss)
  if (!any(pending)) { info("every logged prediction is already settled"); return(invisible(tr)) }
  info(sum(pending), " pending prediction(s)")

  res <- fetch_finished_games(tr$date[pending], source = results_source) %>%
    mutate(game = paste0(.data$away_team, " @ ", .data$home_team),
           total_points = .data$home_score + .data$away_score,
           margin = .data$home_score - .data$away_score) %>%
    select("date", "game", "total_points", "margin")

  tr2 <- tr %>%
    left_join(res, by = c("date", "game")) %>%
    mutate(
      # What actually happened, in the unit each market is graded in. For the
      # moneyline that is the margin -- the sign of it is the whole result --
      # even though the logged `line` for that market is a probability.
      actual = if_else(.data$market == "total", .data$total_points, .data$margin),
      # Only ever fill blanks. A settled row is never re-graded.
      result   = if_else(is.na(.data$win_loss) & !is.na(.data$actual), .data$actual, .data$result),
      win_loss = if_else(is.na(.data$win_loss) & !is.na(.data$actual),
                         case_when(
                           .data$market == "total"  ~ settle_total(.data$actual, .data$line, .data$bet),
                           .data$market == "spread" ~ settle_spread(.data$actual, .data$line, .data$bet),
                           .data$market == "moneyline" ~ settle_moneyline(.data$actual, .data$bet),
                           # Player props are graded against a line you entered
                           # by hand, and their `game` is a team rather than a
                           # matchup, so they never join a game result here.
                           TRUE ~ NA_character_),
                         .data$win_loss),
      units    = if_else(is.na(.data$units) & !is.na(.data$win_loss),
                         bet_units(.data$odds, .data$win_loss, CFG$backtest$unit),
                         .data$units)
    ) %>%
    select(-"total_points", -"margin", -"actual")

  newly <- sum(is.na(tr$win_loss) & !is.na(tr2$win_loss))
  write_track_record(tr2)
  info("settled ", newly, " prediction(s)")

  if (newly == 0) {
    warn("no pending prediction matched a finished game. Usual causes:")
    warn("  - the games have not finished yet")
    warn('  - the results source has no record of them (predictions logged ',
         'against synthetic data will never match real ESPN results -- ',
         'try update_results(results_source = "local"))')
    warn("  - a team-name or date mismatch between the log and the results")
  }

  report_track_record(tr2)
  invisible(tr2)
}

# Run shortly before tip-off to capture the closing number, which is what makes
# CLV measurable on the live record.
record_closing_lines <- function() {
  step("Recording closing lines")
  tr <- read_track_record()
  open_rows <- which(is.na(tr$line_close) & tr$date >= Sys.Date())
  if (!length(open_rows)) { info("nothing to update"); return(invisible(NULL)) }

  lines <- consensus_lines(tidy_odds(fetch_odds_raw()))
  if (!nrow(lines)) { warn("odds feed returned nothing -- no closes recorded")
                      return(invisible(NULL)) }

  lines <- lines %>%
    mutate(game = paste0(.data$away_team, " @ ", .data$home_team),
           # The moneyline's "closing number" is the market's de-vigged closing
           # probability for the home side -- the same quantity `line` holds for
           # that market, so the two are comparable.
           ml_prob_home = devig_two_way(.data$ml_home, .data$ml_away)$a) %>%
    select("date", "game", "spread_current", "total_current", "ml_prob_home")

  tr <- tr %>%
    left_join(lines, by = c("date", "game")) %>%
    mutate(
      close_now = case_when(
        .data$market == "total"     ~ .data$total_current,
        .data$market == "spread"    ~ .data$spread_current,
        .data$market == "moneyline" ~ .data$ml_prob_home,
        TRUE ~ NA_real_),
      line_close = if_else(is.na(.data$line_close), .data$close_now, .data$line_close),
      # Same clv_of() the backtest uses, so a sign convention can never drift
      # between the two halves of the project. Units follow the market: points
      # for totals and spreads, probability for the moneyline -- anything that
      # aggregates this column must group by market (report_track_record does).
      clv_points = if_else(is.na(.data$clv_points),
                           clv_of(.data$market, .data$bet, .data$line, .data$line_close),
                           .data$clv_points)
    ) %>%
    select(-"spread_current", -"total_current", -"ml_prob_home", -"close_now")

  write_track_record(tr)
  info("closing lines recorded for ", sum(!is.na(tr$line_close)), " row(s)")
  invisible(tr)
}

# ---------------------------------------------------------------------------
step("05_forward.R loaded")
info("log_predictions(dry_run = TRUE)   preview today's bets, write nothing")
info("log_predictions()                 write timestamped predictions")
info("record_closing_lines()            capture the close, for CLV")
info("update_results()                  grade finished games, print the record")
