# ---------------------------------------------------------------------------
# 07_usage_model.R -- when a player sits, who absorbs the usage?
# ---------------------------------------------------------------------------
# 06_news_signals.R answers "who is out tonight". This file answers the part
# that actually has money in it: "so whose stat line goes up, and by how much".
#
# That second question is NOT an NLP problem. It is measurable. For every pair
# of teammates (A, B) we compare B's per-game production in games A played
# against games A missed. The difference, shrunk toward zero for small samples,
# is the usage transfer from A to B.
#
# WHAT CAN AND CANNOT BE VALIDATED
#   CAN: the usage-transfer estimates themselves. They come from completed box
#        scores, so validate_usage_model() tests them out-of-sample, honestly,
#        against a no-adjustment baseline. If they do not beat the baseline,
#        this whole file is decoration and it will say so.
#   CANNOT: the news timing. Whether you would really have known about the
#        absence before tip-off is untestable after the fact -- which is why
#        news-driven props are forward-test only. See the header of 06.
#
# A NOTE ON PROP LINES
#   The Odds API free tier does not carry player props, so there is no line to
#   bet into automatically. Projections are still logged to the track record
#   with the line left blank; fill it in by hand from your book if you want a
#   graded record. The projection itself is timestamped either way.
#
# Output: data/processed/usage_transfer.rds
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")
# 07 consumes 06's output ("who is out") to produce its own ("who benefits"),
# so it pulls 06 in rather than making the caller remember to. Guarded so
# sourcing both files in one session does not re-run 06's banner.
if (!exists("news_absences")) source("R/06_news_signals.R")

USAGE <- list(
  seasons        = NULL,   # NULL = current and previous season
  min_games_played   = 20, # games needed before a player is "in the rotation"
  min_games_missed   = 5,  # a pair needs this many absences to estimate anything
  shrink_k           = 8,  # games-missed at which the estimate is trusted halfway
  baseline_window    = 15, # rolling games for a player's own baseline

  # Minutes floors. The absent player has to be someone whose absence actually
  # frees up usage -- a 6-minute end-of-bench player going out explains nothing,
  # and including them just manufactures spurious pairs.
  min_mpg_absent      = 18,
  min_mpg_beneficiary = 8,

  stats = c("minutes", "points", "rebounds", "assists",
            "three_point_field_goals_made", "field_goals_attempted")
)

# The two seasons worth pulling, given today's date.
#
# A season labelled S runs from October of S-1 to June of S. Between the end of
# one season and the start of the next -- roughly late June to late October --
# season_of() already reports the NEXT season, which has not played a game.
# Asking hoopR for it returns nothing, so during the offseason the useful pair
# is the two seasons that actually finished.
default_seasons <- function(today = Sys.Date()) {
  s <- year(today) + (month(today) >= 8)
  started <- today >= as.Date(sprintf("%d-10-15", s - 1L))
  if (started) c(s - 1L, s) else c(s - 2L, s - 1L)
}

# ===========================================================================
# 1. Player-game table, with absences made explicit
# ===========================================================================
# The `reason` column is not usable: it reads "COACH'S DECISION" on tens of
# thousands of rows including ones where the player clearly played. Minutes are
# the reliable signal -- a rotation player with no minutes did not play.

load_player_games <- function(seasons = USAGE$seasons %||% default_seasons()) {
  if (!requireNamespace("hoopR", quietly = TRUE))
    stop('install.packages("hoopR")', call. = FALSE)
  info("loading player box scores for ", paste(seasons, collapse = ", "), " ...")
  pb <- hoopR::load_nba_player_box(seasons = seasons)

  pb %>%
    filter(.data$season_type == 2) %>%          # regular season only
    transmute(
      .data$game_id, .data$season,
      date = as.Date(.data$game_date),
      athlete_id = as.character(.data$athlete_id),
      player = .data$athlete_display_name,
      team = canonical_team(.data$team_abbreviation),
      minutes = coalesce(as.numeric(.data$minutes), 0),
      points = as.numeric(.data$points),
      rebounds = as.numeric(.data$rebounds),
      assists = as.numeric(.data$assists),
      three_point_field_goals_made = as.numeric(.data$three_point_field_goals_made),
      field_goals_attempted = as.numeric(.data$field_goals_attempted),
      starter = .data$starter
    ) %>%
    mutate(played = .data$minutes > 0) %>%
    filter(.data$team %in% TEAM_ALIASES$code)   # drops All-Star rosters
}

# Every player the box scores know about, with the athlete_id that ESPN's news
# API also uses. Two jobs:
#   1. 06_news_signals.R takes this as its `gazetteer` argument, so a player
#      mentioned in an article ESPN did not tag can still be recognised.
#   2. absent_ids_from_news() uses it to turn names back into ids.
# One row per player; `team` is the most recent team seen, so a traded player
# resolves to where he is now.
player_gazetteer <- function(pg, min_games = 5) {
  if (!NROW(pg)) return(tibble(athlete_id = character(), player = character(),
                               team = character(), games = integer(),
                               last_game = as.Date(character())))
  pg %>%
    filter(.data$played) %>%
    arrange(.data$athlete_id, .data$date) %>%
    group_by(.data$athlete_id, .data$player) %>%
    summarise(games = n(), team = last(.data$team), last_game = max(.data$date),
              .groups = "drop") %>%
    filter(.data$games >= min_games) %>%
    # Longest-tenured first, so that if two players somehow share a display
    # name the better-established one wins the name match.
    arrange(desc(.data$games))
}

# 06 produces "these players are probably out"; project_props() wants athlete
# ids. This is the join between them.
#
# ESPN tags most articles with athlete ids directly, so most rows arrive already
# resolved. The gazetteer is the fallback for players who were only matched by
# name out of the sentence text.
absent_ids_from_news <- function(absences, gazetteer = NULL) {
  if (!NROW(absences)) { info("no absences supplied"); return(character()) }
  ids <- as.character(absences$athlete_id)

  unresolved <- is.na(ids) | !nzchar(ids)
  if (any(unresolved) && !is.null(gazetteer) && nrow(gazetteer)) {
    ids[unresolved] <- gazetteer$athlete_id[
      match(absences$player[unresolved], gazetteer$player)]
  }

  out <- unique(ids[!is.na(ids) & nzchar(ids)])
  info(length(out), " of ", nrow(absences),
       " flagged player(s) resolved to an athlete_id")
  lost <- absences$player[is.na(ids) | !nzchar(ids)]
  if (length(lost))
    warn("unresolved (no id, not in the gazetteer): ", paste(lost, collapse = ", "))
  out
}

# Rotation players, with the window during which they were actually on the team.
# The tenure window is the important part -- see team_game_availability().
rotation_players <- function(pg, min_games = USAGE$min_games_played, min_mpg = 0) {
  pg %>%
    filter(.data$played) %>%
    group_by(.data$season, .data$team, .data$athlete_id, .data$player) %>%
    summarise(gp = n(), mpg = mean(.data$minutes),
              first_game = min(.data$date), last_game = max(.data$date),
              .groups = "drop") %>%
    filter(.data$gp >= min_games, .data$mpg >= min_mpg)
}

# For each team-game, who was available-in-principle and who actually played.
#
# "Available in principle" is restricted to a player's TENURE WINDOW with that
# team -- first game played to last game played. Without this, a February trade
# acquisition counts as "absent" for October through January, so their absence
# correlates with the calendar rather than causing anything, and the estimates
# come out backwards. This one filter is the difference between a usage model
# and a spurious-correlation generator.
#
# open_ended = TRUE drops the upper bound. Use it when applying the model
# FORWARD -- to games after the data ends, where `last_game` is just "the last
# game we happen to have seen", not the end of the player's tenure. Leaving it
# FALSE there silently filters away every future game.
team_game_availability <- function(pg, rot = rotation_players(pg), open_ended = FALSE) {
  team_games <- pg %>% distinct(.data$season, .data$team, .data$game_id, .data$date)
  played <- pg %>% filter(.data$played) %>%
    select("game_id", "athlete_id") %>% mutate(did_play = TRUE)

  out <- rot %>%
    select("season", "team", "athlete_id", "player", "first_game", "last_game") %>%
    inner_join(team_games, by = c("season", "team"), relationship = "many-to-many") %>%
    filter(.data$date >= .data$first_game)
  if (!open_ended) out <- out %>% filter(.data$date <= .data$last_game)

  out %>%
    left_join(played, by = c("game_id", "athlete_id")) %>%
    mutate(did_play = coalesce(.data$did_play, FALSE)) %>%
    select(-"first_game", -"last_game")
}

# ===========================================================================
# 2. Usage transfer: A out  ->  B's line moves by how much?
# ===========================================================================
# Shrinkage matters more than the raw difference here. A pair with 5 shared
# absences produces a delta that is mostly noise; shrink_k controls how fast we
# start believing it. delta_shrunk = delta_raw * n / (n + k).

estimate_usage_transfer <- function(pg, avail = NULL,
                                    min_missed = USAGE$min_games_missed,
                                    k = USAGE$shrink_k,
                                    stats = USAGE$stats,
                                    min_mpg_absent = USAGE$min_mpg_absent,
                                    min_mpg_beneficiary = USAGE$min_mpg_beneficiary) {
  # Two different populations: whose absence we model, and whose line we predict.
  rot_absent <- rotation_players(pg, min_mpg = min_mpg_absent)
  rot_benef  <- rotation_players(pg, min_mpg = min_mpg_beneficiary)
  avail <- avail %||% team_game_availability(pg, rot_absent)

  # B's actual production, restricted to games B played.
  prod <- pg %>% filter(.data$played) %>%
    semi_join(rot_benef, by = c("season", "team", "athlete_id")) %>%
    select("game_id", "season", "team", beneficiary_id = "athlete_id",
           beneficiary = "player", all_of(stats))

  # A's availability per team-game.
  absent <- avail %>%
    select("game_id", "season", "team", absent_id = "athlete_id",
           absent_player = "player", "did_play")

  step("Estimating usage transfer")
  info(nrow(rot_absent), " players whose absence is modelled (>= ", min_mpg_absent,
       " mpg); ", nrow(rot_benef), " possible beneficiaries; ",
       n_distinct(prod$game_id), " games")

  pairs <- prod %>%
    inner_join(absent, by = c("game_id", "season", "team"),
               relationship = "many-to-many") %>%
    filter(.data$beneficiary_id != .data$absent_id)

  long <- pairs %>%
    pivot_longer(all_of(stats), names_to = "stat", values_to = "value") %>%
    filter(!is.na(.data$value))

  out <- long %>%
    group_by(.data$season, .data$team, .data$absent_id, .data$absent_player,
             .data$beneficiary_id, .data$beneficiary, .data$stat) %>%
    summarise(
      n_with    = sum(.data$did_play),
      n_without = sum(!.data$did_play),
      mean_with    = mean(.data$value[.data$did_play]),
      mean_without = mean(.data$value[!.data$did_play]),
      .groups = "drop"
    ) %>%
    filter(.data$n_without >= min_missed, .data$n_with >= min_missed) %>%
    mutate(
      delta_raw    = .data$mean_without - .data$mean_with,
      shrinkage    = .data$n_without / (.data$n_without + k),
      delta        = .data$delta_raw * .data$shrinkage
    ) %>%
    arrange(desc(abs(.data$delta)))

  info(nrow(out), " (absent, beneficiary, stat) estimates survived the sample floors")
  out
}

# ===========================================================================
# 3. Baselines and projections
# ===========================================================================
# A player's own recent form, computed on strictly prior games.

player_baselines <- function(pg, as_of = Sys.Date(), window = USAGE$baseline_window,
                             stats = USAGE$stats) {
  d <- pg %>%
    filter(.data$played, .data$date < as_of) %>%
    arrange(.data$athlete_id, .data$date)

  # A traded player has rows under two teams. Collapse to ONE baseline per
  # player -- their form travels with them -- tagged with their current team.
  latest_team <- d %>% group_by(.data$athlete_id) %>%
    slice_tail(n = 1) %>% ungroup() %>% select("athlete_id", "team")

  d %>%
    group_by(.data$athlete_id, .data$player) %>%
    filter(n() >= 5) %>%
    summarise(across(all_of(stats), ~ mean(tail(.x, window))),
              games = n(), last_game = max(.data$date), .groups = "drop") %>%
    left_join(latest_team, by = "athlete_id") %>%
    pivot_longer(all_of(stats), names_to = "stat", values_to = "baseline")
}

# Given tonight's absences, project every teammate's line.
#
# method = "routed" (the default) scales a player's production by the extra
# MINUTES the absences hand him, rather than adding each stat's own transfer.
# validate_usage_model() is why: routed beats the baseline on points and
# minutes with the interval clear of zero, while "direct" is reliably WORSE
# than doing nothing on assists, rebounds and threes. Only change this if your
# own validation run says otherwise on your data.
project_props <- function(absent_ids, transfer, baselines, as_of = Sys.Date(),
                          method = c("routed", "direct")) {
  method <- match.arg(method)
  if (!length(absent_ids)) { info("no absences supplied"); return(tibble()) }
  season <- max(transfer$season, na.rm = TRUE)

  bumps <- transfer %>%
    filter(.data$season == !!season, .data$absent_id %in% absent_ids) %>%
    group_by(.data$team, .data$beneficiary_id, .data$beneficiary, .data$stat) %>%
    # Several players out at once: sum the transfers, but damp the total. Two
    # absences do not hand a role player twice the extra shots.
    summarise(bump_raw = sum(.data$delta), n_absent = n_distinct(.data$absent_id),
              from = paste(unique(.data$absent_player), collapse = ", "),
              .groups = "drop") %>%
    mutate(bump = .data$bump_raw / sqrt(pmax(.data$n_absent, 1)))

  minutes_bump <- bumps %>% filter(.data$stat == "minutes") %>%
    select("team", "beneficiary_id", minutes_bump = "bump")
  base_minutes <- baselines %>% filter(.data$stat == "minutes") %>%
    select("athlete_id", baseline_minutes = "baseline")

  baselines %>%
    inner_join(bumps, by = c("athlete_id" = "beneficiary_id", "stat", "team")) %>%
    left_join(minutes_bump, by = c("athlete_id" = "beneficiary_id", "team")) %>%
    left_join(base_minutes, by = "athlete_id") %>%
    mutate(
      minutes_bump = coalesce(.data$minutes_bump, 0),
      routed = if_else(!is.na(.data$baseline_minutes) & .data$baseline_minutes > 4,
                       .data$baseline * (1 + .data$minutes_bump / .data$baseline_minutes),
                       .data$baseline),
      direct = .data$baseline + .data$bump,
      projection = if (method == "routed") .data$routed else .data$direct
    ) %>%
    transmute(
      date = as_of, .data$team, player = .data$player, .data$stat,
      .data$baseline, bump = .data$projection - .data$baseline,
      .data$projection, .data$minutes_bump,
      absent = .data$from, .data$games, method = method
    ) %>%
    filter(abs(.data$bump) > 0.01) %>%
    arrange(desc(abs(.data$bump)))
}

# ===========================================================================
# 4. Honest validation
# ===========================================================================
# Fit the transfer estimates on early games, then test on later ones: when a
# rotation player was out, did baseline+bump predict teammates' lines better
# than baseline alone? A negative answer here is the whole answer.

validate_usage_model <- function(pg, split_frac = 0.7, stats = c("points", "minutes"),
                                 quiet = FALSE, replicate_by_season = TRUE) {
  if (!quiet) step("Validating the usage model out-of-sample")
  dates <- sort(unique(pg$date))
  cut <- dates[floor(length(dates) * split_frac)]
  train <- pg %>% filter(.data$date <= cut)
  test  <- pg %>% filter(.data$date > cut)
  if (!quiet)
    info("train to ", format(cut), " (", n_distinct(train$game_id), " games); test after (",
         n_distinct(test$game_id), " games)")

  transfer <- if (quiet) suppressMessages(estimate_usage_transfer(train, stats = stats))
              else estimate_usage_transfer(train, stats = stats)
  if (!nrow(transfer)) {
    if (!quiet) warn("no transfer estimates -- not enough history")
    return(invisible(NULL))
  }

  # open_ended: the test games all fall after training ends, so the tenure
  # window must not be capped at the last game we saw in training.
  rot <- rotation_players(train, min_mpg = USAGE$min_mpg_absent)
  avail_test <- team_game_availability(test, rot, open_ended = TRUE)
  outs <- avail_test %>% filter(!.data$did_play) %>%
    select("game_id", "team", absent_id = "athlete_id")

  # Baselines from TRAIN only, so the test period is genuinely unseen.
  base <- player_baselines(train, as_of = cut + 1, stats = stats) %>%
    select("athlete_id", "stat", "baseline")

  actual <- test %>% filter(.data$played) %>%
    select("game_id", "team", "athlete_id", "player", all_of(stats)) %>%
    pivot_longer(all_of(stats), names_to = "stat", values_to = "actual")

  bumps <- outs %>%
    inner_join(transfer %>% select("team", "absent_id", "beneficiary_id", "stat", "delta"),
               by = c("team", "absent_id"), relationship = "many-to-many") %>%
    group_by(.data$game_id, athlete_id = .data$beneficiary_id, .data$stat) %>%
    summarise(bump = sum(.data$delta) / sqrt(n_distinct(.data$absent_id)), .groups = "drop")

  # Two competing adjustment schemes, tested on identical rows:
  #   direct : baseline_stat + delta_stat            (add the stat's own transfer)
  #   routed : baseline_stat * (1 + minutes_bump / baseline_minutes)
  #            (scale production by the extra playing time)
  # Routing exists because minutes redistribution is mechanical -- 240 minutes
  # must go somewhere -- whereas a replacement inherits the absent player's
  # minutes without inheriting his efficiency or role.
  mins <- base %>% filter(.data$stat == "minutes") %>%
    select("athlete_id", baseline_minutes = "baseline")
  mins_bump <- bumps %>% filter(.data$stat == "minutes") %>%
    select("game_id", "athlete_id", minutes_bump = "bump")

  ev <- actual %>%
    inner_join(base, by = c("athlete_id", "stat")) %>%
    left_join(bumps, by = c("game_id", "athlete_id", "stat")) %>%
    left_join(mins, by = "athlete_id") %>%
    left_join(mins_bump, by = c("game_id", "athlete_id")) %>%
    mutate(
      bump = coalesce(.data$bump, 0),
      minutes_bump = coalesce(.data$minutes_bump, 0),
      direct = .data$baseline + .data$bump,
      routed = if_else(!is.na(.data$baseline_minutes) & .data$baseline_minutes > 4,
                       .data$baseline * (1 + .data$minutes_bump / .data$baseline_minutes),
                       .data$baseline)
    ) %>%
    filter(abs(.data$bump) > 0.01)          # only rows the model claims to move

  if (!nrow(ev)) { if (!quiet) warn("no testable rows"); return(invisible(NULL)) }

  # A raw RMSE difference on ~7000 rows can easily be noise. Bootstrap the
  # difference so a tiny "improvement" cannot be reported as a finding.
  boot_gain <- function(actual, base_pred, alt_pred, reps = 1000) {
    eb <- (actual - base_pred)^2; ea <- (actual - alt_pred)^2
    d <- replicate(reps, {
      i <- sample(length(eb), length(eb), replace = TRUE)
      sqrt(mean(eb[i])) - sqrt(mean(ea[i]))
    })
    ci <- unname(quantile(d, c(0.025, 0.975)))
    list(rmse = sqrt(mean(ea)), gain = sqrt(mean(eb)) - sqrt(mean(ea)),
         lo = ci[1], hi = ci[2])
  }

  res <- map_dfr(split(ev, ev$stat), function(d) {
    rb <- rmse(d$actual, d$baseline)
    dir <- boot_gain(d$actual, d$baseline, d$direct)
    rou <- boot_gain(d$actual, d$baseline, d$routed)
    tibble(stat = d$stat[1], n = nrow(d), rmse_baseline = rb,
           rmse_direct = dir$rmse, gain_direct = dir$gain,
           direct_lo = dir$lo, direct_hi = dir$hi, direct_real = dir$lo > 0,
           rmse_routed = rou$rmse, gain_routed = rou$gain,
           routed_lo = rou$lo, routed_hi = rou$hi, routed_real = rou$lo > 0)
  })

  if (quiet) return(invisible(res))

  message("   ", sprintf("%-10s %6s  %8s  %-24s  %-24s", "stat", "n",
                         "baseline", "direct (add delta)", "routed (via minutes)"))
  for (i in seq_len(nrow(res))) {
    r <- res[i, ]
    fmt <- function(rmse, gain, lo, real)
      sprintf("%6.3f %+5.2f%% [%+.3f]%s", rmse, 100 * gain / r$rmse_baseline, lo,
              if (real) "*" else " ")
    message(sprintf("   %-10s %6d  %8.3f  %-24s  %-24s", r$stat, r$n, r$rmse_baseline,
                    fmt(r$rmse_direct, r$gain_direct, r$direct_lo, r$direct_real),
                    fmt(r$rmse_routed, r$gain_routed, r$routed_lo, r$routed_real)))
  }
  message("   * = 95% bootstrap CI on the RMSE gain excludes zero (lower bound shown)")

  win_d <- res$stat[res$direct_real]; win_r <- res$stat[res$routed_real]
  message("")
  if (!length(win_d) && !length(win_r)) {
    message("   Neither scheme reliably beats the plain baseline. Do not bet this.\n",
            "   The likeliest reading: the absence is already reflected in the\n",
            "   player's own recent form, so the adjustment adds variance and\n",
            "   nothing else.")
  } else {
    if (length(win_d)) message("   direct helps: ", paste(win_d, collapse = ", "))
    if (length(win_r)) message("   routed helps: ", paste(win_r, collapse = ", "))
    hurt <- res$stat[res$direct_hi < 0]
    if (length(hurt))
      message("   direct is reliably WORSE than baseline for: ",
              paste(hurt, collapse = ", "), " -- do not apply it to those.")
    message("\n   This is prediction accuracy, NOT market edge. Edge needs a prop line\n",
            "   to beat, and the free data tier does not carry one.")
  }

  # --- does it replicate? --------------------------------------------------
  # A pooled result across seasons can be carried entirely by one of them. That
  # is how a +2% gain in one season and a -1% loss in the next get reported as
  # a win. Re-run the same test within each season and say plainly whether the
  # pooled winners hold up; a finding that appears in one season and vanishes
  # in the next was probably never there.
  seasons <- sort(unique(pg$season))
  if (replicate_by_season && length(seasons) > 1 && (length(win_d) || length(win_r))) {
    message("")
    step("Replication by season")
    per <- map_dfr(seasons, function(s) {
      r <- suppressMessages(
        validate_usage_model(pg %>% filter(.data$season == !!s), split_frac = split_frac,
                             stats = stats, quiet = TRUE, replicate_by_season = FALSE))
      if (is.null(r) || !nrow(r)) return(NULL)
      r$season <- s
      r
    })
    if (!nrow(per)) {
      warn("not enough data to re-test within seasons")
    } else {
      for (st in union(win_d, win_r)) {
        rows <- per %>% filter(.data$stat == st)
        if (!nrow(rows)) next
        held <- sum(rows$routed_real | rows$direct_real)
        message(sprintf("   %-10s pooled says it helps; holds in %d of %d season(s): %s",
                        st, held, nrow(rows),
                        paste(sprintf("%d %+.2f%%", rows$season,
                                      100 * rows$gain_routed / rows$rmse_baseline),
                              collapse = "  ")))
        if (held < nrow(rows))
          warn("  '", st, "' does not replicate in every season. Treat the pooled ",
               "result as unproven -- it is an average over seasons that disagree.")
      }
    }
  }

  invisible(res)
}

# ===========================================================================
# 5. Into the track record
# ===========================================================================
# Uses the player/stat columns that were in the schema from the start. `line`
# and `odds` stay blank unless you supply them -- see the note in the header.

props_to_track_rows <- function(proj, lines = NULL, ts = Sys.time()) {
  if (!nrow(proj)) return(tibble())
  stamp <- format(as.POSIXct(ts), "%Y-%m-%d %H:%M:%S %Z")
  rows <- proj %>% transmute(
    .data$date,
    game = .data$team,                       # opponent unknown here; team is the anchor
    player = .data$player, .data$stat,
    prediction = round(.data$projection, 2),
    # Same convention as the game markets in 05: prediction_raw is the number
    # before the availability signal moved it, news_adj is the movement. That
    # is what lets evaluate_news_contribution() ask whether reacting to the
    # absence beat ignoring it, on the same projections.
    prediction_raw = round(.data$baseline, 2),
    news_adj       = round(.data$bump, 2),
    line = NA_real_, odds = NA_real_, bet = NA_character_,
    result = NA_real_, win_loss = NA_character_, units = NA_real_,
    timestamp = stamp, market = "prop", edge = NA_real_,
    model_version = "usage-v1", event_id = NA_character_,
    line_close = NA_real_, clv_points = NA_real_,
    notes = paste0("usage bump ", sprintf("%+.2f", .data$bump), " from ", .data$absent)
  )

  if (!is.null(lines) && nrow(lines)) {
    rows <- rows %>%
      left_join(lines %>% select("player", "stat", man_line = "line", man_odds = "odds"),
                by = c("player", "stat")) %>%
      mutate(line = coalesce(.data$man_line, .data$line),
             odds = coalesce(.data$man_odds, .data$odds),
             bet = if_else(!is.na(.data$line),
                           if_else(.data$prediction > .data$line, "over", "under"), NA_character_),
             edge = if_else(!is.na(.data$line), .data$prediction - .data$line, NA_real_)) %>%
      select(-"man_line", -"man_odds")
  }
  rows
}

# The whole chain in one call: read the news, decide who is out, work out who
# benefits, and produce tonight's projections.
#
# The look-ahead gate is not optional here and is not a formality. Articles
# carry a published timestamp, and anything published after `cutoff` is
# information you could not have acted on. It defaults to now, which is correct
# for live use; pass an explicit cutoff only when reconstructing a past evening,
# and treat the result as illustrative rather than as a backtest -- see the
# HONESTY WARNING at the top of 06_news_signals.R for why.
project_from_news <- function(pg, transfer = NULL, baselines = NULL,
                              as_of = Sys.Date(), cutoff = Sys.time(),
                              min_confidence = CFG$news$min_confidence,
                              limit = 50, method = c("routed", "direct")) {
  method <- match.arg(method)
  gz <- player_gazetteer(pg)

  raw <- fetch_news(limit)
  if (!NROW(raw)) { warn("no news returned"); return(tibble()) }

  absences <- news_before(raw, cutoff) %>%
    extract_player_status(gazetteer = gz) %>%
    news_absences(min_confidence = min_confidence)
  ids <- absent_ids_from_news(absences, gz)
  if (!length(ids)) { info("nobody flagged absent at this confidence"); return(tibble()) }

  if (is.null(transfer)) {
    path <- file.path(CFG$paths$processed_dir, "usage_transfer.rds")
    if (!file.exists(path))
      stop("No usage_transfer.rds. Run estimate_usage_transfer(pg) and save it, ",
           "and run validate_usage_model(pg) BEFORE trusting the output.",
           call. = FALSE)
    transfer <- readRDS(path)
  }
  if (is.null(baselines)) baselines <- player_baselines(pg, as_of = as_of)

  project_props(ids, transfer, baselines, as_of = as_of, method = method)
}

# Append projections to the same auditable log the game bets go in.
#
# `line` and `odds` stay blank unless you pass them: the free odds tier carries
# no player props, so there is usually no market number to bet into. A row with
# no odds is a timestamped PROJECTION, not a bet -- summarise_bets_simple()
# excludes it from ROI for exactly that reason, while still keeping it on the
# record so the projection can be graded for accuracy later.
log_props <- function(proj, lines = NULL, dry_run = FALSE, ts = Sys.time()) {
  rows <- props_to_track_rows(proj, lines = lines, ts = ts)
  if (!NROW(rows)) { info("no projections to log"); return(invisible(NULL)) }

  n_priced <- sum(!is.na(rows$odds))
  info(nrow(rows), " projection(s), ", n_priced, " with a price attached",
       if (!n_priced) "  (projections only -- they will not count toward ROI)" else "")
  print(rows %>% select("date", "player", "stat", "prediction",
                        "prediction_raw", "news_adj", "line", "bet"))
  append_track_rows(rows, dry_run = dry_run)
}

# ---------------------------------------------------------------------------
step("07_usage_model.R loaded")
info("pg <- load_player_games()          box scores, absences derived from minutes")
info("tr <- estimate_usage_transfer(pg)  A out -> B's line moves by delta")
info("validate_usage_model(pg)           does it beat a plain baseline? (do this first)")
info("project_props(ids, tr, bl)         tonight's projections")
info("project_from_news(pg)              news -> who is out -> projections, in one call")
info("log_props(proj)                    append them to the track record")
warn("Validate before you project. An unvalidated usage bump is a guess with decimals.")
