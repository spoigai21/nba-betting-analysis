# ---------------------------------------------------------------------------
# 04_backtest.R -- did disagreeing with the line actually make money?
# ---------------------------------------------------------------------------
# Rules of the simulation, all of them deliberately unflattering:
#
#  * Flat stakes. Every bet is exactly 1 unit. No "confidence" sizing.
#  * Real prices. Profit is computed from the actual American odds, so the
#    sportsbook's margin is paid on every single bet. Break-even at -110 is
#    52.38%, not 50%.
#  * Bet the line that existed when the prediction was made. If the dataset has
#    opening lines, bets are placed at the OPEN and graded against the CLOSE for
#    closing-line value. If it only has closing lines, we say so.
#  * Out-of-sample only. Every prediction here came from a model that had never
#    seen the season it is betting.
#  * A push is a push -- stake returned, not a win.
#
# Output: printed report, output/figures/*.png, output/backtest_bets.csv
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")
if (!exists("preds")) preds <- readr::read_csv(CFG$paths$preds_csv, show_col_types = FALSE)

TH_TOTAL  <- CFG$backtest$total_edge_threshold
TH_SPREAD <- CFG$backtest$spread_edge_threshold
TH_ML     <- CFG$backtest$ml_ev_threshold
ML_RANGE  <- CFG$backtest$ml_prob_range
ML_SHRINK <- CFG$backtest$ml_shrink_to_market
ML_PERR   <- CFG$backtest$ml_prob_error
# Derived, not chosen: the longest price at which an EV estimate built on a
# probability this uncertain still says more than the uncertainty does.
ML_MAX_DEC <- CFG$backtest$ml_max_ev_error / CFG$backtest$ml_prob_error
DEF_PRICE <- CFG$backtest$default_price

# ===========================================================================
# 1. Which line were we actually able to bet?
# ===========================================================================

choose_line_source <- function(preds) {
  have_open <- mean(!is.na(preds$total_open) | !is.na(preds$spread_open))
  if (have_open > 0.5) {
    info("opening lines available for ", scales::percent(have_open, 1),
         " of games -- betting the OPEN, grading CLV against the CLOSE")
    "open"
  } else {
    warn("no opening lines in this dataset -- betting the CLOSING line. ",
         "This is the pessimistic assumption (the close is the sharpest number), ",
         "and closing-line value cannot be measured from history.")
    "close"
  }
}

# ===========================================================================
# 2. Turn model disagreement into bets
# ===========================================================================

# The numbers we were actually able to bet. Shared by build_bets() and the
# figures, so a chart can never plot a disagreement that differs from the one
# the bet was placed on.
taken_lines <- function(preds, line_source = "open") {
  take <- function(open_col, close_col) {
    if (line_source == "open") coalesce(preds[[open_col]], preds[[close_col]])
    else preds[[close_col]]
  }
  tibble(total  = take("total_open",  "total_close"),
         spread = take("spread_open", "spread_close"))
}

build_bets <- function(preds, line_source = "open") {
  tk <- taken_lines(preds, line_source)
  total_taken  <- tk$total
  spread_taken <- tk$spread

  # --- totals -------------------------------------------------------------
  totals <- preds %>%
    mutate(line_taken = total_taken,
           edge = .data$pred_total - .data$line_taken,
           side = case_when(.data$edge >=  TH_TOTAL ~ "over",
                            .data$edge <= -TH_TOTAL ~ "under",
                            TRUE ~ NA_character_)) %>%
    filter(!is.na(.data$side), !is.na(.data$line_taken), !is.na(.data$total_points)) %>%
    transmute(
      .data$game_id, .data$date, .data$season, .data$home_team, .data$away_team,
      market = "total", .data$side, .data$line_taken,
      line_close = .data$total_close,
      price = coalesce(if_else(.data$side == "over", .data$price_over, .data$price_under),
                       DEF_PRICE),
      # The other side's price, so the placebo control can flip side AND price.
      price_alt = coalesce(if_else(.data$side == "over", .data$price_under, .data$price_over),
                           DEF_PRICE),
      .data$edge,
      ev = NA_real_,          # point markets carry no probability estimate
      result_value = .data$total_points,
      outcome = settle_total(.data$total_points, .data$line_taken, .data$side),
      clv_points = clv_of("total", .data$side, .data$line_taken, .data$line_close)
    )

  # --- spreads ------------------------------------------------------------
  spreads <- preds %>%
    mutate(line_taken = spread_taken,
           # model_spread is the model's own home-side line
           edge = .data$line_taken - .data$model_spread,
           side = case_when(.data$edge >=  TH_SPREAD ~ "home",
                            .data$edge <= -TH_SPREAD ~ "away",
                            TRUE ~ NA_character_)) %>%
    filter(!is.na(.data$side), !is.na(.data$line_taken), !is.na(.data$margin)) %>%
    transmute(
      .data$game_id, .data$date, .data$season, .data$home_team, .data$away_team,
      market = "spread", .data$side, .data$line_taken,
      line_close = .data$spread_close,
      price = coalesce(if_else(.data$side == "home",
                               .data$price_spread_home, .data$price_spread_away),
                       DEF_PRICE),
      price_alt = coalesce(if_else(.data$side == "home",
                                   .data$price_spread_away, .data$price_spread_home),
                           DEF_PRICE),
      .data$edge,
      ev = NA_real_,
      result_value = .data$margin,
      outcome = settle_spread(.data$margin, .data$line_taken, .data$side),
      clv_points = clv_of("spread", .data$side, .data$line_taken, .data$line_close)
    )

  # --- moneyline ----------------------------------------------------------
  # The only market where the model's output is already a probability, so it is
  # the only one where "edge" can be stated honestly in the units that matter.
  #
  # Three things make this different from the point markets above:
  #   1. There is no number to disagree with, only a price. The comparison is
  #      against the market's DE-VIGGED probability, not its raw implied one --
  #      raw implied probabilities sum to ~1.05 and would hand us a fake 5%.
  #   2. Selection is on EXPECTED VALUE at the price actually offered. A 3-point
  #      probability edge is worth ~0.06 units on a coin flip and ~0.01 on a
  #      -500 favourite; a probability threshold cannot tell those apart.
  #   3. Public datasets carry only CLOSING moneylines, so these bets are struck
  #      at the close -- the sharpest number of the day, and the least flattering
  #      assumption available. CLV is therefore unmeasurable here, not zero.
  #
  # The naive version of this market is a trap, and it is worth being explicit
  # about why. EV is dec times as sensitive to an error in p as it is to the
  # size of the edge, so on a +1500 underdog a three-point probability error
  # reads as a forty-five-point edge. Feed a barely-better-than-base-rate win
  # model into an unguarded EV rule and it will bet every longshot on the board
  # and report a 30% expected return. Two guards stop that, both sized from
  # measurements that 08_diagnostics.R prints for your own data:
  #
  #   SHRINK   believe only part of the disagreement with the market, because
  #            the market's de-vigged probabilities are measurably the more
  #            accurate of the two estimates.
  #   HAIRCUT  then require the bet to survive the model's typical calibration
  #            error, applied against whichever side is being considered.
  #
  # Together they make longshots effectively unbettable and leave only large,
  # low-leverage disagreements -- which is the correct posture for a model that
  # cannot demonstrate it is sharper than the price it is betting into.
  # The selection rule itself lives in utils.R::moneyline_pick(), because
  # 05_forward.R has to choose live bets by exactly this rule for the forward
  # test to be a test of this backtest.
  mlp <- moneyline_pick(preds$pred_home_win_prob, preds$ml_home, preds$ml_away)
  moneyline <- preds %>%
    mutate(ml_side = mlp$side, ml_edge = mlp$edge, ml_ev = mlp$ev,
           ml_price = mlp$price, ml_price_alt = mlp$price_alt) %>%
    filter(!is.na(.data$ml_side), !is.na(.data$margin)) %>%
    transmute(
      .data$game_id, .data$date, .data$season, .data$home_team, .data$away_team,
      market = "moneyline", side = .data$ml_side,
      line_taken = NA_real_,           # a moneyline has no line, only a price
      line_close = NA_real_,
      price      = .data$ml_price,
      price_alt  = .data$ml_price_alt,
      # `edge` is the raw disagreement, reported for interpretation. `ev` is the
      # post-shrink, post-haircut number the bet was actually selected on, and
      # is deliberately the smaller of the two.
      edge = .data$ml_edge,
      ev   = .data$ml_ev,
      result_value = .data$margin,
      outcome = settle_moneyline(.data$margin, .data$ml_side),
      clv_points = NA_real_            # no opening moneyline to have beaten
    )

  bind_rows(totals, spreads, moneyline) %>%
    mutate(
      # When we bet the close, the number taken IS the close, so clv_points is
      # identically zero. That would read as "neutral CLV" when the truth is
      # "CLV was never measured" -- so record it as missing, not as zero.
      clv_points = if (line_source == "close") NA_real_ else .data$clv_points,
      units = bet_units(.data$price, .data$outcome, CFG$backtest$unit),
      beat_close = case_when(is.na(.data$clv_points) ~ NA,
                             .data$clv_points > 0 ~ TRUE,
                             .data$clv_points < 0 ~ FALSE,
                             TRUE ~ NA)
    ) %>%
    arrange(.data$date, .data$game_id)
}

# ===========================================================================
# 3. Metrics
# ===========================================================================

summarise_bets <- function(b, label = "all") {
  n <- nrow(b)
  if (n == 0) return(tibble(segment = label, n = 0L))
  graded <- b %>% filter(.data$outcome %in% c("win", "loss"))
  wins <- sum(graded$outcome == "win")
  wr <- if (nrow(graded)) wins / nrow(graded) else NA_real_
  ci_wr <- wilson_ci(wins, nrow(graded))
  roi <- sum(b$units, na.rm = TRUE) / (n * CFG$backtest$unit)
  ci_roi <- bootstrap_ci(b$units, mean, CFG$backtest$bootstrap_reps)

  tibble(
    segment    = label,
    n          = n,
    wins       = wins,
    losses     = sum(graded$outcome == "loss"),
    pushes     = sum(b$outcome == "push", na.rm = TRUE),
    win_rate   = wr,
    wr_lo      = ci_wr[["lower"]],
    wr_hi      = ci_wr[["upper"]],
    break_even = mean(break_even_prob(b$price), na.rm = TRUE),
    units      = sum(b$units, na.rm = TRUE),
    roi        = roi,
    roi_lo     = ci_roi[["lower"]],
    roi_hi     = ci_roi[["upper"]],
    clv_mean   = mean(b$clv_points, na.rm = TRUE),
    beat_close = mean(b$beat_close, na.rm = TRUE)
  )
}

print_summary <- function(s) {
  for (i in seq_len(nrow(s))) {
    r <- s[i, ]
    if (r$n == 0) { message(sprintf("   %-22s no bets", r$segment)); next }
    message(sprintf(
      "   %-22s n=%4d  win %5.1f%% [%.1f-%.1f]  b/e %5.1f%%  units %+7.2f  ROI %+6.2f%% [%+.1f%%, %+.1f%%]",
      r$segment, r$n, 100 * r$win_rate, 100 * r$wr_lo, 100 * r$wr_hi,
      100 * r$break_even, r$units, 100 * r$roi, 100 * r$roi_lo, 100 * r$roi_hi))
  }
}

# ===========================================================================
# 4. Controls -- what does "no skill" look like on this same sample?
# ===========================================================================
# Betting a random side on the same games should lose roughly the vig. If the
# strategy's result sits inside the placebo's spread, it is noise.

placebo_roi <- function(b, reps = 400) {
  if (!nrow(b)) return(c(lower = NA_real_, upper = NA_real_))

  other_side <- ifelse(b$market == "total",
                       ifelse(b$side == "over", "under", "over"),
                       ifelse(b$side == "home", "away", "home"))
  # Flipping the side has to flip the PRICE with it. On a -110/-110 spread that
  # barely registers; on a moneyline it is the entire difference between backing
  # a -500 favourite and a +400 underdog, and holding the price fixed would make
  # the control compare our bets against something no bettor could have placed.
  other_price <- coalesce(b$price_alt, b$price)

  settle_any <- function(side)
    ifelse(b$market == "total",  settle_total(b$result_value, b$line_taken, side),
    ifelse(b$market == "spread", settle_spread(b$result_value, b$line_taken, side),
                                 settle_moneyline(b$result_value, side)))

  flip <- function() {
    swap  <- runif(nrow(b)) < 0.5
    side  <- ifelse(swap, other_side,  b$side)
    price <- ifelse(swap, other_price, b$price)
    mean(bet_units(price, settle_any(side)), na.rm = TRUE)
  }
  setNames(unname(quantile(replicate(reps, flip()), c(0.025, 0.975))),
           c("lower", "upper"))
}

# ===========================================================================
# 5. Figures
# ===========================================================================

make_figures <- function(b, preds, line_source = "open") {
  if (nrow(b)) {
    cum <- b %>% arrange(.data$date) %>% group_by(.data$market) %>%
      mutate(bet_no = row_number(), cum_units = cumsum(coalesce(.data$units, 0))) %>%
      ungroup()

    p1 <- ggplot(cum, aes(.data$date, .data$cum_units, colour = .data$market)) +
      geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
      geom_line(linewidth = 0.8) +
      labs(title = "Cumulative profit, flat 1-unit bets",
           subtitle = "Out-of-sample walk-forward backtest, prices included",
           x = NULL, y = "units", colour = "market",
           caption = "A random bettor drifts down at roughly the vig. Ending above zero is only meaningful with a large sample.") +
      theme_project()
    save_fig(p1, "01_cumulative_units.png")

    # Points-denominated markets only. Moneyline "edge" is a probability and
    # would be squashed into the first bucket, reading as a real result when it
    # is a units mismatch -- it gets its own figure below.
    p2 <- b %>%
      filter(.data$market != "moneyline") %>%
      mutate(bucket = cut(abs(.data$edge),
                          breaks = c(0, 2, 3, 4, 6, 8, Inf),
                          labels = c("0-2", "2-3", "3-4", "4-6", "6-8", "8+"),
                          include.lowest = TRUE)) %>%
      group_by(.data$market, .data$bucket) %>%
      summarise(n = n(), roi = mean(.data$units, na.rm = TRUE), .groups = "drop") %>%
      filter(.data$n >= 20)
    if (nrow(p2)) {
      p2plot <- ggplot(p2, aes(.data$bucket, .data$roi, fill = .data$market)) +
        geom_hline(yintercept = 0, colour = "grey40") +
        geom_col(position = position_dodge(0.8), width = 0.7) +
        geom_text(aes(label = paste0("n=", .data$n)),
                  position = position_dodge(0.8), vjust = -0.4, size = 3, colour = "grey30") +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "ROI by size of disagreement with the line",
             subtitle = "If the model has an edge, bigger disagreements should pay better",
             x = "model edge over the line (points)", y = "ROI", fill = "market") +
        theme_project()
      save_fig(p2plot, "02_roi_by_edge.png")
    }
  }

  # Calibration of the home-win probability model.
  cal <- preds %>%
    filter(!is.na(.data$pred_home_win_prob), !is.na(.data$home_win)) %>%
    mutate(bin = cut(.data$pred_home_win_prob, breaks = seq(0, 1, 0.1),
                     include.lowest = TRUE)) %>%
    group_by(.data$bin) %>%
    summarise(pred = mean(.data$pred_home_win_prob),
              actual = mean(.data$home_win), n = n(), .groups = "drop") %>%
    filter(.data$n >= 20)
  if (nrow(cal)) {
    p3 <- ggplot(cal, aes(.data$pred, .data$actual)) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
      geom_point(aes(size = .data$n)) +
      geom_line() +
      scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(title = "Calibration: when the model says 60%, does it happen 60% of the time?",
           subtitle = "Out-of-sample home-win probabilities, decile bins",
           x = "predicted", y = "observed", size = "games") +
      theme_project()
    save_fig(p3, "03_calibration.png", width = 7, height = 6)
  }

  # Model vs market disagreement -- shows how often we even have an opinion.
  # Measured against the line we could actually BET (the open, when the dataset
  # has one), not the close. preds$edge_* are close-based and would put the
  # dashed betting thresholds in the wrong place relative to the histogram.
  tk <- taken_lines(preds, line_source)
  dis <- tibble(total  = preds$pred_total - tk$total,
                spread = tk$spread - preds$model_spread) %>%
    pivot_longer(everything(), names_to = "market", values_to = "edge") %>%
    filter(!is.na(.data$edge))
  if (nrow(dis)) {
    p4 <- ggplot(dis, aes(.data$edge)) +
      geom_histogram(bins = 60, fill = "grey35") +
      geom_vline(xintercept = c(-TH_TOTAL, TH_TOTAL), linetype = 2, colour = "firebrick") +
      facet_wrap(~ .data$market, scales = "free") +
      labs(title = "How far the model sits from the market",
           subtitle = paste0("Dashed lines = betting thresholds (",
                             TH_TOTAL, " pts totals, ", TH_SPREAD, " pts spreads)"),
           x = "model minus line (points)", y = "games") +
      theme_project()
    save_fig(p4, "04_edge_distribution.png")
  }

  if (nrow(b) && any(!is.na(b$clv_points))) {
    p5 <- ggplot(b %>% filter(!is.na(.data$clv_points)),
                 aes(.data$clv_points, fill = .data$market)) +
      geom_vline(xintercept = 0, colour = "grey40") +
      geom_histogram(bins = 40, alpha = 0.8, position = "identity") +
      labs(title = "Closing-line value",
           subtitle = "Points better (right) or worse (left) than the number the market closed at",
           x = "CLV (points)", y = "bets", fill = "market",
           caption = "Consistently positive CLV is the strongest available evidence of real edge.") +
      theme_project()
    save_fig(p5, "05_clv.png")
  }

  # Moneyline: did the claimed edge actually show up?
  # Every moneyline bet carries an explicit prediction -- "this returns +6% per
  # unit" -- so unlike the point markets we can plot claimed against realised
  # and see whether the probabilities mean anything. Points on the dashed line
  # mean the model's EV estimates are honest; points below it mean the model is
  # overconfident, which is the normal failure mode.
  mlb <- b %>% filter(.data$market == "moneyline", !is.na(.data$ev))
  if (nrow(mlb) >= 40) {
    evb <- mlb %>%
      mutate(bucket = cut(.data$ev, breaks = c(-Inf, 0.04, 0.07, 0.12, Inf),
                          labels = c("2-4%", "4-7%", "7-12%", "12%+"))) %>%
      group_by(.data$bucket) %>%
      summarise(n = n(), realised = mean(.data$units, na.rm = TRUE),
                claimed = mean(.data$ev), .groups = "drop") %>%
      filter(.data$n >= 20)
    if (nrow(evb)) {
      p6 <- ggplot(evb, aes(.data$claimed, .data$realised)) +
        geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
        geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
        geom_line(colour = "grey35") +
        geom_point(aes(size = .data$n)) +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Moneyline: claimed edge vs realised return",
             subtitle = "Dashed line = the model's EV estimates coming true exactly",
             x = "expected value claimed at bet time", y = "realised ROI",
             size = "bets",
             caption = "Below the dashed line = overconfident probabilities. This is the usual result.") +
        theme_project()
      save_fig(p6, "06_moneyline_ev.png", width = 7, height = 6)
    }
  }
}

# ===========================================================================
# 6. Run
# ===========================================================================

line_source <- choose_line_source(preds)
bets <- build_bets(preds, line_source)

step("Backtest summary")
info("bet at the ", toupper(line_source), " line; thresholds: totals ",
     TH_TOTAL, " pts, spreads ", TH_SPREAD, " pts, moneyline ",
     scales::percent(TH_ML, 0.1), " EV")
info("moneyline guards: believe ", scales::percent(ML_SHRINK, 1),
     " of the disagreement, haircut ", scales::percent(ML_PERR, 0.1),
     ", model prob in [", ML_RANGE[1], ", ", ML_RANGE[2], "], price no longer than ",
     sprintf("%+d", round(prob_to_american(1 / ML_MAX_DEC))))

n_ml_avail <- sum(!is.na(preds$ml_home) & !is.na(preds$ml_away))
if (n_ml_avail) {
  info("moneyline is struck at the CLOSING price (datasets carry no opening ",
       "moneyline), so it is the least flattering of the three and its CLV ",
       "cannot be measured")
} else {
  warn("no moneyline prices in this dataset -- that market is skipped")
}

n_avail <- nrow(preds) * 2 + n_ml_avail
info("bets placed on ", nrow(bets), " of ", n_avail,
     " available game-markets (", scales::percent(nrow(bets) / n_avail, 0.1), ")")

summary_all <- bind_rows(
  summarise_bets(bets, "ALL"),
  summarise_bets(bets %>% filter(.data$market == "total"),     "totals"),
  summarise_bets(bets %>% filter(.data$market == "spread"),    "spreads"),
  summarise_bets(bets %>% filter(.data$market == "moneyline"), "moneyline")
)
print_summary(summary_all)

step("By season")
by_season <- bets %>% group_split(.data$season) %>%
  map_dfr(~ summarise_bets(.x, paste("season", .x$season[1])))
print_summary(by_season)

step("Placebo control (random side, same games, same prices)")
pl <- placebo_roi(bets)
info("a no-skill bettor lands between ", sprintf("%+.2f%%", 100 * pl[["lower"]]),
     " and ", sprintf("%+.2f%%", 100 * pl[["upper"]]), " ROI on this sample")

step("Closing-line value")
if (any(!is.na(bets$clv_points))) {
  n_clv <- sum(!is.na(bets$clv_points))
  info("measurable on ", n_clv, " of ", nrow(bets), " bets",
       if (n_clv < nrow(bets))
         " (moneyline is struck at the close, so it has no CLV to measure)" else "")
  info("mean CLV: ", sprintf("%+.2f", mean(bets$clv_points, na.rm = TRUE)), " points")
  info("beat the closing number on ",
       scales::percent(mean(bets$beat_close, na.rm = TRUE), 0.1), " of those bets")
  info("(50% is coin-flip; sustained CLV above ~53% is the real signal)")
} else {
  warn("CLV unavailable -- this dataset has no opening lines to bet into.")
}

make_figures(bets, preds, line_source)
readr::write_csv(bets, file.path(CFG$paths$output_dir, "backtest_bets.csv"))
readr::write_csv(summary_all, file.path(CFG$paths$output_dir, "backtest_summary.csv"))

# ===========================================================================
# 7. The honest verdict
# ===========================================================================

step("Verdict")
a <- summary_all %>% filter(.data$segment == "ALL")
beat <- mean(bets$beat_close, na.rm = TRUE)
have_clv <- !is.na(beat)
roi_ci <- sprintf("%+.1f%%, %+.1f%%", 100 * a$roi_lo, 100 * a$roi_hi)

if (a$n < CFG$backtest$min_bets_for_conclusion) {
  message("   Sample too small to conclude anything. ", a$n, " bets; ",
          CFG$backtest$min_bets_for_conclusion, "+ needed.\n",
          "   Whatever the ROI says here, it is noise.")

} else if (a$roi_lo > 0 && have_clv && beat > 0.53) {
  message("   ROI is positive with a 95% interval excluding zero [", roi_ci, "], and\n",
          "   the bets beat the closing number ", sprintf("%.0f%%", 100 * beat),
          " of the time. That is the pattern a\n",
          "   real edge makes. Before believing it: re-read 02_features.R for\n",
          "   look-ahead, and confirm on FORWARD-tested bets (05_forward.R) --\n",
          "   backtests flatter models.")

} else if (a$roi_lo > 0) {
  message("   ROI is positive with an interval excluding zero [", roi_ci, "], but\n",
          "   closing-line value is not there to support it. Profit without CLV is\n",
          "   usually variance. Treat as unproven; forward-test it.")

} else if (have_clv && beat > 0.53) {
  # The informative middle case: the numbers we bet were better than the numbers
  # the market settled on, but not by enough (yet) to show up in profit.
  message("   Mixed, and worth reading carefully.\n",
          "   ROI is inconclusive -- the interval [", roi_ci, "] includes zero, so\n",
          "   this sample does not demonstrate profit.\n",
          "   But the bets beat the closing number ", sprintf("%.0f%%", 100 * beat),
          " of the time (", sprintf("%+.2f", mean(bets$clv_points, na.rm = TRUE)),
          " pts avg).\n",
          "   CLV is the leading indicator: it shows up in far fewer bets than ROI\n",
          "   does, because it is measured against a number rather than a coin flip.\n",
          "   Verdict: promising, unproven. Keep the rules fixed and collect more.")

} else if (a$roi_hi < 0) {
  # Not merely unproven -- reliably losing. Worth saying plainly, and worth
  # comparing against the placebo: a strategy that loses at the same rate as
  # betting at random has no information in it at all, as opposed to having
  # information too small to overcome the vig.
  like_placebo <- !is.na(pl[["lower"]]) && a$roi >= pl[["lower"]] && a$roi <= pl[["upper"]]
  message("   No edge, and the interval [", roi_ci, "] sits entirely BELOW zero:\n",
          "   this strategy loses reliably, not inconclusively.\n",
          if (like_placebo)
            paste0("   Its ROI also falls inside the placebo range (",
                   sprintf("%+.2f%% to %+.2f%%", 100 * pl[["lower"]], 100 * pl[["upper"]]),
                   "), so it is\n   indistinguishable from picking a side at random and paying the vig.\n")
          else "",
          "   That is a real finding, not a failure. It says the model is not\n",
          "   adding information the market lacks -- see the encompassing test in\n",
          "   08_diagnostics.R, which measures that directly.")
} else {
  message("   No demonstrated edge. The ROI interval [", roi_ci, "] includes zero",
          if (have_clv) sprintf(",\n   and CLV is neutral (beat the close %.0f%% of the time).",
                                100 * beat) else ".",
          "\n   This is the expected result against an efficient market, and it is a\n",
          "   real finding -- not a failure. The market prices these games well.")
}
message("")
