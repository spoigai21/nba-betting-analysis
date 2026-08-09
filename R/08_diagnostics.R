# ---------------------------------------------------------------------------
# 08_diagnostics.R -- where does the model fail, and is the market wrong there?
# ---------------------------------------------------------------------------
# This is Phase 5: the human-in-the-loop step. Its job is to produce HYPOTHESES
# worth testing, not conclusions. Nothing here changes the model.
#
# THE TRAP THIS FILE IS BUILT TO AVOID
#   Slice a backtest twenty ways and two slices will look profitable at the 5%
#   level even if the model is worthless -- that is what "5% level" means. Any
#   file that prints "ROI by segment" and stops has manufactured a finding.
#   Three defences, in order of importance:
#
#   1. LEAD WITH BIAS, NOT WITH ROI. Whether the market's number is
#      systematically wrong in a segment is a far more stable question than
#      whether we happened to profit there. Bias is measured on every game in
#      the segment; ROI is measured only on the handful we chose to bet, and is
#      a coin flip on top of that. Bias moves the model; ROI mostly moves noise.
#   2. TEST THE MARKET, NOT OURSELVES. "Our model is biased here" is a bug
#      report. "The MARKET is biased here" is the only kind of finding that can
#      become an edge -- and it is the rarer one, because the market is good.
#   3. COUNT THE COMPARISONS OUT LOUD. Every table below reports how many tests
#      it ran, how many hits chance alone predicts, and Holm-adjusted p-values
#      alongside the raw ones. A raw p is a hypothesis; an adjusted p is closer
#      to a finding.
#
# Read the output as a to-do list of things to go and check, and remember that
# the honest prior on every one of them is "the market already knows".
#
# Run:  source("R/08_diagnostics.R")
# Output: printed tables, output/figures/07_*.png, 08_*.png,
#         output/diagnostics_segments.csv
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")

MIN_N <- CFG$diagnostics$min_segment_n
ALPHA <- CFG$diagnostics$alpha

if (!exists("preds"))
  preds <- readr::read_csv(CFG$paths$preds_csv, show_col_types = FALSE)
if (!exists("model_data")) model_data <- readRDS(CFG$paths$model_data_rds)

# ===========================================================================
# 1. Segment definitions
# ===========================================================================
# Deliberately a short, pre-declared list. Adding segments until something goes
# significant is the exact failure this file warns about, so the dimensions are
# fixed here and the count of them is reported with every result.

build_segments <- function(preds, md) {
  join_cols <- intersect(
    c("game_id", "h_b2b", "a_b2b", "h_rest_days", "a_rest_days",
      "h_games_last_7", "a_games_last_7"), names(md))

  preds %>%
    left_join(md %>% select(all_of(join_cols)), by = "game_id") %>%
    mutate(
      month = factor(month.abb[lubridate::month(.data$date)], levels = month.abb),
      season_f = factor(.data$season),
      # How the market saw the game, not how we did -- so the segment itself
      # can never be a function of the model being tested.
      home_role = case_when(
        is.na(.data$spread_close)   ~ NA_character_,
        .data$spread_close <= -8    ~ "home heavy fav",
        .data$spread_close <   -3   ~ "home fav",
        .data$spread_close <=   3   ~ "close game",
        .data$spread_close <    8   ~ "home dog",
        TRUE                        ~ "home heavy dog"),
      total_band = cut(.data$total_close, breaks = c(-Inf, 215, 225, 235, Inf),
                       labels = c("under 215", "215-225", "225-235", "over 235")),
      rest_state = case_when(
        is.na(.data$h_b2b) | is.na(.data$a_b2b) ~ NA_character_,
        .data$h_b2b == 1 & .data$a_b2b == 1 ~ "both on a b2b",
        .data$h_b2b == 1 ~ "home on a b2b",
        .data$a_b2b == 1 ~ "away on a b2b",
        TRUE ~ "both rested"),
      density = cut(.data$h_games_last_7 + .data$a_games_last_7,
                    breaks = c(-Inf, 4, 5, 6, Inf),
                    labels = c("light week", "normal", "heavy", "very heavy"))
    )
}

DIMENSIONS <- c("home_role", "total_band", "rest_state", "density",
                "month", "season_f")

# ===========================================================================
# 2. Bias and skill, per segment
# ===========================================================================
# For each segment we ask three separate questions and keep them separate:
#
#   market_bias  mean(actual - market number). Is the LINE systematically wrong
#                here? This is the only column that can become an edge.
#   model_bias   mean(actual - model prediction). Is OUR number systematically
#                wrong here? This is a bug report, and is fixable.
#   skill        market RMSE minus model RMSE. Positive means the model is the
#                more accurate of the two in this segment.

seg_row <- function(actual, model_pred, market_pred, dimension, segment, target) {
  e_mkt <- actual - market_pred
  e_mdl <- actual - model_pred
  tt <- suppressWarnings(try(t.test(e_mkt), silent = TRUE))
  ok <- !inherits(tt, "try-error")
  tibble(
    dimension = dimension, target = target, segment = segment, n = length(actual),
    market_bias   = mean(e_mkt),
    bias_lo       = if (ok) tt$conf.int[1] else NA_real_,
    bias_hi       = if (ok) tt$conf.int[2] else NA_real_,
    p_raw         = if (ok) tt$p.value else NA_real_,
    model_bias    = mean(e_mdl),
    rmse_market   = sqrt(mean(e_mkt^2)),
    rmse_model    = sqrt(mean(e_mdl^2)),
    skill         = sqrt(mean(e_mkt^2)) - sqrt(mean(e_mdl^2))
  )
}

segment_table <- function(d, dimensions = DIMENSIONS, min_n = MIN_N) {
  targets <- list(
    total  = list(actual = "total_points", model = "pred_total",  market = "total_close"),
    margin = list(actual = "margin",       model = "pred_margin", market = "market_margin")
  )
  # The market's implied margin is the negated home spread; naming it here
  # keeps the two targets symmetrical below.
  d$market_margin <- -d$spread_close

  map_dfr(names(targets), function(tg) {
    sp <- targets[[tg]]
    map_dfr(dimensions, function(dim) {
      dd <- d %>%
        filter(!is.na(.data[[dim]]), !is.na(.data[[sp$actual]]),
               !is.na(.data[[sp$model]]), !is.na(.data[[sp$market]]))
      if (!nrow(dd)) return(NULL)
      map_dfr(split(dd, as.character(dd[[dim]])), function(g) {
        if (nrow(g) < min_n) return(NULL)
        seg_row(g[[sp$actual]], g[[sp$model]], g[[sp$market]],
                dim, as.character(g[[dim]][1]), tg)
      })
    })
  })
}

# ===========================================================================
# 3. Reporting, with the comparison count in view
# ===========================================================================

report_segments <- function(st, label) {
  step(label)
  if (!nrow(st)) { warn("no segment reached the minimum sample size"); return(invisible(st)) }

  # Holm is the right correction here: it controls the family-wise error rate
  # without assuming the tests are independent, and they are not (a game sits
  # in six segments at once).
  st <- st %>% mutate(p_holm = p.adjust(.data$p_raw, method = "holm"))

  k <- nrow(st)
  expected_false <- ALPHA * k
  info(k, " segments tested at alpha ", ALPHA, "; chance alone yields about ",
       round(expected_false, 1), " 'significant' hits")

  for (tg in unique(st$target)) {
    sub <- st %>% filter(.data$target == tg) %>% arrange(.data$dimension, desc(.data$n))
    message("\n   --- ", tg, " ---")
    message(sprintf("   %-12s %-16s %5s %9s %9s %8s  %7s %7s",
                    "dimension", "segment", "n", "mkt bias", "our bias", "skill",
                    "p", "p(adj)"))
    for (i in seq_len(nrow(sub))) {
      r <- sub[i, ]
      flag <- if (!is.na(r$p_holm) && r$p_holm < ALPHA) " <<" else ""
      message(sprintf("   %-12s %-16s %5d %+9.2f %+9.2f %+8.3f  %7.3f %7.3f%s",
                      r$dimension, substr(r$segment, 1, 16), r$n,
                      r$market_bias, r$model_bias, r$skill, r$p_raw, r$p_holm, flag))
    }
  }
  message("\n   'mkt bias' = actual minus the closing line. A market that prices a")
  message("   segment correctly sits at zero. 'skill' > 0 means our number beat")
  message("   the line's number there. '<<' survives the multiple-comparison")
  message("   correction -- everything else is a lead, not a finding.")
  invisible(st)
}

# ===========================================================================
# 4. Win-probability calibration
# ===========================================================================
# This table is not decoration: it is where the moneyline guards in config.R
# get their numbers. ml_prob_error should be about the typical |model - actual|
# in the bins that carry real sample, and ml_prob_range should be the span of
# those bins. If this table changes on your data, those settings must change.

calibration_report <- function(d) {
  step("Win-probability calibration (sizes the moneyline guards)")
  dd <- d %>% filter(!is.na(.data$pred_home_win_prob), !is.na(.data$home_win))
  if (!nrow(dd)) { warn("no win probabilities to check"); return(invisible(NULL)) }

  have_ml <- sum(!is.na(dd$ml_home) & !is.na(dd$ml_away)) > 30
  if (have_ml) dd$p_market <- devig_two_way(dd$ml_home, dd$ml_away)$a

  tab <- dd %>%
    mutate(bin = cut(.data$pred_home_win_prob, breaks = seq(0, 1, 0.1),
                     include.lowest = TRUE)) %>%
    group_by(.data$bin) %>%
    summarise(n = n(), model = mean(.data$pred_home_win_prob),
              actual = mean(.data$home_win),
              market = if (have_ml) mean(.data$p_market) else NA_real_,
              .groups = "drop") %>%
    mutate(model_err = .data$model - .data$actual,
           market_err = .data$market - .data$actual)

  message(sprintf("   %-12s %6s %7s %7s %7s %9s %9s", "bin", "n", "model",
                  "market", "actual", "our err", "mkt err"))
  for (i in seq_len(nrow(tab))) {
    r <- tab[i, ]
    message(sprintf("   %-12s %6d %7.3f %7s %7.3f %+9.3f %9s",
                    as.character(r$bin), r$n, r$model,
                    if (is.na(r$market)) "n/a" else sprintf("%.3f", r$market),
                    r$actual, r$model_err,
                    if (is.na(r$market_err)) "n/a" else sprintf("%+.3f", r$market_err)))
  }

  solid <- tab %>% filter(.data$n >= 100)
  if (nrow(solid)) {
    typical <- mean(abs(solid$model_err))
    lo <- min(as.numeric(sub("[^0-9.]*([0-9.]+).*", "\\1", as.character(solid$bin))))
    info("bins with 100+ games span roughly ", sprintf("%.2f", lo), " to ",
         sprintf("%.2f", max(solid$model + 0.05)),
         "; typical |error| there is ", sprintf("%.3f", typical))
    info("config has ml_prob_error = ", CFG$backtest$ml_prob_error,
         " and ml_prob_range = [", CFG$backtest$ml_prob_range[1], ", ",
         CFG$backtest$ml_prob_range[2], "]")
    if (typical > CFG$backtest$ml_prob_error * 1.5)
      warn("measured error is well above ml_prob_error -- raise it, or the ",
           "moneyline will bet noise")
  }

  if (have_ml) {
    # Compare like with like: a real file can be missing a fifth of its
    # moneylines, and averaging the model over games the market never priced
    # would not be a comparison at all. (Skipping this is also how the report
    # ends up dividing by an NA and stopping the whole pipeline.)
    kp <- !is.na(dd$p_market) & !is.na(dd$pred_home_win_prob) & !is.na(dd$home_win)
    if (sum(kp) > 30) {
      m_mae <- mean(abs(dd$pred_home_win_prob[kp] - dd$home_win[kp]))
      k_mae <- mean(abs(dd$p_market[kp] - dd$home_win[kp]))
      info("on the ", sum(kp), " games the market priced -- mean |error|: model ",
           sprintf("%.4f", m_mae), " vs market ", sprintf("%.4f", k_mae),
           if (k_mae < m_mae)
             "  -- the market is the more accurate estimator, which is why ml_shrink_to_market < 1"
           else
             "  -- the model is more accurate here; re-check for leakage before believing it")
      info("RMS disagreement between them: ",
           sprintf("%.4f", sqrt(mean((dd$pred_home_win_prob[kp] - dd$p_market[kp])^2))))
    }
  }
  invisible(tab)
}

# ===========================================================================
# 4b. Forecast encompassing -- the question accuracy cannot answer
# ===========================================================================
# RMSE asks "is our number closer?" and the answer here is no -- the closing
# line beats us on both targets. That is the wrong question to stop on.
#
# The right one is whether our number carries information the line does NOT,
# and those are different things. A forecast can be worse overall and still be
# useful, if its errors are made in different places than the market's; a
# forecast can also be nearly as accurate as the market and yet be worthless,
# because it is only re-deriving what the line already says.
#
# The standard test is an encompassing regression:
#
#     actual ~ market_prediction + model_prediction
#
# If the market encompasses the model, the model's coefficient collapses to
# zero -- once you know the line, our number adds nothing. A coefficient
# reliably away from zero means genuinely independent information.
#
# TWO WARNINGS, both load-bearing:
#   * Independent information is NECESSARY for an edge, not SUFFICIENT. The
#     information still has to be worth more than the vig, and this regression
#     says nothing about that. 04_backtest.R is where that gets decided.
#   * The two predictors are heavily collinear by construction -- both are
#     estimates of the same quantity -- which inflates the standard errors.
#     A small coefficient with a wide interval is genuinely inconclusive, not
#     evidence of absence.

encompassing_report <- function(d) {
  step("Forecast encompassing: does the model know anything the line does not?")

  d$market_margin <- -d$spread_close
  targets <- list(
    total  = list(actual = "total_points", model = "pred_total",  market = "total_close"),
    margin = list(actual = "margin",       model = "pred_margin", market = "market_margin")
  )

  fit_one <- function(dd) {
    fit <- lm(actual ~ market + model, data = dd)
    co  <- summary(fit)$coefficients
    ci  <- confint(fit)
    list(fit = fit,
         beta_market = co["market", "Estimate"],
         beta_model  = co["model",  "Estimate"],
         lo = ci["model", 1], hi = ci["model", 2],
         p  = co["model", "Pr(>|t|)"])
  }

  out <- map_dfr(names(targets), function(tg) {
    sp <- targets[[tg]]
    dd <- tibble(actual = d[[sp$actual]], model = d[[sp$model]],
                 market = d[[sp$market]], season = d$season) %>%
      filter(!is.na(.data$actual), !is.na(.data$model), !is.na(.data$market))
    if (nrow(dd) < 200) return(NULL)

    f <- fit_one(dd)
    tibble(
      target = tg, n = nrow(dd),
      beta_market = f$beta_market, beta_model = f$beta_model,
      model_lo = f$lo, model_hi = f$hi, p_model = f$p,
      # How much of our number is a restatement of the line.
      cor_preds   = cor(dd$market, dd$model),
      rmse_market = rmse(dd$actual, dd$market),
      rmse_model  = rmse(dd$actual, dd$model),
      rmse_comb   = rmse(dd$actual, fitted(f$fit))
    )
  })

  if (!nrow(out)) { warn("not enough out-of-sample forecasts to test"); return(invisible(out)) }

  message(sprintf("   %-8s %6s %8s %8s %-22s %8s %8s",
                  "target", "n", "b(mkt)", "b(model)", "model 95% CI", "p", "cor"))
  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    message(sprintf("   %-8s %6d %8.3f %8.3f  [%+.3f, %+.3f]%s %8.4f %8.3f",
                    r$target, r$n, r$beta_market, r$beta_model, r$model_lo, r$model_hi,
                    if (r$model_lo > 0 || r$model_hi < 0) " *" else "  ",
                    r$p_model, r$cor_preds))
  }
  message("   * = the model's coefficient interval excludes zero")

  message("\n   accuracy, for contrast (RMSE, lower is better):")
  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    message(sprintf("     %-8s market %6.2f   model %6.2f   fitted blend %6.2f",
                    r$target, r$rmse_market, r$rmse_model, r$rmse_comb))
  }
  message("   The blend is fitted in-sample on these same forecasts. With two")
  message("   parameters over thousands of games the optimism is small, but it")
  message("   is an upper bound, not an achievable result.")

  # --- stability across seasons -------------------------------------------
  # A coefficient that changes sign season to season is noise wearing a
  # p-value. One that holds its sign is worth acting on. This is not
  # decoration: a pooled interval can clear zero on a coefficient that is
  # positive half the time and negative the other half, and reporting that as
  # a finding is exactly the failure this file exists to prevent.
  message("")
  stable <- setNames(rep(FALSE, nrow(out)), out$target)
  for (tg in out$target) {
    sp <- targets[[tg]]
    dd <- tibble(actual = d[[sp$actual]], model = d[[sp$model]],
                 market = d[[sp$market]], season = d$season) %>%
      filter(!is.na(.data$actual), !is.na(.data$model), !is.na(.data$market))
    per <- map_dfr(split(dd, dd$season), function(g) {
      if (nrow(g) < 150) return(NULL)
      f <- fit_one(g)
      tibble(season = g$season[1], beta_model = f$beta_model)
    })
    if (nrow(per) > 1) {
      stable[[tg]] <- length(unique(sign(per$beta_model))) == 1
      info(tg, " model coefficient by season: ",
           paste(sprintf("%d %+.2f", per$season, per$beta_model), collapse = "  "),
           if (stable[[tg]]) "  (sign stable)" else "  (SIGN FLIPS -- treat as noise)")
    }
  }

  # --- verdict -------------------------------------------------------------
  # Two conditions, both required: the pooled interval must clear zero AND the
  # per-season sign must hold. Either alone is a way to fool yourself.
  message("")
  out$sign_stable <- unname(stable[out$target])
  informative <- out %>%
    filter((.data$model_lo > 0 | .data$model_hi < 0), .data$sign_stable)

  unstable <- out %>%
    filter((.data$model_lo > 0 | .data$model_hi < 0), !.data$sign_stable)
  if (nrow(unstable)) {
    for (i in seq_len(nrow(unstable))) {
      r <- unstable[i, ]
      message(sprintf(
        "   %s: the pooled coefficient (%+.3f) clears zero, but its sign flips",
        r$target, r$beta_model))
      message("   across seasons. That is not a signal -- it is a pooled average of")
      message("   inconsistent seasons, and it will not reproduce out of sample.")
      if (r$beta_model < 0)
        message("   It is also NEGATIVE, which would mean fading our own forecast: ",
                "\n   far likelier to be model mis-specification than an edge.")
    }
    message("")
  }

  if (!nrow(informative)) {
    message("   The closing line encompasses the model on every target: once you")
    message("   know the line, our number adds nothing measurable. That is the")
    message("   expected result, and it says the feature set is re-deriving what")
    message("   the market already prices rather than finding anything new.")
    message("   Adding features in the same family will not change this. The way")
    message("   out is a signal the line demonstrably lacks -- which is what the")
    message("   news and availability work in 06/07 is for.")
  } else {
    for (i in seq_len(nrow(informative))) {
      r <- informative[i, ]
      message(sprintf(
        "   %s: the model's coefficient is %+.3f with the interval clear of zero.",
        r$target, r$beta_model))
    }
    message("   That is independent information, and it is the precondition for")
    message("   an edge. It is NOT yet an edge: 04_backtest.R decides whether it")
    message("   survives the vig, and the forward test decides whether it is real.")
  }
  invisible(out)
}

# ===========================================================================
# 5. ROI by segment -- reported last, and deliberately hedged
# ===========================================================================

roi_by_segment <- function(bets, seg, dimensions = DIMENSIONS, min_n = 40) {
  step("ROI by segment (read this AFTER the bias tables)")
  if (!nrow(bets)) { warn("no bets"); return(invisible(NULL)) }

  b <- bets %>% left_join(seg %>% select("game_id", all_of(dimensions)), by = "game_id")
  out <- map_dfr(dimensions, function(dim) {
    dd <- b %>% filter(!is.na(.data[[dim]]))
    if (!nrow(dd)) return(NULL)
    map_dfr(split(dd, as.character(dd[[dim]])), function(g) {
      if (nrow(g) < min_n) return(NULL)
      tt <- suppressWarnings(try(t.test(g$units), silent = TRUE))
      tibble(dimension = dim, segment = as.character(g[[dim]][1]), n = nrow(g),
             roi = mean(g$units, na.rm = TRUE),
             p_raw = if (inherits(tt, "try-error")) NA_real_ else tt$p.value)
    })
  })
  if (!nrow(out)) { warn("no segment had enough bets"); return(invisible(NULL)) }

  out <- out %>% mutate(p_holm = p.adjust(.data$p_raw, method = "holm")) %>%
    arrange(desc(.data$roi))
  info(nrow(out), " segments; expect about ", round(ALPHA * nrow(out), 1),
       " to look significant by chance alone")
  for (i in seq_len(nrow(out))) {
    r <- out[i, ]
    flag <- if (!is.na(r$p_holm) && r$p_holm < ALPHA) " <<" else ""
    message(sprintf("   %-12s %-16s n=%4d  ROI %+7.2f%%  p %.3f  p(adj) %.3f%s",
                    r$dimension, substr(r$segment, 1, 16), r$n, 100 * r$roi,
                    r$p_raw, r$p_holm, flag))
  }
  survivors <- sum(out$p_holm < ALPHA, na.rm = TRUE)
  if (!survivors)
    message("\n   Nothing survives correction. That is the expected result and it is\n",
            "   the correct one to report. Do not go shopping in the raw p column.")
  invisible(out)
}

# ===========================================================================
# 6. Candidate hypotheses
# ===========================================================================
# The actual deliverable: a short list of things worth investigating, phrased
# so that each one is falsifiable on the next batch of data.

hypotheses <- function(st) {
  step("Candidate hypotheses")
  if (!nrow(st)) { info("nothing to suggest"); return(invisible(NULL)) }

  st <- st %>% mutate(p_holm = p.adjust(.data$p_raw, method = "holm"))
  mkt <- st %>% filter(!is.na(.data$p_holm), .data$p_holm < ALPHA) %>%
    arrange(.data$p_holm)
  ours <- st %>% filter(abs(.data$model_bias) > 1.0, .data$n >= MIN_N) %>%
    arrange(desc(abs(.data$model_bias)))

  if (nrow(mkt)) {
    message("   MARKET mispricing that survived correction -- worth a look:")
    for (i in seq_len(min(5, nrow(mkt)))) {
      r <- mkt[i, ]
      message(sprintf("     %s = %s: %s finishes %+.2f vs the line (n=%d, p(adj) %.4f)",
                      r$dimension, r$segment, r$target, r$market_bias, r$n, r$p_holm))
    }
    message("     Before believing any of these: they are still six dimensions'")
    message("     worth of looking. Confirm on a season this analysis has not seen.")
  } else {
    message("   No market bias survives multiple-comparison correction.")
    message("   That is the expected finding against a real market, and it is a")
    message("   result rather than a dead end: it says the line already prices")
    message("   rest, schedule density and game shape about as well as we can.")
  }

  if (nrow(ours)) {
    message("\n   OUR OWN bias -- these are bugs to fix, not edges to bet:")
    for (i in seq_len(min(5, nrow(ours)))) {
      r <- ours[i, ]
      message(sprintf("     %s = %s: our %s is off by %+.2f on average (n=%d)",
                      r$dimension, r$segment, r$target, r$model_bias, r$n))
    }
    message("     A large own-bias next to a near-zero market bias means the")
    message("     feature set is missing something the line already has.")
  }
  invisible(list(market = mkt, model = ours))
}

# ===========================================================================
# 7. Figures
# ===========================================================================

diagnostic_figures <- function(st) {
  if (!nrow(st)) return(invisible(NULL))

  p7 <- st %>%
    filter(.data$dimension %in% c("home_role", "rest_state", "total_band", "density")) %>%
    mutate(lab = paste0(.data$segment, "  (n=", .data$n, ")")) %>%
    ggplot(aes(x = .data$market_bias, y = reorder(.data$lab, .data$market_bias))) +
    geom_vline(xintercept = 0, colour = "grey40") +
    geom_errorbar(aes(xmin = .data$bias_lo, xmax = .data$bias_hi),
                  orientation = "y", width = 0.25, colour = "grey55") +
    geom_point(size = 2) +
    facet_wrap(~ .data$target, scales = "free_x") +
    labs(title = "Is the closing line biased in any of these situations?",
         subtitle = "Actual result minus the closing number, with 95% intervals",
         x = "points (positive = the market's number was too low)", y = NULL,
         caption = "Intervals crossing zero mean the market prices that situation correctly. Most will.") +
    theme_project()
  save_fig(p7, "07_market_bias_by_segment.png", width = 10, height = 6.5)

  p8 <- st %>%
    filter(.data$dimension %in% c("month", "season_f")) %>%
    ggplot(aes(x = .data$segment, y = .data$skill, fill = .data$target)) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    facet_wrap(~ .data$dimension, scales = "free_x") +
    labs(title = "Model accuracy minus market accuracy, over time",
         subtitle = "Positive = our number beat the line's number that period",
         x = NULL, y = "RMSE advantage (points)", fill = "target",
         caption = "Sustained positive values would be remarkable. Bouncing around zero is normal.") +
    theme_project()
  save_fig(p8, "08_skill_over_time.png", width = 10, height = 5.5)
}

# ===========================================================================
# 8. Run
# ===========================================================================

seg <- build_segments(preds, model_data)
step("Diagnostics")
info(nrow(seg), " out-of-sample games across ", length(DIMENSIONS),
     " pre-declared dimensions")

seg_stats <- segment_table(seg)
seg_stats <- report_segments(seg_stats, "Bias and skill by segment")
encompassing <- encompassing_report(seg)
calibration_report(seg)

bets_path <- file.path(CFG$paths$output_dir, "backtest_bets.csv")
if (file.exists(bets_path)) {
  bets_d <- readr::read_csv(bets_path, show_col_types = FALSE)
  roi_by_segment(bets_d, seg)
} else {
  warn("no backtest_bets.csv -- run 04_backtest.R first for the ROI table")
}

hypotheses(seg_stats)
diagnostic_figures(seg_stats)

readr::write_csv(seg_stats, file.path(CFG$paths$output_dir, "diagnostics_segments.csv"))
step("Saved ", file.path(CFG$paths$output_dir, "diagnostics_segments.csv"))
