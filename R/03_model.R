# ---------------------------------------------------------------------------
# 03_model.R -- simple, explainable models, trained walk-forward
# ---------------------------------------------------------------------------
# Three models, all linear:
#   total_points ~ pace/scoring features     (lm)
#   margin       ~ strength/rest features    (lm)
#   home_win     ~ strength/rest features    (logistic glm, for calibration)
#
# Training is WALK-FORWARD: to predict season S the model is fitted only on
# seasons before S, then refitted with S included before predicting S+1. No
# prediction is ever made by a model that has seen the game it is predicting,
# or anything that happened after it.
#
# Linear models are deliberate. They are transparent, they do not need tuning
# (so there is nothing to quietly tune against the test set), and against an
# efficient market a fancier model mostly buys a fancier way to overfit.
#
# Output: output/backtest_predictions.csv, output/models.rds
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")
if (!exists("model_data")) model_data <- readRDS(CFG$paths$model_data_rds)

feature_sets <- function() {
  if (isTRUE(CFG$model$use_market_features)) {
    list(total = FEATURES_TOTAL_MARKET, margin = FEATURES_MARGIN_MARKET)
  } else {
    list(total = FEATURES_TOTAL, margin = FEATURES_MARGIN)
  }
}

# Feature vectors live in 02; reload it if this script is run on its own.
if (!exists("FEATURES_TOTAL")) {
  FEATURES_TOTAL  <- c("pace_sum", "form_total_sum", "off_sum",
                       "rest_sum", "b2b_any", "pace_rating_sum")
  FEATURES_MARGIN <- c("net_diff", "form_diff", "prev_net_diff", "win_pct_diff",
                       "rest_diff", "b2b_diff", "density_diff", "rating_diff")
  FEATURES_TOTAL_MARKET  <- c(FEATURES_TOTAL,  "total_open")
  FEATURES_MARGIN_MARKET <- c(FEATURES_MARGIN, "spread_open")
}

make_formula <- function(target, feats) {
  as.formula(paste(target, "~", paste(feats, collapse = " + ")))
}

fit_models <- function(train, feats = feature_sets()) {
  # Market features are dropped automatically if the dataset lacks opening lines.
  usable <- function(f) f[vapply(f, function(cc)
    cc %in% names(train) && sum(!is.na(train[[cc]])) > 50 &&
      sd(train[[cc]], na.rm = TRUE) > 0, logical(1))]

  ft <- usable(feats$total)
  fm <- usable(feats$margin)

  fits <- list(
    total  = lm(make_formula("total_points", ft), data = train),
    margin = lm(make_formula("margin", fm), data = train),
    winp   = glm(make_formula("home_win", fm), data = train, family = binomial())
  )

  # An NA coefficient means two features are exact linear combinations of each
  # other. lm drops one without complaining; say so out loud instead.
  for (nm in names(fits)) {
    aliased <- names(which(is.na(coef(fits[[nm]]))))
    if (length(aliased))
      warn(nm, " model: feature(s) dropped as collinear -- ",
           paste(aliased, collapse = ", "))
  }

  c(fits, list(features = list(total = ft, margin = fm), n_train = nrow(train)))
}

predict_models <- function(models, newdata) {
  tibble(
    pred_total  = as.numeric(predict(models$total,  newdata = newdata)),
    pred_margin = as.numeric(predict(models$margin, newdata = newdata)),
    pred_home_win_prob = as.numeric(predict(models$winp, newdata = newdata,
                                            type = "response"))
  )
}

# ===========================================================================
# Walk-forward evaluation
# ===========================================================================

walk_forward <- function(md, min_train_games = 500) {
  seasons <- sort(unique(md$season))
  first_test <- CFG$model$first_test_season
  if (is.na(first_test)) {
    # Earliest season that still leaves a usable training set behind it.
    cum <- md %>% count(.data$season) %>% arrange(.data$season) %>%
      mutate(before = cumsum(.data$n) - .data$n)
    ok <- cum$season[cum$before >= min_train_games]
    first_test <- if (length(ok)) min(ok) else seasons[max(2, length(seasons))]
  }
  test_seasons <- seasons[seasons >= first_test]
  if (!length(test_seasons))
    stop("Not enough history to walk forward. Need at least two seasons.", call. = FALSE)

  step("Walk-forward: testing seasons ", paste(test_seasons, collapse = ", "))

  out <- map_dfr(test_seasons, function(s) {
    train <- md %>% filter(.data$season < s)
    test  <- md %>% filter(.data$season == s)
    if (nrow(train) < 200) { warn("season ", s, ": only ", nrow(train),
                                  " training games, skipping"); return(NULL) }
    models <- fit_models(train)
    info("season ", s, ": train ", nrow(train), " -> test ", nrow(test))
    bind_cols(
      test %>% select("game_id", "date", "season", "home_team", "away_team",
                      "total_points", "margin", "home_win",
                      "spread_close", "total_close", "spread_open", "total_open",
                      "ml_home", "ml_away", "price_spread_home", "price_spread_away",
                      "price_over", "price_under"),
      predict_models(models, test)
    )
  })

  out %>% mutate(
    # The model's own view of the line, on the market's scale.
    model_spread = -.data$pred_margin,             # spread is quoted home-side
    edge_total   = .data$pred_total  - .data$total_close,
    edge_spread  = .data$spread_close - .data$model_spread
    # edge_spread > 0  =>  model thinks home is stronger than the line says
  )
}

# ===========================================================================
# Accuracy report -- is the model any good, before any betting?
# ===========================================================================

accuracy_report <- function(preds) {
  step("Out-of-sample accuracy")

  cmp <- function(label, actual, model_pred, market_pred) {
    keep <- !is.na(actual) & !is.na(model_pred)
    m_rmse <- rmse(actual[keep], model_pred[keep])
    line <- rep(NA_real_, length(actual))
    if (!all(is.na(market_pred))) line <- market_pred
    k2 <- keep & !is.na(line)
    mk_rmse <- if (any(k2)) rmse(actual[k2], line[k2]) else NA_real_
    message(sprintf("   %-8s  model RMSE %6.2f   market RMSE %s   n = %d",
                    label, m_rmse,
                    if (is.na(mk_rmse)) "   n/a" else sprintf("%6.2f", mk_rmse),
                    sum(keep)))
    tibble(target = label, model_rmse = m_rmse, market_rmse = mk_rmse, n = sum(keep))
  }

  acc <- bind_rows(
    cmp("total",  preds$total_points, preds$pred_total,  preds$total_close),
    cmp("margin", preds$margin,       preds$pred_margin, -preds$spread_close)
  )

  # Win-probability calibration and Brier score.
  kp <- !is.na(preds$pred_home_win_prob) & !is.na(preds$home_win)
  brier <- mean((preds$pred_home_win_prob[kp] - preds$home_win[kp])^2)
  base  <- mean(preds$home_win[kp])
  info("home-win Brier score: ", round(brier, 4),
       "  (always predicting the base rate ", round(base, 3), " gives ",
       round(base * (1 - base), 4), ")")

  if (all(!is.na(acc$market_rmse))) {
    beat <- acc$model_rmse < acc$market_rmse
    for (i in seq_len(nrow(acc))) {
      if (isTRUE(beat[i]))
        info(acc$target[i], ": model is MORE accurate than the closing line -- ",
             "check for leakage before believing it")
      else
        info(acc$target[i], ": the closing line is more accurate (expected). ",
             "Edge, if any, comes from disagreeing selectively -- see 04_backtest.R")
    }
  }
  acc
}

# ===========================================================================
# Run
# ===========================================================================

preds <- walk_forward(model_data)
accuracy <- accuracy_report(preds)

# A model fitted on EVERYTHING, for predicting genuinely future games in 05.
final_models <- fit_models(model_data)
step("Final model coefficients (fitted on all ", final_models$n_train, " games)")
print(round(coef(final_models$total), 3))
print(round(coef(final_models$margin), 3))

saveRDS(list(models = final_models, accuracy = accuracy,
             trained_through = max(model_data$date)),
        CFG$paths$models_rds)
readr::write_csv(preds, CFG$paths$preds_csv)
step("Saved ", CFG$paths$preds_csv, " and ", CFG$paths$models_rds)
