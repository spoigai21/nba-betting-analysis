# ---------------------------------------------------------------------------
# config.R -- every tunable number in the project, in one place
# ---------------------------------------------------------------------------
# Change settings HERE rather than editing the analysis scripts. That keeps the
# pipeline honest: you can see, in one screen, every choice that could have been
# tuned after seeing results.
# ---------------------------------------------------------------------------

CFG <- list(

  paths = list(
    # Your Kaggle download. If your file has a different name, either rename it
    # to data/nba_betting.csv or change this line.
    raw_csv        = "data/nba_betting.csv",
    processed_dir  = "data/processed",
    games_rds      = "data/processed/games.rds",
    games_csv      = "data/processed/games_tidy.csv",
    model_data_rds = "data/processed/model_data.rds",
    models_rds     = "output/models.rds",
    preds_csv      = "output/backtest_predictions.csv",
    output_dir     = "output",
    figures_dir    = "output/figures",
    track_record   = "output/track_record.csv",
    manual_news    = "data/manual_news.csv"
  ),

  # --- Column mapping -------------------------------------------------------
  # 01_load_data.R guesses which of your CSV's columns is the date, the spread,
  # the total, etc. If it guesses wrong it will TELL you, and you fix it here:
  #
  #   column_overrides = list(spread_close = "Home Line Close", total_close = "OU")
  #
  # Left side = the project's standard name; right side = the name in YOUR file.
  column_overrides = list(),

  # Some datasets quote the spread from the away/favourite side. 01 checks the
  # sign against actual game results and flips it if needed. Set to FALSE to
  # trust your file's sign as-is.
  auto_fix_spread_sign = TRUE,

  # --- Model ----------------------------------------------------------------
  model = list(
    # A team needs this many completed games in the current season before its
    # form features are trustworthy. Earlier games are dropped from modelling.
    min_prior_games = 10,

    # Rolling-form window (games).
    form_window = 10,

    # --- opponent-adjusted ratings -------------------------------------------
    # Ridge penalty for the team ratings fitted in 02_features.R. Read it as
    # "how many games of average play a team must post before its rating is
    # believed": at 10, a team twenty games in sits about a third of the way
    # back toward league average.
    rating_lambda = 10,

    # Games a season must have on the board before ratings are fitted at all.
    # Thirty teams and a home-advantage term is 31 parameters; below this the
    # fit is mostly penalty.
    rating_min_games = 60,

    # Backtesting is walk-forward: train on everything strictly before the test
    # season, predict that season, then roll forward. This is the first season
    # that gets predicted (needs at least one full prior season to train on).
    first_test_season = NA_integer_,   # NA = second-oldest season in the data

    # Should the model be allowed to see the OPENING market line as a feature?
    # FALSE  = pure fundamentals model. Harder, but a genuine independent view.
    # TRUE   = market-anchored model. Much more accurate, but it mostly learns
    #          to repeat the line, so any "edge" it shows is fragile.
    # The CLOSING line is NEVER a feature -- it is evaluation only.
    use_market_features = FALSE
  ),

  # --- Backtest -------------------------------------------------------------
  backtest = list(
    # Only bet when the model disagrees with the line by at least this much.
    total_edge_threshold  = 3.0,   # points
    spread_edge_threshold = 2.0,   # points

    # Moneyline is selected on EXPECTED VALUE, not on a probability gap.
    # Three points of probability edge on a -500 favourite is worth a fraction
    # of the same three points on a coin flip, and only EV knows the difference.
    # 0.02 = "bet when the model says this returns +2% per unit staked".
    ml_ev_threshold = 0.02,

    # The band in which the model's probability for the SIDE BEING BET must
    # fall. Not a taste setting: it is the region where the model has enough
    # sample to have demonstrated calibration at all. On the sample data every
    # bin from 0.2 to 0.9 carries 150+ games and lands within 3.5 points of
    # observed; outside that band the bins hold ~20 games and miss by 15.
    # Betting a side the model calls 13% means trusting a number the model has
    # never been checked on, at fifteen-to-one leverage.
    ml_prob_range = c(0.20, 0.90),

    # --- Two guards that stop the moneyline betting the model's own error ---
    # Both numbers are MEASURED, not chosen. 08_diagnostics.R re-measures them
    # on your data and tells you if these values are wrong for it.
    #
    # Believe only this fraction of the model's disagreement with the market.
    # Justification: on the sample data the model's probabilities are LESS
    # accurate than the market's de-vigged ones (MAE 0.4425 vs 0.4325), so a
    # disagreement is mostly the model being wrong. 0.5 is already generous.
    ml_shrink_to_market = 0.50,

    # Margin of safety, in probability points, applied against whichever side
    # is being considered. Set to the model's typical calibration error in the
    # bins where it has real sample (see 08's calibration table).
    ml_prob_error = 0.03,

    # How much EV uncertainty, in units per unit staked, we will tolerate from
    # probability error alone. This is NOT a price preference -- it derives the
    # maximum price the model is entitled to bet:
    #
    #     an error of d in p moves EV by d * decimal_odds
    #     so a tolerance of T implies  decimal_odds <= T / d
    #
    # At d = 0.03 and T = 0.10 that is decimal 3.33, or about +233. Beyond it
    # the EV estimate is mostly a restatement of the model's own error bar.
    #
    # This guard exists because of a specific, structural failure this project
    # ran into: a linear model's probabilities are COMPRESSED relative to the
    # market's, so on every heavy-favourite game it judges the underdog
    # underpriced. Unguarded, that reads as a steady supply of +1400 value bets
    # and produces a headline 30% expected return. It is under-dispersion, not
    # edge, and no threshold on EV or on disagreement size can tell them apart
    # -- only refusing the leverage can.
    ml_max_ev_error = 0.10,

    # Odds to assume when the dataset does not record a price.
    default_price = -110,

    unit = 1,                      # flat betting: every bet is 1 unit
    bootstrap_reps = 2000,         # for ROI confidence intervals

    # Sanity guard: refuse to report an ROI headline on a tiny sample.
    min_bets_for_conclusion = 100
  ),

  # --- News / availability signals (forward test only) ----------------------
  # See the header of R/06_news_signals.R for why these never touch a backtest.
  news = list(
    # Apply manual_news.csv adjustments when logging forward predictions.
    # The raw model number is kept alongside the adjusted one either way, so
    # 06's evaluate_news_contribution() can grade the reads separately.
    apply_in_forward = TRUE,

    # Confidence floor for treating an extracted news statement as an absence.
    min_confidence = 0.7,

    # Refuse to apply a hand-entered impact larger than this. A points estimate
    # this big is a guess with decimals, not a read.
    max_abs_impact = 8.0
  ),

  # --- LLM strategy-signal extraction (09) ----------------------------------
  # Deliberately NOT used for "who is out". ESPN tags articles with the same
  # athlete ids the box scores use, so entity resolution is already exact, and
  # injury-report prose is formulaic enough for the pattern rules in 06. The
  # model is pointed at the part regex cannot do: coach-revealed rotation and
  # minutes intentions.
  llm = list(
    model    = "claude-opus-5",
    effort   = "low",          # extraction from one short article
    api_key_env = "ANTHROPIC_API_KEY",
    base     = "https://api.anthropic.com/v1/messages",
    version  = "2023-06-01",
    max_tokens = 4000,

    # Every extraction is cached by (article, prompt version, model) and the
    # cache is committed. An LLM call is not reproducible; a cache of its
    # answers is. Nothing is ever re-queried for an article already seen, so
    # re-running the pipeline cannot change a logged extraction.
    cache    = "data/llm_news_cache.jsonl",

    # Below this, a signal is recorded but not surfaced as actionable.
    min_confidence = 0.6
  ),

  # --- SportsDataIO: structured availability (optional, paid) ---------------
  # hoopR exposes ESPN roster status, which is a substitute for an injury feed
  # rather than one. SportsDataIO sells the real thing: probable / questionable
  # / doubtful / out, refreshed several times a day, plus lineup projections.
  # Free trial is 1,000 calls a month -- about 33 a day, ample for one pull.
  #
  # AUTH is verified: the key goes in an Ocp-Apim-Subscription-Key header (a
  # ?key= query parameter also works).
  #
  # THE PATH IS NOT VERIFIED. SportsDataIO does not publish the NBA injury
  # endpoint outside an account, and guessing it into production code would be
  # a silent 404 waiting to happen. Check it in your account's API explorer and
  # correct it here; fetch_sdio_injuries() reports clearly when it is wrong.
  sportsdataio = list(
    base          = "https://api.sportsdata.io/v3/nba",
    injuries_path = "scores/json/Injuries",   # <- VERIFY against your account
    key_env       = "SPORTSDATAIO_API_KEY"
  ),

  # --- Diagnostics (08) -----------------------------------------------------
  diagnostics = list(
    # A segment needs this many rows before it is reported at all.
    min_segment_n = 60,

    # Two-sided significance level used before multiple-comparison correction.
    alpha = 0.05
  ),

  # --- Forward-testing (live odds) -----------------------------------------
  odds_api = list(
    base      = "https://api.the-odds-api.com/v4",
    sport     = "basketball_nba",
    regions   = "us",
    markets   = "spreads,totals,h2h",
    odds_fmt  = "american",
    # Prefer this book when several are returned (NULL = median across books).
    bookmaker = NULL,
    key_env   = "ODDS_API_KEY"
  ),

  # --- Synthetic sample data ------------------------------------------------
  # Used by 99_make_sample_data.R so the pipeline runs before you have a real
  # CSV. The simulated market is deliberately near-efficient.
  sample = list(
    # NBA seasons are labelled by ending year, so 2026 is the 2025-26 season.
    # Keep this current: a synthetic set that stops two seasons back silently
    # trains and tests the pipeline on an era the live path no longer sees.
    seasons      = 2023:2026,
    games_per_season = 1230,
    home_advantage   = 2.4,      # points
    margin_sd        = 11.5,
    total_sd         = 17.5,
    line_noise_sd    = 0.60,     # how far the closing line sits from truth
    open_noise_sd    = 1.20      # extra noise on the opening line
  )
)
