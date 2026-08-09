# ---------------------------------------------------------------------------
# utils.R -- small helpers used everywhere: odds maths, team names, stats
# ---------------------------------------------------------------------------
# Nothing here knows about NBA modelling. These are the plumbing functions.
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# Pretty section headers so a long pipeline run is readable.
step <- function(...) message("\n== ", ..., " ", strrep("=", max(0, 60 - nchar(paste0(...)))))
info <- function(...) message("   - ", ...)
warn <- function(...) message("   ! ", ...)

# ===========================================================================
# ODDS MATHS
# ===========================================================================
# American odds: -110 means "risk 110 to win 100"; +150 means "risk 100 to win
# 150". Every profit number in this project runs through these functions, which
# is how the sportsbook's margin (the vig) stays in the results.

american_to_decimal <- function(odds) {
  odds <- as.numeric(odds)
  ifelse(is.na(odds), NA_real_,
         ifelse(odds > 0, 1 + odds / 100, 1 + 100 / abs(odds)))
}

# Implied probability INCLUDING the vig. The two sides of a market sum to >1;
# the excess is the book's margin.
american_to_prob <- function(odds) {
  odds <- as.numeric(odds)
  ifelse(is.na(odds), NA_real_,
         ifelse(odds > 0, 100 / (odds + 100), abs(odds) / (abs(odds) + 100)))
}

decimal_to_american <- function(dec) {
  dec <- as.numeric(dec)
  ifelse(is.na(dec) | dec <= 1, NA_real_,
         ifelse(dec >= 2, (dec - 1) * 100, -100 / (dec - 1)))
}

# Median of several books' prices for the same bet.
#
# Taking the median of the American numbers themselves is wrong, and silently
# so: the scale jumps from -100 straight to +100 with nothing in between, so
# two books at -105 and +105 produce a "median" of 0, which is not a price at
# all. Decimal odds are continuous and monotonic in how good the price is, so
# the median is taken there and converted back.
median_american <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  decimal_to_american(median(american_to_decimal(x)))
}

prob_to_american <- function(p) {
  p <- as.numeric(p)
  ifelse(is.na(p) | p <= 0 | p >= 1, NA_real_,
         ifelse(p >= 0.5, -100 * p / (1 - p), 100 * (1 - p) / p))
}

# Strip the vig from a two-way market so the two probabilities sum to 1.
# Proportional (a.k.a. multiplicative) method -- simple and standard.
devig_two_way <- function(odds_a, odds_b) {
  pa <- american_to_prob(odds_a)
  pb <- american_to_prob(odds_b)
  tot <- pa + pb
  list(a = pa / tot, b = pb / tot, hold = tot - 1)
}

# Break-even win rate at a given price. At -110 this is 0.5238 -- the number
# every strategy in this repo has to clear.
break_even_prob <- function(odds) american_to_prob(odds)

# Profit in units for a settled flat bet.
bet_units <- function(price, outcome, stake = 1) {
  dec <- american_to_decimal(price)
  ifelse(is.na(outcome) | outcome == "pending", NA_real_,
  ifelse(outcome == "win",  stake * (dec - 1),
  ifelse(outcome == "loss", -stake, 0)))            # "push" -> 0
}

# Settle a side bet given the number the bet needed to beat.
# value  : what actually happened (e.g. actual total points)
# line   : the number bet against
# side   : "over"/"under" for totals, "home"/"away" for spreads
settle_total <- function(value, line, side) {
  ifelse(is.na(value) | is.na(line), NA_character_,
  ifelse(value == line, "push",
  ifelse((side == "over" & value > line) | (side == "under" & value < line),
         "win", "loss")))
}

# Spreads are quoted from the home side: -4.5 means home must win by 5+.
# A home bet wins when (home_margin + spread_home) > 0.
settle_spread <- function(margin, spread_home, side) {
  adj <- ifelse(side == "home", margin + spread_home, -margin - spread_home)
  ifelse(is.na(adj), NA_character_,
  ifelse(adj == 0, "push", ifelse(adj > 0, "win", "loss")))
}

# Moneyline: there is no line to beat, only who won. NBA games cannot end
# level, so there is no push -- a margin of exactly 0 means the score columns
# are wrong, and returning NA says so instead of inventing a result.
settle_moneyline <- function(margin, side) {
  ifelse(is.na(margin) | margin == 0, NA_character_,
  ifelse((side == "home" & margin > 0) | (side == "away" & margin < 0),
         "win", "loss"))
}

# Expected profit per 1 unit staked at a given price, if the true win
# probability is p. This is the right scale on which to choose moneyline bets:
# a probability edge is worth money in proportion to the price it is priced at,
# and only EV knows that. Positive EV = worth making, before any vig argument.
bet_ev <- function(p, price) {
  dec <- american_to_decimal(price)
  ifelse(is.na(p) | is.na(dec), NA_real_, p * (dec - 1) - (1 - p))
}

# Closing-line value: how much better the number we took was than the number
# the market closed at, signed so that positive is always good for us.
#
# One function, used by both the backtest and the live log. Sign conventions are
# the easiest thing in this entire project to get backwards, and a flipped sign
# converts a losing strategy into a winning-looking one without any other
# symptom -- so the rule is written once and unit-tested, rather than re-derived
# at each call site.
#
# Units follow the market: points for totals and spreads, probability for the
# moneyline (whose "line" is the market's de-vigged win probability).
clv_of <- function(market, bet, line_taken, line_close) {
  ifelse(is.na(line_taken) | is.na(line_close) | is.na(bet), NA_real_,
  # Over wants the close HIGHER than what it took; under wants it lower.
  ifelse(market == "total",
         ifelse(bet == "over", line_close - line_taken, line_taken - line_close),
  # A home spread bet wants the close to be a SMALLER number than it took
  # (took +6.5, closed +5 => we got the better side of a move toward home).
  ifelse(market == "spread",
         ifelse(bet == "home", line_taken - line_close, line_close - line_taken),
  # Backing a side the market then moves toward means we took the better price.
  ifelse(market == "moneyline",
         ifelse(bet == "home", line_close - line_taken, line_taken - line_close),
         NA_real_))))
}

# ===========================================================================
# MONEYLINE SELECTION
# ===========================================================================
# Lives here, rather than inside 04_backtest.R, because BOTH the backtest and
# the live path in 05_forward.R must choose moneyline bets by exactly the same
# rule. A forward test whose selection differs from the backtest's is not a
# test of the backtest.
#
# The rule and the reasoning behind each guard are documented at the moneyline
# section of 04_backtest.R. In short: shrink the disagreement toward the market
# because the market is the more accurate estimator, take a haircut for the
# model's own calibration error, and refuse prices long enough that the EV
# estimate is dominated by that error.
#
# Returns one row per input game; `side` is NA where no bet qualifies.
moneyline_pick <- function(p_model_home, ml_home, ml_away,
                           shrink       = CFG$backtest$ml_shrink_to_market,
                           haircut      = CFG$backtest$ml_prob_error,
                           band         = CFG$backtest$ml_prob_range,
                           ev_threshold = CFG$backtest$ml_ev_threshold,
                           max_dec      = CFG$backtest$ml_max_ev_error /
                                          CFG$backtest$ml_prob_error) {
  p_raw <- as.numeric(p_model_home)
  ml_home <- as.numeric(ml_home)
  ml_away <- as.numeric(ml_away)

  p_mkt <- devig_two_way(ml_home, ml_away)$a
  p_use <- p_mkt + shrink * (p_raw - p_mkt)

  ev_h <- bet_ev(p_use - haircut,       ml_home)
  ev_a <- bet_ev((1 - p_use) - haircut, ml_away)

  in_band <- function(p) !is.na(p) & p >= band[1] & p <= band[2]
  ok_h <- in_band(p_raw)     & !is.na(ml_home) & american_to_decimal(ml_home) <= max_dec
  ok_a <- in_band(1 - p_raw) & !is.na(ml_away) & american_to_decimal(ml_away) <= max_dec
  ok_h <- !is.na(ok_h) & ok_h
  ok_a <- !is.na(ok_a) & ok_a

  take_h <- ok_h & !is.na(ev_h) & ev_h >= ev_threshold & (is.na(ev_a) | ev_h >= ev_a)
  take_a <- ok_a & !is.na(ev_a) & ev_a >= ev_threshold & !take_h

  side <- ifelse(is.na(p_raw) | is.na(p_mkt), NA_character_,
          ifelse(take_h, "home", ifelse(take_a, "away", NA_character_)))
  pick <- function(h, a) ifelse(is.na(side), NA_real_, ifelse(side == "home", h, a))

  tibble(
    side           = side,
    p_market       = p_mkt,
    p_model_shrunk = p_use,
    # The raw disagreement, signed toward the side taken. Reported, never
    # selected on -- see the moneyline section of 04_backtest.R.
    edge      = pick(p_raw - p_mkt, p_mkt - p_raw),
    ev        = pick(ev_h, ev_a),
    price     = pick(ml_home, ml_away),
    price_alt = pick(ml_away, ml_home)
  )
}

# ===========================================================================
# NAMES AND COLUMNS
# ===========================================================================

# Lowercase, collapse punctuation to "_": "Home Team (Close)" -> "home_team_close"
norm_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

clean_names <- function(df) {
  nm <- norm_key(names(df))
  nm[nm == ""] <- paste0("x", seq_along(nm))[nm == ""]
  nm <- make.unique(nm, sep = "_")
  setNames(df, nm)
}

# --- Canonical team codes ---------------------------------------------------
# Datasets disagree ("LA Lakers", "Lakers", "LAL", "L.A. Lakers"). Everything is
# funnelled into one 3-letter code so joins actually join. Relocated franchises
# map to their current code so history is continuous.
TEAM_ALIASES <- tribble(
  ~code,  ~aliases,
  "ATL",  "atlanta hawks|atlanta|hawks|atl",
  "BOS",  "boston celtics|boston|celtics|bos",
  "BKN",  "brooklyn nets|brooklyn|nets|bkn|brk|new jersey nets|new jersey|njn|nj",
  "CHA",  "charlotte hornets|charlotte|hornets|cha|cho|charlotte bobcats|bobcats",
  "CHI",  "chicago bulls|chicago|bulls|chi",
  "CLE",  "cleveland cavaliers|cleveland|cavaliers|cavs|cle",
  "DAL",  "dallas mavericks|dallas|mavericks|mavs|dal",
  "DEN",  "denver nuggets|denver|nuggets|den",
  "DET",  "detroit pistons|detroit|pistons|det",
  "GSW",  "golden state warriors|golden state|warriors|gsw|gs",
  "HOU",  "houston rockets|houston|rockets|hou",
  "IND",  "indiana pacers|indiana|pacers|ind",
  "LAC",  "los angeles clippers|la clippers|l a clippers|clippers|lac|la cli",
  "LAL",  "los angeles lakers|la lakers|l a lakers|lakers|lal",
  "MEM",  "memphis grizzlies|memphis|grizzlies|mem",
  "MIA",  "miami heat|miami|heat|mia",
  "MIL",  "milwaukee bucks|milwaukee|bucks|mil",
  "MIN",  "minnesota timberwolves|minnesota|timberwolves|wolves|min",
  "NOP",  "new orleans pelicans|new orleans|pelicans|nop|no|noh|nok|new orleans hornets|new orleans oklahoma city hornets",
  "NYK",  "new york knicks|new york|knicks|nyk|ny",
  "OKC",  "oklahoma city thunder|oklahoma city|thunder|okc|seattle supersonics|seattle|sonics|sea",
  "ORL",  "orlando magic|orlando|magic|orl",
  "PHI",  "philadelphia 76ers|philadelphia|76ers|sixers|phi",
  "PHX",  "phoenix suns|phoenix|suns|phx|pho",
  "POR",  "portland trail blazers|portland|trail blazers|blazers|por",
  "SAC",  "sacramento kings|sacramento|kings|sac",
  "SAS",  "san antonio spurs|san antonio|spurs|sas|sa",
  "TOR",  "toronto raptors|toronto|raptors|tor",
  "UTA",  "utah jazz|utah|jazz|uta|utah jaz",
  "WAS",  "washington wizards|washington|wizards|was|wsh|washington bullets|bullets"
)

.team_lookup <- local({
  lk <- TEAM_ALIASES %>%
    mutate(alias = strsplit(aliases, "\\|")) %>%
    unnest(alias) %>%
    mutate(key = norm_key(alias)) %>%
    distinct(key, .keep_all = TRUE)
  setNames(lk$code, lk$key)
})

# Returns the canonical code, or the (normalised, uppercased) input if unknown.
# Unknown names are surfaced by 01_load_data.R rather than silently dropped.
canonical_team <- function(x) {
  key <- norm_key(x)
  out <- unname(.team_lookup[key])
  ifelse(is.na(out), toupper(gsub("_", " ", key)), out)
}

# ===========================================================================
# ROLLING HELPERS -- all of them exclude the current row
# ===========================================================================
# These two enforce the project's central rule: a feature for game N may use
# games 1..N-1 and nothing else. Getting this wrong is the classic way a
# backtest invents a 60% win rate, and the error leaves no trace in the output,
# so both are covered directly in tests/run_tests.R.

# Mean of the previous `k` values (k = Inf for an expanding, season-to-date
# mean). Returns NA until at least one prior value exists. Missing values are
# skipped rather than counted as zero.
roll_mean_prior <- function(x, k = Inf) {
  n <- length(x)
  if (n == 0) return(numeric(0))
  xs <- ifelse(is.na(x), 0, x)
  cs <- c(0, cumsum(xs))
  cn <- c(0, cumsum(!is.na(x)))
  idx <- seq_len(n)
  hi  <- idx - 1L                      # window ends at the PREVIOUS game
  lo  <- if (is.infinite(k)) rep(0L, n) else pmax(0L, idx - 1L - k)
  cnt <- cn[hi + 1L] - cn[lo + 1L]
  ifelse(cnt > 0, (cs[hi + 1L] - cs[lo + 1L]) / pmax(cnt, 1), NA_real_)
}

# How many prior games fall within `days` days of this game. Assumes `date` is
# sorted ascending within the group it is called on.
games_in_prior_days <- function(date, days = 7) {
  d <- as.numeric(date)
  vapply(seq_along(d), function(i) {
    if (i == 1) return(0L)
    sum(d[seq_len(i - 1)] > d[i] - days)
  }, integer(1))
}

# ===========================================================================
# STATS
# ===========================================================================

# Wilson interval -- a well-behaved confidence interval for a win rate, unlike
# the naive normal interval which misbehaves on small samples.
wilson_ci <- function(wins, n, conf = 0.95) {
  if (n == 0) return(c(lower = NA_real_, upper = NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- wins / n
  d <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / d
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / d
  c(lower = max(0, centre - half), upper = min(1, centre + half))
}

# Bootstrap CI for any statistic of a vector (used for ROI).
bootstrap_ci <- function(x, statistic = mean, reps = 2000, conf = 0.95) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(c(lower = NA_real_, upper = NA_real_))
  boots <- replicate(reps, statistic(sample(x, length(x), replace = TRUE)))
  unname(quantile(boots, c((1 - conf) / 2, 1 - (1 - conf) / 2))) %>%
    setNames(c("lower", "upper"))
}

# Betting lines live on the half-point grid.
round_half <- function(x) round(x * 2) / 2

# Candidate raw datasets in data/, biggest first. Excludes the files this
# project writes there itself -- 06_news_signals.R drops manual_news.csv into
# data/, and without this it would sort ahead of the real dataset.
PROJECT_OWNED_CSVS <- c("manual_news.csv")

betting_csvs <- function(dir = "data") {
  f <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  f <- f[!basename(f) %in% PROJECT_OWNED_CSVS]
  f[order(file.size(f), decreasing = TRUE)]
}

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mae  <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)

# NBA seasons are labelled by their ending year: Oct 2024 -> Jun 2025 is 2025.
season_of <- function(date) {
  d <- as.Date(date)
  ifelse(is.na(d), NA_integer_, as.integer(year(d) + (month(d) >= 8)))
}

# Shared ggplot look, so every figure in output/figures/ matches.
theme_project <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey30"),
      panel.grid.minor = element_blank(),
      plot.caption  = element_text(colour = "grey45", hjust = 0)
    )
}

save_fig <- function(plot, filename, width = 9, height = 5.5) {
  path <- file.path(CFG$paths$figures_dir, filename)
  ggsave(path, plot, width = width, height = height, dpi = 150)
  info("figure: ", path)
  invisible(path)
}
