# ---------------------------------------------------------------------------
# track_record.R -- the auditable log: schema, safe append, standings
# ---------------------------------------------------------------------------
# This lives on its own because TWO scripts write to the track record:
# 05_forward.R logs game bets and 07_usage_model.R logs player-prop
# projections. Both need identical append semantics, and neither should have
# to source the other to get them.
#
# The invariants that make this file an audit trail rather than a spreadsheet:
#
#   1. append_track_rows() only ever APPENDS. It never edits a logged row.
#   2. A (date, game, player, stat, market, model_version) that is already in
#      the file is dropped, not re-logged. The FIRST timestamp is the honest
#      one -- re-logging would let you quietly restate a prediction.
#   3. Results are filled into blank cells only (see update_results() in 05).
#   4. Column types are pinned, not guessed. An all-blank result column would
#      otherwise be read back as logical and refuse to bind with real rows.
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")

# The first twelve columns are the schema documented in README. Everything
# after them is provenance: how the number was produced, and what it was
# graded against.
TRACK_COLS <- c(
  # --- the documented core ---
  "date", "game", "player", "stat", "prediction", "line", "odds",
  "bet", "result", "win_loss", "units", "timestamp",
  # --- provenance ---
  # clv_points is recorded in the NATURAL UNIT OF ITS MARKET: points for totals
  # and spreads, probability for the moneyline (where the "line" is the
  # market's de-vigged win probability). Anything that averages this column
  # must therefore group by market, or it will add points to probabilities.
  "market", "edge", "model_version", "event_id",
  "line_close", "clv_points",
  # --- news attribution (06) ---
  # prediction_raw is the model's number BEFORE any hand-entered news
  # adjustment; prediction is what the bet was actually made on. Keeping both
  # is what lets evaluate_news_contribution() grade the reads separately from
  # the model, which is the whole reason for entering them in their own file.
  "prediction_raw", "news_adj",
  "notes"
)

TRACK_COL_TYPES <- readr::cols(
  date = readr::col_date(), game = readr::col_character(),
  player = readr::col_character(), stat = readr::col_character(),
  prediction = readr::col_double(), line = readr::col_double(),
  odds = readr::col_double(), bet = readr::col_character(),
  result = readr::col_double(), win_loss = readr::col_character(),
  units = readr::col_double(), timestamp = readr::col_character(),
  market = readr::col_character(), edge = readr::col_double(),
  model_version = readr::col_character(), event_id = readr::col_character(),
  line_close = readr::col_double(), clv_points = readr::col_double(),
  prediction_raw = readr::col_double(), news_adj = readr::col_double(),
  notes = readr::col_character()
)

empty_track_record <- function() {
  tibble(
    date = as.Date(character()), game = character(), player = character(),
    stat = character(), prediction = double(), line = double(), odds = double(),
    bet = character(), result = double(), win_loss = character(),
    units = double(), timestamp = character(), market = character(),
    edge = double(), model_version = character(), event_id = character(),
    line_close = double(), clv_points = double(),
    prediction_raw = double(), news_adj = double(), notes = character()
  )
}

# Fill in any columns a caller did not supply, with the right type, and put
# them in the canonical order. Lets 05 and 07 build only the columns that mean
# something for their market.
conform_track_rows <- function(rows) {
  proto <- empty_track_record()
  for (cc in TRACK_COLS) {
    if (!cc %in% names(rows)) rows[[cc]] <- proto[[cc]][NA_integer_]
  }
  rows %>% select(all_of(TRACK_COLS))
}

read_track_record <- function(path = CFG$paths$track_record) {
  if (!file.exists(path)) return(empty_track_record())
  tr <- suppressWarnings(
    readr::read_csv(path, show_col_types = FALSE, col_types = TRACK_COL_TYPES))
  if (!nrow(tr)) return(empty_track_record())
  # A record written before these columns existed is still readable.
  conform_track_rows(tr)
}

# The identity of a prediction. Props have a player and no opponent; game bets
# have a game and no player. Including both makes one key work for both.
track_key <- function(d) {
  paste(d$date, d$game, coalesce(d$player, ""), d$stat,
        d$market, d$model_version, sep = "|")
}

# The only supported way to write predictions. Returns the rows that were
# actually new (invisibly), so callers can report honestly.
append_track_rows <- function(rows, dry_run = FALSE, path = CFG$paths$track_record,
                              quiet = FALSE) {
  if (!NROW(rows)) return(invisible(empty_track_record()))
  rows <- conform_track_rows(rows)

  existing <- read_track_record(path)
  fresh <- rows[!track_key(rows) %in% track_key(existing), , drop = FALSE]

  if (!quiet && nrow(fresh) < nrow(rows))
    info(nrow(rows) - nrow(fresh),
         " prediction(s) already logged earlier; keeping the original timestamp")

  if (dry_run) {
    if (!quiet) info("dry run -- nothing written")
    return(invisible(fresh))
  }
  if (!nrow(fresh)) return(invisible(fresh))

  readr::write_csv(bind_rows(existing, fresh) %>% select(all_of(TRACK_COLS)), path)
  if (!quiet) {
    info("appended ", nrow(fresh), " row(s) to ", path)
    info("timestamp written: ", fresh$timestamp[1], " -- before tip-off, on purpose.")
  }
  invisible(fresh)
}

# Overwrite the whole file. Used only by the two functions that are ALLOWED to
# edit existing rows -- update_results() and record_closing_lines() -- both of
# which fill blank cells and never touch a settled one.
write_track_record <- function(tr, path = CFG$paths$track_record) {
  readr::write_csv(conform_track_rows(tr), path)
  invisible(tr)
}

# ===========================================================================
# Standings
# ===========================================================================

summarise_bets_simple <- function(b) {
  graded <- b %>% filter(.data$win_loss %in% c("win", "loss"))
  # Props logged without a line are projections, not bets: they have no odds
  # and cannot be graded, so they must not dilute ROI.
  staked <- b %>% filter(!is.na(.data$odds))
  list(
    n = nrow(b), w = sum(graded$win_loss == "win"), l = sum(graded$win_loss == "loss"),
    p = sum(b$win_loss == "push", na.rm = TRUE),
    win_rate = if (nrow(graded)) mean(graded$win_loss == "win") else NA_real_,
    units = sum(b$units, na.rm = TRUE),
    roi = if (nrow(staked)) sum(staked$units, na.rm = TRUE) / nrow(staked) else NA_real_,
    n_staked = nrow(staked),
    break_even = mean(break_even_prob(b$odds), na.rm = TRUE)
  )
}

report_track_record <- function(tr = read_track_record()) {
  settled <- tr %>% filter(!is.na(.data$win_loss))
  step("Live track record")
  if (!nrow(settled)) { info("no settled bets yet"); return(invisible(NULL)) }

  s <- summarise_bets_simple(settled)
  message(sprintf("   %d settled | %d-%d-%d | win %.1f%% | units %+.2f | ROI %+.2f%%",
                  s$n, s$w, s$l, s$p, 100 * s$win_rate, s$units, 100 * s$roi))
  message(sprintf("   break-even at these prices: %.1f%%", 100 * s$break_even))

  if (s$n < CFG$backtest$min_bets_for_conclusion)
    message(sprintf("   %d more settled bets before this number means anything.",
                    CFG$backtest$min_bets_for_conclusion - s$n))

  by_market <- settled %>% group_by(.data$market) %>%
    summarise(n = n(), units = sum(.data$units, na.rm = TRUE),
              win_rate = mean(.data$win_loss == "win", na.rm = TRUE), .groups = "drop")
  if (nrow(by_market) > 1) { message("   by market:"); print(by_market) }

  # Grouped by market on purpose: clv_points is in points for the point
  # markets and in probability for the moneyline, so a single average of the
  # column would be adding two different units together.
  clv <- settled %>% filter(!is.na(.data$clv_points))
  if (nrow(clv)) {
    message("   closing-line value (in each market's own unit):")
    clv %>% group_by(.data$market) %>%
      summarise(n = n(), mean_clv = mean(.data$clv_points),
                beat_close = mean(.data$clv_points > 0), .groups = "drop") %>%
      pwalk(function(market, n, mean_clv, beat_close)
        message(sprintf("     %-10s %+.3f %s, beat the close %.1f%% (n=%d)",
                        market, mean_clv,
                        if (market == "moneyline") "prob" else "pts",
                        100 * beat_close, n)))
  } else {
    message("   CLV not recorded yet -- see record_closing_lines()")
  }

  by_v <- settled %>% group_by(.data$model_version) %>%
    summarise(n = n(), units = sum(.data$units, na.rm = TRUE), .groups = "drop")
  if (nrow(by_v) > 1) { message("   by model version:"); print(by_v) }
  invisible(s)
}
