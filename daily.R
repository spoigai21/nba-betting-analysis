# ---------------------------------------------------------------------------
# daily.R -- the in-season loop, in the order it has to happen
# ---------------------------------------------------------------------------
#   Rscript daily.R morning   results in, grade yesterday, report
#   Rscript daily.R log       predictions for tonight  (a few hours before tip)
#   Rscript daily.R close     capture closing lines    (~15 min before tip)
#
# Order is not arbitrary. Predictions must be built on results that already
# include yesterday, closing lines must be captured before tip-off or CLV is
# gone for good, and grading must come after the games finish. Running these
# out of order does not error -- it quietly logs a worse record.
#
# See RUNBOOK.md for when to start, what each step should print, and what to do
# when one of them refuses.
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[1] else "morning"

suppressMessages(source("R/05_forward.R"))

if (mode == "morning") {
  # Fold in last night's results FIRST, so team form is current before
  # anything is predicted from it, then grade what those results settle.
  refresh_results()
  source("R/02_features.R")          # rebuild team form on the new results
  update_results()
  report_track_record()
  invisible(report_preflight(forward_preflight(team_games)))

} else if (mode == "log") {
  # log_predictions() runs its own pre-flight and refuses if it fails.
  log_predictions(dry_run = TRUE)    # look at this before the real one
  log_predictions()

} else if (mode == "close") {
  record_closing_lines()

} else {
  stop("usage: Rscript daily.R [morning|log|close]", call. = FALSE)
}
