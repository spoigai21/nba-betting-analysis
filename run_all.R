# ---------------------------------------------------------------------------
# run_all.R -- the whole backtest pipeline, start to finish
# ---------------------------------------------------------------------------
# From RStudio:   source("run_all.R")
# From a shell:   Rscript run_all.R
#
# Steps 01-04 are the historical half: load, build features, fit, backtest.
# They are safe to re-run at any time and never touch output/track_record.csv.
#
# The forward test (05) and news signals (06) are interactive and are NOT run
# here on purpose -- they write a permanent, timestamped record and should be
# invoked deliberately:
#
#   source("R/05_forward.R"); log_predictions(dry_run = TRUE)
# ---------------------------------------------------------------------------

t0 <- Sys.time()

source("R/00_setup.R")

# No data yet? Fall back to the synthetic set so the pipeline still runs.
if (!length(betting_csvs("data"))) {
  warn("no CSV in data/ -- generating a synthetic dataset instead")
  warn("its market is efficient by construction, so expect 'no edge'")
  source("R/99_make_sample_data.R")
}

source("R/01_load_data.R")
source("R/02_features.R")
source("R/03_model.R")
source("R/04_backtest.R")
source("R/08_diagnostics.R")   # where does it fail? -- reads only, changes nothing

step("Done in ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s")
info("figures : ", CFG$paths$figures_dir)
info("bets    : ", file.path(CFG$paths$output_dir, "backtest_bets.csv"))
info("summary : ", file.path(CFG$paths$output_dir, "backtest_summary.csv"))
info("segments: ", file.path(CFG$paths$output_dir, "diagnostics_segments.csv"))
message("")
info("Next, the honest half:  source(\"R/05_forward.R\"); log_predictions(dry_run = TRUE)")
