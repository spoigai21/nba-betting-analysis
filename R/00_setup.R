# ---------------------------------------------------------------------------
# 00_setup.R -- packages, paths, options
# ---------------------------------------------------------------------------
# Every other script starts by sourcing this file. It is safe to source many
# times (it only installs a package if it is genuinely missing).
#
# Run on its own the first time:   source("R/00_setup.R")
# ---------------------------------------------------------------------------

# --- Required packages ------------------------------------------------------
# tidyverse  : dplyr / ggplot2 / readr / tidyr / purrr / stringr
# lubridate  : dates
.required_pkgs <- c("tidyverse", "lubridate")

# Optional: only needed for live forward-testing (05) and news signals (06).
.optional_pkgs <- c("jsonlite", "hoopR")

install_if_missing <- function(pkgs, optional = FALSE) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) return(invisible(TRUE))
  if (optional) {
    message(
      "Optional package(s) not installed: ", paste(missing, collapse = ", "),
      "\n  Install with: install.packages(c(",
      paste0('"', missing, '"', collapse = ", "), "))"
    )
    return(invisible(FALSE))
  }
  message("Installing missing package(s): ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
  invisible(TRUE)
}

install_if_missing(.required_pkgs)
install_if_missing(.optional_pkgs, optional = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# --- Options ----------------------------------------------------------------
options(
  dplyr.summarise.inform = FALSE,
  readr.show_col_types   = FALSE,
  stringsAsFactors       = FALSE
)

# Reproducibility. Anything random in this project (synthetic data, bootstrap
# confidence intervals) is seeded so results are identical run to run.
set.seed(20260805)

# --- .env -------------------------------------------------------------------
# Secrets (e.g. ODDS_API_KEY=abc123) live in .env, which is gitignored.
load_dotenv <- function(path = ".env") {
  if (!file.exists(path)) return(invisible(FALSE))
  lines <- readLines(path, warn = FALSE)
  lines <- lines[!grepl("^\\s*(#|$)", lines)]
  for (ln in lines) {
    kv <- sub("^\\s*(export\\s+)?", "", ln)
    key <- sub("=.*$", "", kv)
    val <- sub("^[^=]*=", "", kv)
    key <- trimws(key)
    val <- trimws(val)
    val <- gsub('^["\']|["\']$', "", val)          # strip surrounding quotes
    if (nzchar(key)) do.call(Sys.setenv, setNames(list(val), key))
  }
  invisible(TRUE)
}
load_dotenv()

# --- Project files ----------------------------------------------------------
source("R/config.R")
source("R/utils.R")
source("R/track_record.R")   # schema + safe append, shared by 05 and 07

for (d in c(CFG$paths$processed_dir, CFG$paths$output_dir, CFG$paths$figures_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

invisible(TRUE)
