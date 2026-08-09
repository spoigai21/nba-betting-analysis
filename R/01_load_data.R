# ---------------------------------------------------------------------------
# 01_load_data.R -- raw CSV  ->  one tidy row per game
# ---------------------------------------------------------------------------
# Public NBA betting datasets all carry the same information under different
# column names ("spread" / "home_line_close" / "handicap"). Rather than hard-code
# one dataset's schema, this script INFERS the mapping, prints what it inferred,
# and lets you override any guess in R/config.R.
#
# Output schema (data/processed/games.rds), one row per game:
#
#   game_id, date, season, home_team, away_team,
#   home_score, away_score, total_points, margin, home_win,
#   spread_close, total_close, ml_home, ml_away,          <- closing market
#   spread_open,  total_open,                             <- opening market
#   price_spread_home/away, price_over, price_under       <- the vig
#
# Conventions used everywhere downstream:
#   margin       = home_score - away_score   (positive = home won)
#   spread_close = the HOME side's number    (negative = home favoured)
#   a home spread bet wins when  margin + spread_close > 0
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")

# ===========================================================================
# 1. Which column is which?
# ===========================================================================
# exact   : normalised names matched outright, best candidate first
# require : token sets -- a column matches if it contains ALL tokens of any set
# avoid   : tokens that disqualify a column (stops "spread_open" being read as
#           the closing spread, or an away column being read as home)

FIELD_SPECS <- list(
  date = list(
    exact   = c("date", "game_date", "gamedate", "commence_time", "start_date",
                "game_datetime", "datetime", "date_time"),
    require = list(c("date"), c("commence", "time")),
    avoid   = c("update", "updated", "birth", "created")
  ),
  season = list(
    exact   = c("season", "season_year", "year", "seas"),
    require = list(c("season")),
    avoid   = c("type", "stage")
  ),
  home_team = list(
    exact   = c("home_team", "home", "hometeam", "team_home", "home_name",
                "home_team_name", "home_abbr", "home_club", "htm"),
    require = list(c("home", "team"), c("home", "name"), c("home", "abbr")),
    avoid   = c("away", "visitor", "road", "score", "pts", "points", "id", "rest")
  ),
  away_team = list(
    exact   = c("away_team", "away", "awayteam", "team_away", "visitor",
                "visitor_team", "visitor_team_name", "road_team", "away_name",
                "away_abbr", "atm"),
    require = list(c("away", "team"), c("visitor", "team"), c("road", "team"),
                   c("away", "name"), c("away", "abbr")),
    avoid   = c("home", "score", "pts", "points", "id", "rest")
  ),
  home_score = list(
    exact   = c("home_score", "score_home", "pts_home", "home_pts", "home_points",
                "points_home", "final_home", "home_final", "home_team_score",
                "hscore", "home_score_final"),
    require = list(c("home", "score"), c("home", "pts"), c("home", "points"),
                   c("home", "final")),
    avoid   = c("away", "visitor", "spread", "line", "open", "close", "half")
  ),
  away_score = list(
    exact   = c("away_score", "score_away", "pts_away", "away_pts", "away_points",
                "points_away", "final_away", "away_final", "away_team_score",
                "visitor_score", "visitor_points", "ascore"),
    require = list(c("away", "score"), c("away", "pts"), c("away", "points"),
                   c("visitor", "score"), c("away", "final")),
    avoid   = c("home", "spread", "line", "open", "close", "half")
  ),

  # --- opening market (matched BEFORE closing, so "open" names are claimed) ---
  spread_open = list(
    exact   = c("spread_open", "open_spread", "opening_spread", "home_spread_open",
                "spread_home_open", "home_line_open", "opening_line",
                "open_spread_home", "spread_open_home"),
    require = list(c("spread", "open"), c("line", "open"), c("handicap", "open")),
    avoid   = c("away", "visitor", "price", "odds", "juice", "vig", "total", "ou")
  ),
  total_open = list(
    exact   = c("total_open", "open_total", "opening_total", "ou_open",
                "open_over_under", "opening_over_under", "open_ou"),
    require = list(c("total", "open"), c("ou", "open"), c("over", "under", "open")),
    avoid   = c("price", "odds", "juice", "spread")
  ),

  # --- closing market -------------------------------------------------------
  spread_close = list(
    exact   = c("spread_close", "close_spread", "closing_spread", "home_spread_close",
                "spread_home_close", "home_line_close", "closing_line",
                "spread", "home_spread", "spread_home", "line", "home_line",
                "handicap", "home_handicap", "point_spread", "spread_favorite"),
    require = list(c("spread"), c("handicap")),
    avoid   = c("open", "opening", "away", "visitor", "price", "odds", "juice",
                "vig", "result", "cover", "margin", "movement")
  ),
  total_close = list(
    exact   = c("total_close", "close_total", "closing_total", "total",
                "over_under", "ou", "o_u", "total_line", "game_total",
                "closing_total_line", "total_points_line"),
    require = list(c("total"), c("over", "under"), c("ou")),
    avoid   = c("open", "opening", "price", "odds", "juice", "vig", "result",
                "score", "points", "spread")
  ),
  # A bare "Home Odds" is how a large share of public exports label the
  # moneyline. It is only safe to claim it once "spread"/"total"/"over"/"under"
  # are in `avoid`, so that spread juice and total juice cannot be swept up by
  # the same rule -- those always carry their market in the name.
  ml_home = list(
    exact   = c("moneyline_home", "home_moneyline", "ml_home", "home_ml",
                "money_line_home", "home_money_line", "ml_home_close",
                "moneyline_home_close", "home_odds", "odds_home",
                "home_odds_close", "home_price", "price_home"),
    require = list(c("moneyline", "home"), c("ml", "home"), c("money", "line", "home"),
                   c("home", "odds"), c("home", "price")),
    avoid   = c("away", "visitor", "open", "spread", "total", "over", "under",
                "line", "handicap")
  ),
  ml_away = list(
    exact   = c("moneyline_away", "away_moneyline", "ml_away", "away_ml",
                "money_line_away", "away_money_line", "visitor_moneyline",
                "ml_away_close", "away_odds", "odds_away", "away_odds_close",
                "away_price", "price_away", "visitor_odds"),
    require = list(c("moneyline", "away"), c("ml", "away"), c("moneyline", "visitor"),
                   c("away", "odds"), c("away", "price"), c("visitor", "odds")),
    avoid   = c("home", "open", "spread", "total", "over", "under",
                "line", "handicap")
  ),

  # --- prices (the vig) -----------------------------------------------------
  price_spread_home = list(
    exact   = c("spread_price_home", "home_spread_odds", "spread_odds_home",
                "home_spread_price", "price_spread_home", "spread_home_price",
                "home_spread_juice", "spread_home_odds"),
    require = list(c("spread", "home", "price"), c("spread", "home", "odds"),
                   c("spread", "home", "juice")),
    avoid   = c("away", "visitor", "open")
  ),
  price_spread_away = list(
    exact   = c("spread_price_away", "away_spread_odds", "spread_odds_away",
                "away_spread_price", "price_spread_away", "spread_away_price",
                "away_spread_juice", "spread_away_odds"),
    require = list(c("spread", "away", "price"), c("spread", "away", "odds"),
                   c("spread", "away", "juice")),
    avoid   = c("home", "open")
  ),
  price_over = list(
    exact   = c("over_odds", "total_over_odds", "over_price", "price_over",
                "total_price_over", "over_juice", "odds_over"),
    require = list(c("over", "price"), c("over", "odds"), c("over", "juice")),
    avoid   = c("under", "open", "spread")
  ),
  price_under = list(
    exact   = c("under_odds", "total_under_odds", "under_price", "price_under",
                "total_price_under", "under_juice", "odds_under"),
    require = list(c("under", "price"), c("under", "odds"), c("under", "juice")),
    avoid   = c("over", "open", "spread")
  )
)

match_one_field <- function(available, spec) {
  hit <- available[available %in% spec$exact]
  if (length(hit)) return(hit[order(match(hit, spec$exact))][1])

  toks <- strsplit(available, "_")
  ok <- vapply(seq_along(available), function(i) {
    t <- toks[[i]]
    if (length(spec$avoid) && any(spec$avoid %in% t)) return(FALSE)
    any(vapply(spec$require, function(rq) all(rq %in% t), logical(1)))
  }, logical(1))
  if (!any(ok)) return(NA_character_)
  cand <- available[ok]
  cand[order(nchar(cand))][1]     # shortest match = least decorated name
}

detect_columns <- function(df, overrides = CFG$column_overrides) {
  cols <- names(df)
  map  <- setNames(rep(NA_character_, length(FIELD_SPECS)), names(FIELD_SPECS))

  # Manual overrides win outright. Accept either the raw or normalised name.
  for (fld in names(overrides)) {
    want <- norm_key(overrides[[fld]])
    if (!fld %in% names(map)) { warn("override for unknown field: ", fld); next }
    if (!want %in% cols) { warn("override column not in file: ", overrides[[fld]]); next }
    map[[fld]] <- want
  }

  for (fld in names(FIELD_SPECS)) {
    if (!is.na(map[[fld]])) next
    available <- setdiff(cols, map[!is.na(map)])
    map[[fld]] <- match_one_field(available, FIELD_SPECS[[fld]])
  }
  map
}

print_mapping <- function(map, df) {
  step("Column mapping")
  for (fld in names(map)) {
    src <- map[[fld]]
    message(sprintf("   %-18s <- %s", fld, if (is.na(src)) "(not found)" else src))
  }
  unused <- setdiff(names(df), map[!is.na(map)])
  if (length(unused)) {
    info("unused columns: ", paste(head(unused, 25), collapse = ", "),
         if (length(unused) > 25) sprintf(" ... (+%d more)", length(unused) - 25) else "")
  }
  info("Wrong guess? Set it in R/config.R, e.g.")
  info('  column_overrides = list(spread_close = "home_line_close")')
}

# ===========================================================================
# 2. Reading and parsing
# ===========================================================================

find_raw_csv <- function(path = CFG$paths$raw_csv) {
  if (file.exists(path)) return(path)
  candidates <- betting_csvs("data")
  if (!length(candidates)) {
    stop(
      "No CSV found in data/.\n",
      "  Either download a Kaggle betting dataset into data/ (see INSTRUCTIONS.md Step 6),\n",
      "  or generate a synthetic one to try the pipeline:\n",
      "      source(\"R/99_make_sample_data.R\")",
      call. = FALSE
    )
  }
  if (length(candidates) > 1)
    warn("several CSVs in data/; using the largest, ", candidates[1],
         " (set CFG$paths$raw_csv to choose)")
  candidates[1]
}

# Dates arrive as ISO strings, US strings, ISO datetimes, or bare numbers
# like 20211019. Try each until one parses most of the column.
parse_dates <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  if (is.numeric(x)) {
    if (all(x > 19000101 & x < 21001231, na.rm = TRUE))
      return(as.Date(as.character(x), format = "%Y%m%d"))
    # Excel serial dates
    if (all(x > 20000 & x < 60000, na.rm = TRUE))
      return(as.Date(x, origin = "1899-12-30"))
  }

  chr <- as.character(x)
  formats <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y",
               "%Y%m%d", "%b %d, %Y", "%d %b %Y")
  best <- NULL; best_ok <- -1
  base <- sub("[T ].*$", "", trimws(chr))          # drop any time component
  for (f in formats) {
    d <- suppressWarnings(as.Date(base, format = f))
    ok <- sum(!is.na(d))
    if (ok > best_ok) { best_ok <- ok; best <- d }
  }
  if (best_ok < 0.8 * sum(!is.na(chr)))
    warn("only ", best_ok, "/", sum(!is.na(chr)), " dates parsed -- check the date column")
  best
}

# "-3.5", "+3.5", "PK", "pick'em", "-110" as text, "3½"
parse_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  chr <- trimws(as.character(x))
  chr[grepl("^(pk|pick|pick'?em|even|ev)$", tolower(chr))] <- "0"
  chr <- gsub("½", ".5", chr)
  # Strip everything that cannot be part of a number.
  #
  # The "-" MUST come last in this class and MUST NOT be backslash-escaped.
  # Inside a bracket expression R treats a backslash as a literal character
  # rather than an escape, so the obvious-looking "[^0-9+\\-\\.eE]" quietly
  # removes the minus sign from every value -- turning "-3.5" into 3.5 and
  # "-110" into 110. That flips every favourite into an underdog and every
  # price into its opposite, with no error and nothing odd-looking downstream.
  # Datasets that store these columns as text are exactly the ones this
  # function exists for, so the bug only appears on real data. Covered by
  # tests/run_tests.R.
  chr <- gsub("[^0-9eE+.-]", "", chr)
  chr[chr %in% c("", "+", "-", ".")] <- NA
  suppressWarnings(as.numeric(chr))
}

# --- long ("one row per team") -> wide ("one row per game") ------------------
# Some datasets store two rows per game with a home/away flag. Detect that and
# pivot before mapping.
detect_long_format <- function(df) {
  cols <- names(df)
  has_team <- any(c("team", "team_name", "team_abbr", "tm") %in% cols)
  flag_col <- intersect(c("vh", "home_away", "is_home", "home_or_away",
                          "location", "venue", "site", "h_a"), cols)
  has_team && length(flag_col) > 0
}

reshape_long_to_wide <- function(df) {
  cols <- names(df)
  team_col  <- intersect(c("team", "team_name", "team_abbr", "tm"), cols)[1]
  flag_col  <- intersect(c("vh", "home_away", "is_home", "home_or_away",
                           "location", "venue", "site", "h_a"), cols)[1]
  date_col  <- intersect(c("date", "game_date", "gamedate"), cols)[1]
  score_col <- intersect(c("final", "score", "pts", "points", "final_score",
                           "team_score"), cols)[1]
  if (is.na(date_col) || is.na(score_col))
    stop("Long-format file detected but the date or score column could not be found. ",
         "Set them in CFG$column_overrides, or reshape the file to one row per game.",
         call. = FALSE)

  flag <- tolower(as.character(df[[flag_col]]))
  is_home <- flag %in% c("h", "home", "1", "true", "vs", "vs.")

  side_cols <- setdiff(cols, c(date_col, flag_col))
  wide <- df %>%
    mutate(.is_home = is_home,
           .side = ifelse(.data$.is_home, "home", "away"),
           .date = parse_dates(.data[[date_col]])) %>%
    group_by(.data$.date) %>%
    # These files list a game's two rows adjacently (the near-universal
    # convention), so pair them by position. Do NOT pair on cumsum(is_home):
    # that splits the pair whenever the home row comes first.
    mutate(.pair = ceiling(row_number() / 2)) %>%
    ungroup() %>%
    select(-all_of(flag_col)) %>%
    pivot_wider(id_cols = c(".date", ".pair"),
                names_from = ".side",
                values_from = all_of(side_cols),
                names_glue = "{.value}_{.side}")

  wide %>%
    rename(date = ".date") %>%
    select(-".pair") %>%
    clean_names()
}

# ===========================================================================
# 3. Build the tidy table
# ===========================================================================

build_games <- function(df, map) {
  need <- c("date", "home_team", "away_team", "home_score", "away_score")
  missing <- need[is.na(map[need])]
  if (length(missing))
    stop("Could not identify required column(s): ", paste(missing, collapse = ", "),
         "\n  Set them in R/config.R under column_overrides.", call. = FALSE)

  pull_num <- function(fld) if (is.na(map[[fld]])) NA_real_ else parse_number(df[[map[[fld]]]])

  g <- tibble(
    date       = parse_dates(df[[map[["date"]]]]),
    home_team  = canonical_team(df[[map[["home_team"]]]]),
    away_team  = canonical_team(df[[map[["away_team"]]]]),
    home_score = pull_num("home_score"),
    away_score = pull_num("away_score"),

    spread_close = pull_num("spread_close"),
    total_close  = pull_num("total_close"),
    ml_home      = pull_num("ml_home"),
    ml_away      = pull_num("ml_away"),
    spread_open  = pull_num("spread_open"),
    total_open   = pull_num("total_open"),

    price_spread_home = pull_num("price_spread_home"),
    price_spread_away = pull_num("price_spread_away"),
    price_over        = pull_num("price_over"),
    price_under       = pull_num("price_under")
  )

  g$season <- if (!is.na(map[["season"]])) {
    s <- parse_number(df[[map[["season"]]]])
    # "2021" for a 2021-22 season -> store the ending year, 2022
    if (all(s < 2100 & s > 1900, na.rm = TRUE) &&
        median(s, na.rm = TRUE) < median(season_of(g$date), na.rm = TRUE) - 0.5) {
      s + 1L
    } else as.integer(s)
  } else season_of(g$date)
  g$season[is.na(g$season)] <- season_of(g$date[is.na(g$season)])

  g %>%
    filter(!is.na(.data$date), !is.na(.data$home_team), !is.na(.data$away_team)) %>%
    mutate(
      total_points = .data$home_score + .data$away_score,
      margin       = .data$home_score - .data$away_score,
      home_win     = as.integer(.data$margin > 0),
      completed    = !is.na(.data$home_score) & !is.na(.data$away_score),
      game_id      = paste0(format(.data$date, "%Y%m%d"), "_",
                            .data$away_team, "_at_", .data$home_team)
    ) %>%
    arrange(.data$date, .data$game_id) %>%
    distinct(.data$game_id, .keep_all = TRUE) %>%
    select("game_id", "date", "season", "home_team", "away_team",
           "home_score", "away_score", "total_points", "margin", "home_win",
           "completed", "spread_close", "total_close", "ml_home", "ml_away",
           "spread_open", "total_open", "price_spread_home", "price_spread_away",
           "price_over", "price_under")
}

# ===========================================================================
# 4. Sanity checks -- catch the mistakes that silently ruin a backtest
# ===========================================================================

validate_games <- function(g) {
  step("Data checks")
  played <- g %>% filter(.data$completed)

  # -- team names ----------------------------------------------------------
  # Anything that fails to map is either an exhibition (All-Star, Rising Stars)
  # or a name this project has no alias for. Exhibitions must not reach the
  # model: a 180-point All-Star game would wreck every pace and defence stat.
  unknown <- setdiff(unique(c(g$home_team, g$away_team)), TEAM_ALIASES$code)
  if (length(unknown)) {
    bad <- g$home_team %in% unknown | g$away_team %in% unknown
    share <- mean(bad)
    warn("unrecognised team names: ", paste(head(unknown, 10), collapse = ", "),
         if (length(unknown) > 10) sprintf(" (+%d more)", length(unknown) - 10) else "")
    if (share > 0.05) {
      # Too many to be exhibitions -- this is a mapping problem. Keep the rows
      # rather than silently deleting most of the dataset.
      warn(scales::percent(share, 0.1), " of games involve an unmapped team. ",
           "That is a naming mismatch, not exhibition games -- add aliases to ",
           "TEAM_ALIASES in R/utils.R. Rows KEPT so nothing is lost silently.")
    } else {
      info("dropping ", sum(bad), " game(s) involving unmapped teams ",
           "(All-Star and other exhibitions look like this)")
      g <- g[!bad, ]
      played <- g %>% filter(.data$completed)
    }
  } else {
    info("all team names mapped to canonical codes")
  }

  # -- scores --------------------------------------------------------------
  if (nrow(played)) {
    med <- median(played$total_points, na.rm = TRUE)
    if (med < 150 || med > 280)
      warn("median total points is ", round(med, 1),
           " -- the score columns may be wrong")
    else info("median game total: ", round(med, 1), " points")
    if (any(played$margin == 0, na.rm = TRUE))
      warn(sum(played$margin == 0, na.rm = TRUE),
           " games with a 0 margin -- NBA games cannot tie; check the score columns")
  }

  # -- spread orientation --------------------------------------------------
  # The home spread must be NEGATIVELY correlated with the home margin: home
  # favoured (spread -6) goes with home winning (margin +8). A positive
  # correlation means the file quotes the away/favourite side.
  if (sum(!is.na(played$spread_close) & !is.na(played$margin)) > 30) {
    r <- cor(played$spread_close, played$margin, use = "complete.obs")
    info("cor(spread_close, margin) = ", round(r, 3),
         " (expect roughly -0.45: spread sd ~6 against margin sd ~13)")
    if (r > 0.2) {
      if (isTRUE(CFG$auto_fix_spread_sign)) {
        warn("spread appears to be quoted from the AWAY side -- flipping sign")
        g$spread_close <- -g$spread_close
        g$spread_open  <- -g$spread_open
      } else {
        warn("spread sign looks inverted, but auto_fix_spread_sign is FALSE")
      }
    }
    if (all(played$spread_close <= 0, na.rm = TRUE))
      warn("every spread is <= 0. This file probably stores the FAVOURITE's spread ",
           "plus a separate 'favourite' column; you will need to rebuild the home ",
           "spread by hand before the backtest is meaningful.")
  }

  # -- totals --------------------------------------------------------------
  if (sum(!is.na(played$total_close)) > 30) {
    mt <- median(played$total_close, na.rm = TRUE)
    info("median closing total line: ", mt)
    if (mt < 150 || mt > 280)
      warn("closing total line looks wrong (median ", mt, ")")
    info("line vs result bias: ",
         round(mean(played$total_points - played$total_close, na.rm = TRUE), 2),
         " points (a fair market sits near 0)")
  }

  # -- prices --------------------------------------------------------------
  price_cols <- c("price_spread_home", "price_spread_away", "price_over", "price_under")
  have <- price_cols[vapply(price_cols, function(cc) any(!is.na(g[[cc]])), logical(1))]
  if (!length(have))
    warn("no odds/price columns found -- the backtest will assume ",
         CFG$backtest$default_price, " on every bet")
  else info("price columns present: ", paste(have, collapse = ", "))

  # -- opening lines -------------------------------------------------------
  if (all(is.na(g$spread_open)) && all(is.na(g$total_open)))
    warn("no OPENING lines in this dataset. Closing-line value cannot be computed ",
         "from history; 04_backtest.R will report CLV as unavailable.")

  # -- coverage ------------------------------------------------------------
  info("games: ", nrow(g), "  (", sum(g$completed), " completed)")
  info("seasons: ", paste(range(g$season, na.rm = TRUE), collapse = " to "))
  info("dates: ", paste(format(range(g$date, na.rm = TRUE)), collapse = " to "))
  miss <- g %>% summarise(across(c("spread_close", "total_close", "ml_home"),
                                 ~ mean(is.na(.x))))
  info("missing rates -- spread ", scales::percent(miss$spread_close, 0.1),
       ", total ", scales::percent(miss$total_close, 0.1),
       ", moneyline ", scales::percent(miss$ml_home, 0.1))

  g
}

# ===========================================================================
# 5. Run
# ===========================================================================

load_and_clean <- function(path = NULL) {
  path <- path %||% find_raw_csv()
  step("Loading ", path)
  raw <- readr::read_csv(path, show_col_types = FALSE, na = c("", "NA", "N/A", "-", "NL")) %>%
    clean_names()
  info(nrow(raw), " rows x ", ncol(raw), " columns")

  if (detect_long_format(raw)) {
    info("detected one-row-per-team layout; reshaping to one row per game")
    raw <- reshape_long_to_wide(raw)
  }

  map <- detect_columns(raw)
  print_mapping(map, raw)

  games <- build_games(raw, map) %>% validate_games()

  saveRDS(games, CFG$paths$games_rds)
  readr::write_csv(games, CFG$paths$games_csv)
  step("Saved ", CFG$paths$games_rds)
  games
}

# `games` is the table every later script starts from.
#
# Guarded so that tests -- and anything else wanting only the column-detection
# and parsing functions above -- can source this file without triggering a full
# load of whatever happens to be sitting in data/. Nothing in the normal
# pipeline sets this option, so run_all.R behaves exactly as it always has.
if (!isTRUE(getOption("nba.schema_only", FALSE))) {
  games <- load_and_clean()
  glimpse(games)
}
