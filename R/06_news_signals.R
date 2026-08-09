# ---------------------------------------------------------------------------
# 06_news_signals.R -- injury / lineup / rotation signals (advanced)
# ---------------------------------------------------------------------------
# The base model knows nothing about who is actually playing tonight. That is
# the biggest single gap between it and the market, and it is where a human who
# follows the league has a genuine advantage.
#
# HONESTY WARNING -- read before using any of this
#   This signal is almost impossible to backtest. Historical injury feeds record
#   who ended up playing, not what was KNOWN at the time you would have bet. Any
#   backtest that "adds injuries" is quietly using tomorrow's newspaper.
#   Therefore: news adjustments are FORWARD-TEST ONLY. They are logged in the
#   track record alongside the raw model number, so their contribution can be
#   measured separately after the fact.
#
# Two sources, in order of trust:
#   1. Your own judgement, entered in data/manual_news.csv before tip-off.
#      Start here. It is what README calls "your edge".
#   2. hoopR (an open-source wrapper around public endpoints -- NOT a scraper)
#      for rosters and, where available, injury status.
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")

NEWS_COLS <- c("date", "team", "player", "status", "impact_margin", "impact_total",
               "note", "source", "timestamp")

# ===========================================================================
# 1. Manual signals -- the honest starting point
# ===========================================================================
# impact_margin : points you think this news moves THIS TEAM's margin.
#                 Star out = negative (e.g. -3.5). Star returning = positive.
# impact_total  : points you think it moves the GAME total.
#                 A defensive anchor sitting is positive; a primary scorer
#                 sitting is negative.
# Keep these small. If you find yourself writing -9, you are guessing.

manual_news_template <- function(path = CFG$paths$manual_news, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) {
    info(path, " already exists -- not overwriting")
    return(invisible(path))
  }
  example <- tibble(
    date = Sys.Date(), team = "BOS", player = "Example Player",
    status = "out", impact_margin = -3.0, impact_total = -1.5,
    note = "coach ruled out at shootaround; delete this example row",
    source = "manual", timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  readr::write_csv(example, path)
  info("wrote template ", path)
  invisible(path)
}

read_manual_news <- function(path = CFG$paths$manual_news) {
  if (!file.exists(path)) return(tibble())
  n <- readr::read_csv(path, show_col_types = FALSE)
  missing <- setdiff(NEWS_COLS, names(n))
  if (length(missing))
    stop("manual_news.csv is missing column(s): ", paste(missing, collapse = ", "),
         "\n  Run manual_news_template(overwrite = TRUE) to regenerate the header.",
         call. = FALSE)
  n %>%
    mutate(date = as.Date(.data$date), team = canonical_team(.data$team)) %>%
    filter(!grepl("delete this example row", .data$note %||% "", fixed = TRUE))
}

# ===========================================================================
# 2. Automated availability
# ===========================================================================
# hoopR 3.0.0 has NO injury endpoint -- there is no hoopR::*injuries*() to call.
# What it does expose is the current roster per team, which carries a status
# field (athlete_status_name). That is the closest open-source substitute:
# it tells you who is on the roster and flags anyone not listed Active.
#
# CAVEAT, and it matters: this was verified in the offseason, when every status
# reads "Active" or "Free Agent". Whether ESPN populates this field with
# "Out" / "Day-To-Day" during the season is UNCONFIRMED. Check what
# fetch_rosters() actually returns on a game day before relying on it, and
# treat manual entry as the primary source until you have.

# Who is on each roster, with ESPN's status field. ~30 HTTP requests.
fetch_rosters <- function() {
  if (!requireNamespace("hoopR", quietly = TRUE)) {
    warn('hoopR not installed -- install.packages("hoopR")')
    return(tibble())
  }
  teams <- try(hoopR::espn_nba_teams(), silent = TRUE)
  if (inherits(teams, "try-error")) { warn("could not list teams"); return(tibble()) }
  info("pulling ", nrow(teams), " rosters from ESPN via hoopR (one request each) ...")

  want <- c("athlete_display_name", "athlete_position_abbreviation",
            "athlete_status_name", "athlete_status_abbreviation", "athlete_active")

  out <- map_dfr(seq_len(nrow(teams)), function(i) {
    r <- try(hoopR::espn_nba_team_current_roster(team_id = teams$team_id[i]), silent = TRUE)
    if (inherits(r, "try-error") || !NROW(r)) {
      warn("roster pull failed for ", teams$abbreviation[i]); return(NULL)
    }
    as_tibble(r) %>%
      select(any_of(want)) %>%
      mutate(team = canonical_team(teams$abbreviation[i]), .before = 1)
  })
  if (!nrow(out)) return(tibble())

  names(out) <- sub("^athlete_", "", names(out))
  out %>% rename(player = "display_name") %>%
    mutate(fetched_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
}

# Anyone whose roster status is not plain "Active".
fetch_injuries <- function(rosters = fetch_rosters()) {
  if (!nrow(rosters) || !"status_name" %in% names(rosters)) {
    warn("no roster status available -- use manual entry, see manual_news_template()")
    return(tibble())
  }
  flagged <- rosters %>% filter(!.data$status_name %in% c("Active"))
  info(nrow(flagged), " of ", nrow(rosters), " rostered players are not listed Active")
  if (!nrow(flagged))
    info("If it is a game day and this is still zero, ESPN is not populating ",
         "injury status here. Enter your reads by hand instead.")
  flagged
}

# --- SportsDataIO: buying the structured answer -----------------------------
# The roster-status approach above reconstructs availability from a field ESPN
# may or may not populate. SportsDataIO sells it directly, with the designation
# and a timestamp. Where a vendor sells the structured answer, buying it beats
# extracting it -- which leaves the NLP work in 09 pointed only at the part
# nobody sells, coach-revealed rotation intent.
#
# Returns the same shape as fetch_injuries(), so injuries_to_news() consumes it
# unchanged and you still price the impact yourself.

sdio_key <- function() {
  key <- Sys.getenv(CFG$sportsdataio$key_env, "")
  if (!nzchar(key))
    stop("No ", CFG$sportsdataio$key_env, ". Free trial at https://sportsdata.io ",
         "(1,000 calls/month), then put it in .env:\n",
         "      SPORTSDATAIO_API_KEY=your_key_here", call. = FALSE)
  key
}

fetch_sdio_injuries <- function(path = CFG$sportsdataio$injuries_path) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    warn('install.packages("httr2")'); return(tibble())
  }
  url <- paste0(CFG$sportsdataio$base, "/", path)
  info("GET ", url)

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_headers(`Ocp-Apim-Subscription-Key` = sdio_key()) |>
      httr2::req_timeout(60) |>
      httr2::req_perform(),
    error = function(e) e)

  if (inherits(resp, "error")) {
    warn("SportsDataIO request failed: ", conditionMessage(resp))
    warn("If this is a 404, the endpoint path is wrong -- it is a placeholder. ",
         "Check your account's API explorer and set CFG$sportsdataio$injuries_path.")
    return(tibble())
  }

  raw <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (!length(raw)) { info("no injuries returned"); return(tibble()) }

  # Field names are taken defensively: a vendor schema is not ours to assume.
  pick <- function(x, ...) {
    for (nm in c(...)) if (!is.null(x[[nm]])) return(as.character(x[[nm]]))
    NA_character_
  }
  out <- map_dfr(raw, function(r) tibble(
    team        = canonical_team(pick(r, "Team", "TeamAbbreviation")),
    player      = pick(r, "Name", "PlayerName", "FirstName"),
    status_name = pick(r, "Status", "InjuryStatus"),
    body_part   = pick(r, "BodyPart", "InjuryBodyPart"),
    updated     = pick(r, "Updated", "LastUpdated"),
    source      = "sportsdataio"
  ))
  info(nrow(out), " injury row(s) from SportsDataIO")
  out %>% filter(.data$team %in% TEAM_ALIASES$code)
}

# Automation finds WHO; you decide HOW MUCH. This turns flagged players into
# manual_news.csv rows with the impact columns left blank, deliberately -- a
# points estimate is a judgement call, and the project's whole stance is that
# the human makes it. Fill them in, then read_manual_news() picks them up.
injuries_to_news <- function(inj = fetch_injuries(), date = Sys.Date(),
                             path = CFG$paths$manual_news, append = TRUE) {
  if (!nrow(inj)) { info("nothing to write"); return(invisible(NULL)) }
  rows <- inj %>%
    transmute(date = date, .data$team, .data$player,
              status = tolower(.data$status_name),
              impact_margin = NA_real_, impact_total = NA_real_,
              note = "auto-flagged from ESPN roster status; set the impacts by hand",
              source = "hoopR/espn_roster",
              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))

  if (append && file.exists(path)) {
    old <- readr::read_csv(path, show_col_types = FALSE)
    key <- function(d) paste(d$date, d$team, d$player)
    rows <- rows[!key(rows) %in% key(old), ]
    if (!nrow(rows)) { info("all flagged players already in ", path); return(invisible(NULL)) }
    rows <- bind_rows(old, rows)
  }
  readr::write_csv(rows, path)
  info("wrote ", path, " -- now fill in impact_margin / impact_total")
  invisible(rows)
}

# ===========================================================================
# 3. Reading the news (NLP)
# ===========================================================================
# ESPN publishes a public news API that returns, per article, a `categories`
# block tagging the athletes it is about -- with the SAME athlete_id used in
# the box scores. So entity resolution is exact and free; no fuzzy name
# matching required, and no scraping of article pages.
#
# That leaves the actual language problem: given a sentence mentioning a
# player, what is it claiming about their availability? Injury-report prose is
# formulaic ("ruled out", "listed as questionable", "game-time decision"), so
# pattern rules over the sentence containing the mention do this well and stay
# fully auditable -- every extraction keeps the sentence it came from.
#
# LOOK-AHEAD: articles carry a `published` timestamp. Anything published after
# tip-off is information you could not have had. news_before() enforces that,
# and nothing downstream should use raw news without passing through it.

NEWS_API <- "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/news"

fetch_news <- function(limit = 50) {
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop('install.packages("jsonlite")', call. = FALSE)
  url <- paste0(NEWS_API, "?limit=", limit)
  raw <- try(jsonlite::fromJSON(url, simplifyVector = FALSE), silent = TRUE)
  if (inherits(raw, "try-error")) { warn("news fetch failed"); return(tibble()) }

  out <- map_dfr(raw$articles %||% list(), function(a) {
    ath <- keep(a$categories %||% list(), ~ identical(.x$type, "athlete"))
    tibble(
      article_id  = as.character(a$id %||% NA),
      headline    = a$headline %||% NA_character_,
      description = a$description %||% NA_character_,
      published   = as.POSIXct(a$published %||% NA, tz = "UTC",
                               format = "%Y-%m-%dT%H:%M:%SZ"),
      url         = a$links$web$href %||% NA_character_,
      athlete_ids = list(map_chr(ath, ~ as.character(.x$athlete$id %||% NA))),
      athlete_names = list(map_chr(ath, ~ as.character(.x$description %||% NA)))
    )
  })
  info("fetched ", nrow(out), " articles",
       if (nrow(out)) paste0(", newest ", format(max(out$published, na.rm = TRUE))) else "")
  out
}

# The look-ahead gate. Keep only what was published before `cutoff`.
news_before <- function(news, cutoff = Sys.time()) {
  if (!nrow(news)) return(news)
  keep_rows <- !is.na(news$published) & news$published < as.POSIXct(cutoff)
  dropped <- sum(!keep_rows)
  if (dropped) info("dropped ", dropped, " article(s) published at or after the cutoff")
  news[keep_rows, ]
}

# --- status rules ----------------------------------------------------------
# Ordered: the first pattern that matches the sentence wins. `weight` is how
# much to trust the phrasing, not how likely the player is to sit.
# Order is load-bearing. "will not miss any time" must be read as AVAILABLE
# before the absence patterns get a look at the word "miss", and "returns after
# missing four games" must be read as RETURNING before the speculative
# miss-time pattern claims it. Hence: availability first, speculation last.
STATUS_PATTERNS <- tribble(
  ~status,          ~weight, ~pattern,
  "available",        0.90,  "\\b(will not miss|won'?t miss|avoid(ed|s)? missing|no structural damage|cleared to (play|return)|expected to play|will play|active (tonight|for))\\b",
  "out",              0.95,  "\\b(ruled out|will not play|won'?t play|is out|listed as out|out for the (season|year)|has been ruled out)\\b",
  "out",              0.80,  "\\b(sidelined|shut down|will sit|expected to miss|set to miss|will miss|misses)\\b",
  "doubtful",         0.90,  "\\bdoubtful\\b",
  "questionable",     0.90,  "\\bquestionable\\b",
  "game_time",        0.85,  "game[- ]time decision",
  "probable",         0.85,  "\\bprobable\\b",
  "rest",             0.85,  "\\b(load management|maintenance day|scheduled rest|resting|rested|given a night off)\\b",
  "minutes_limit",    0.80,  "\\bminutes (restriction|limit|cap)\\b",
  "returning",        0.85,  "\\b(return(s|ing|ed)?|cleared|activated|upgraded to available|back in the lineup)\\b",
  # Speculative absence: real signal, but weak enough that the default
  # confidence floor in news_absences() will exclude it.
  "out",              0.50,  "\\bmiss(es|ing)? (time|the game|tonight|[a-z]+ (more )?games)\\b",
  "injury_mention",   0.40,  "\\b(injur(y|ed|ies)|strain(ed)?|sprain(ed)?|soreness|surgery|MRI|contusion)\\b"
)

ABSENCE_STATUSES <- c("out", "doubtful", "questionable", "game_time", "rest")

# Explicit contradictions of an absence claim that the patterns above do not
# already capture as "available".
NEGATION_PATTERN <- "\\bnot (out|sidelined|doubtful|questionable)\\b|\\bdenies\\b"
# Phrases that make any claim speculative.
HEDGE_PATTERN <- "\\b(could|may|might|possibl[ey]|uncertain|unclear|if he|considering|weighing)\\b"

# An absence claim SCOPED to a recurring or partial situation is a workload
# plan, not tonight's absence -- and the ordered rules cannot see the
# difference on their own. "will not play" matches the `out` pattern at 0.95
# before anything looks at what qualifies it, so
#
#     "will not play both ends of back-to-backs"
#
# is read as OUT at 0.95 confidence. Feed that to apply_news() and a healthy
# star is treated as absent: a multi-point error, in the wrong direction, on a
# game you would then bet.
#
# The markers below all indicate a POLICY over several games rather than one
# missed game. Note "back-to-backs" is plural on purpose: "the second night of
# a back-to-back" is a single event and must keep reading as an absence, while
# "misses the second night of back-to-backs" is a standing arrangement.
SCOPE_QUALIFIER_PATTERN <- paste0(
  "\\b(both ends?|either end)\\b",
  "|\\bone game of (every|each)\\b",
  "|\\bback[- ]to[- ]backs\\b",
  "|\\bevery (other )?(back[- ]to[- ]back|game)\\b")

split_sentences <- function(text) {
  text <- text[!is.na(text)]
  if (!length(text)) return(character())
  unlist(strsplit(paste(text, collapse = ". "), "(?<=[.!?])\\s+", perl = TRUE))
}

classify_sentence <- function(sentence) {
  s <- tolower(sentence)
  hit <- NULL
  for (i in seq_len(nrow(STATUS_PATTERNS))) {
    if (grepl(STATUS_PATTERNS$pattern[i], s, perl = TRUE)) { hit <- STATUS_PATTERNS[i, ]; break }
  }
  if (is.null(hit)) return(list(status = NA_character_, confidence = 0,
                                negated = FALSE, hedged = FALSE))

  negated <- grepl(NEGATION_PATTERN, s, perl = TRUE)
  hedged  <- grepl(HEDGE_PATTERN, s, perl = TRUE)
  scoped  <- grepl(SCOPE_QUALIFIER_PATTERN, s, perl = TRUE)
  conf <- hit$weight
  if (hedged) conf <- conf * 0.6
  # A negation next to an absence claim does not flip it to "playing" -- the
  # sentence is simply ambiguous, and saying so is more honest than guessing.
  status <- hit$status
  if (negated && status %in% ABSENCE_STATUSES) { status <- "unclear"; conf <- conf * 0.3 }

  # A scoped absence is a rest plan, not tonight's absence. The claim is still
  # confidently stated -- it just says something different from what the
  # pattern matched -- so the confidence is kept and only the label changes.
  if (scoped && status %in% ABSENCE_STATUSES && !identical(status, "rest"))
    status <- "rest"

  list(status = status, confidence = round(conf, 2),
       negated = negated, hedged = hedged, scoped = scoped)
}

# Optional gazetteer (from 07_usage_model.R::player_gazetteer()) lets us catch
# players ESPN did not tag. Without it we rely on ESPN's own athlete tags,
# which is already exact -- just less complete.
extract_player_status <- function(news, gazetteer = NULL) {
  if (!nrow(news)) return(tibble())

  rows <- map_dfr(seq_len(nrow(news)), function(i) {
    art <- news[i, ]
    sentences <- split_sentences(c(art$headline, art$description))
    if (!length(sentences)) return(NULL)

    tagged <- tibble(athlete_id = art$athlete_ids[[1]],
                     player = art$athlete_names[[1]]) %>%
      filter(!is.na(.data$player))
    if (!is.null(gazetteer) && nrow(gazetteer))
      tagged <- bind_rows(tagged, gazetteer %>% select("athlete_id", "player")) %>%
        distinct(.data$player, .keep_all = TRUE)
    if (!nrow(tagged)) return(NULL)

    map_dfr(sentences, function(sen) {
      # Which candidate players are named in THIS sentence? Restricting to the
      # sentence is what keeps "Booker is out" from being attached to every
      # other player the article happens to mention.
      present <- tagged[vapply(tagged$player,
                               function(p) grepl(p, sen, fixed = TRUE), logical(1)), ]
      if (!nrow(present)) return(NULL)
      cl <- classify_sentence(sen)
      if (is.na(cl$status)) return(NULL)
      present %>% transmute(
        .data$athlete_id, .data$player,
        status = cl$status, confidence = cl$confidence,
        negated = cl$negated, hedged = cl$hedged, scoped = cl$scoped,
        sentence = sen, headline = art$headline,
        published = art$published, url = art$url
      )
    })
  })

  if (!nrow(rows)) { info("no availability statements found in this batch"); return(rows) }

  # One row per player: keep the most confident, most recent statement.
  rows %>%
    arrange(desc(.data$confidence), desc(.data$published)) %>%
    distinct(.data$player, .keep_all = TRUE) %>%
    arrange(desc(.data$confidence))
}

# Who is probably not playing, at a confidence you choose.
news_absences <- function(extracted, min_confidence = 0.7) {
  if (!nrow(extracted)) return(extracted)
  out <- extracted %>%
    filter(.data$status %in% ABSENCE_STATUSES, .data$confidence >= min_confidence)

  # A scoped claim ("sits one game of every back-to-back") says a player will
  # miss SOME game, not that he misses THIS one. Treating it as tonight's
  # absence is precisely the false positive this guard exists to stop, so it is
  # reported separately rather than folded into the absence list.
  if ("scoped" %in% names(out)) {
    n_scoped <- sum(out$scoped, na.rm = TRUE)
    if (n_scoped)
      info(n_scoped, " scoped rest-policy statement(s) held back -- they describe ",
           "a workload plan, not tonight's availability")
    out <- out %>% filter(!.data$scoped)
  }

  info(nrow(out), " player(s) flagged as likely absent at confidence >= ", min_confidence)
  out
}

# ===========================================================================
# 4. Applying news to a prediction
# ===========================================================================
# Adjustments are additive and are kept in their own columns, so the raw model
# number survives untouched in the track record. That separation is what lets
# you later ask "did my news reads add anything, or did they cost me?"

apply_news <- function(up, news = read_manual_news()) {
  if (!nrow(news)) {
    return(up %>% mutate(news_margin_adj = 0, news_total_adj = 0,
                         news_note = NA_character_,
                         pred_margin_adj = .data$pred_margin,
                         pred_total_adj  = .data$pred_total))
  }

  # A hand-entered impact past this size is a guess with decimals, not a read.
  # Clamping loudly beats silently trusting a -9.
  cap <- CFG$news$max_abs_impact
  over <- which(abs(coalesce(news$impact_margin, 0)) > cap |
                abs(coalesce(news$impact_total, 0)) > cap)
  if (length(over)) {
    warn(length(over), " news impact(s) exceed +/-", cap,
         " and were clamped: ", paste(unique(news$player[over]), collapse = ", "))
    news$impact_margin <- pmax(pmin(news$impact_margin, cap), -cap)
    news$impact_total  <- pmax(pmin(news$impact_total,  cap), -cap)
  }

  agg <- news %>%
    group_by(.data$date, .data$team) %>%
    summarise(margin_adj = sum(.data$impact_margin, na.rm = TRUE),
              total_adj  = sum(.data$impact_total,  na.rm = TRUE),
              note = paste(.data$player, .data$status, collapse = "; "),
              .groups = "drop")

  up %>%
    left_join(agg %>% rename(home_team = "team", h_margin = "margin_adj",
                             h_total = "total_adj", h_note = "note"),
              by = c("date", "home_team")) %>%
    left_join(agg %>% rename(away_team = "team", a_margin = "margin_adj",
                             a_total = "total_adj", a_note = "note"),
              by = c("date", "away_team")) %>%
    mutate(
      # A home-team hit lowers the home margin; an away-team hit raises it.
      news_margin_adj = coalesce(.data$h_margin, 0) - coalesce(.data$a_margin, 0),
      news_total_adj  = coalesce(.data$h_total, 0) + coalesce(.data$a_total, 0),
      # Label each side's note with the team it belongs to, and emit a note only
      # for the sides that actually have one -- the old unconditional paste left
      # a dangling "BOS out |" on every one-sided game, in a permanent log.
      news_note = {
        h <- if_else(is.na(.data$h_note), NA_character_,
                     paste0(.data$home_team, ": ", .data$h_note))
        a <- if_else(is.na(.data$a_note), NA_character_,
                     paste0(.data$away_team, ": ", .data$a_note))
        both <- if_else(!is.na(h) & !is.na(a), paste(h, a, sep = " | "), coalesce(h, a))
        both
      },
      pred_margin_adj = .data$pred_margin + .data$news_margin_adj,
      pred_total_adj  = .data$pred_total  + .data$news_total_adj
    ) %>%
    select(-any_of(c("h_margin", "a_margin", "h_total", "a_total", "h_note", "a_note")))
}

# ===========================================================================
# 5. Did the news reads actually help?
# ===========================================================================
# Run this once you have a decent number of settled forward-test bets that
# carried a news adjustment.

evaluate_news_contribution <- function(track = NULL) {
  track <- track %||% read_track_record()
  if (!nrow(track)) { warn("no track record yet"); return(invisible(NULL)) }

  settled <- track %>% filter(!is.na(.data$win_loss), !is.na(.data$units))
  if (!nrow(settled)) { info("nothing settled yet"); return(invisible(NULL)) }

  # Keyed on news_adj, not on the notes text. The notes column also carries
  # moneyline EV and usage-bump provenance, so "has a note" is not the same
  # question as "was this number moved by a hand-entered read".
  settled <- settled %>%
    mutate(had_news = !is.na(.data$news_adj) & .data$news_adj != 0)

  step("News-adjusted vs plain bets")
  out <- settled %>%
    group_by(.data$had_news) %>%
    summarise(n = n(),
              win_rate = mean(.data$win_loss == "win", na.rm = TRUE),
              units = sum(.data$units, na.rm = TRUE),
              roi = sum(.data$units, na.rm = TRUE) / n(), .groups = "drop")
  print(out)

  # The sharper question, and the one this column pair exists to answer: on the
  # bets that WERE adjusted, did the adjustment point the right way? Comparing
  # adjusted against un-adjusted bets confounds the read with whatever made
  # those games newsworthy; comparing the raw and adjusted numbers on the SAME
  # bet does not.
  adj <- settled %>% filter(.data$had_news, !is.na(.data$prediction_raw),
                            !is.na(.data$result))
  if (nrow(adj)) {
    err_raw <- abs(adj$result - adj$prediction_raw)
    err_adj <- abs(adj$result - adj$prediction)
    better <- mean(err_adj < err_raw)
    message(sprintf(
      "   on the %d adjusted bets: mean |error| %.2f raw -> %.2f adjusted; the read helped on %.0f%%",
      nrow(adj), mean(err_raw), mean(err_adj), 100 * better))
    if (mean(err_adj) > mean(err_raw))
      warn("the adjustments are making predictions WORSE on average. ",
           "That is the finding -- do not quietly keep applying them.")
  }

  if (nrow(out) < 2 || min(out$n) < 50)
    info("both groups need 50+ settled bets before this comparison says anything")
  invisible(out)
}

# ---------------------------------------------------------------------------
step("06_news_signals.R loaded")
info("manual_news_template()          create data/manual_news.csv")
info("read_manual_news()              load your pre-game reads")
info("fetch_rosters()                 all 30 ESPN rosters + status field")
info("fetch_injuries()                players not listed Active")
info("injuries_to_news()              draft news rows for you to price up")
info("apply_news(up)                  add adjustments to upcoming predictions")
info("evaluate_news_contribution()    did the reads pay? (forward test only)")
warn("Never apply news adjustments inside the backtest -- see the header of this file.")
