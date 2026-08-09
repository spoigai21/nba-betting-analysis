# ---------------------------------------------------------------------------
# 09_llm_news.R -- coach-revealed strategy, the part regex cannot read
# ---------------------------------------------------------------------------
# WHAT THIS IS FOR, AND WHAT IT IS NOT FOR
#
# It is NOT a replacement for 06_news_signals.R. That file answers "who is
# out", and it already does that well: ESPN tags every article with the same
# athlete ids the box scores use, so entity resolution is exact and free, and
# injury-report prose ("ruled out", "listed as questionable", "game-time
# decision") is formulaic enough that ordered pattern rules handle it while
# staying fully auditable.
#
# This file is pointed at the residue those rules cannot touch: a coach saying
# he intends to stagger a starter's minutes on the second night of a back to
# back, or that a rotation is changing, or that someone is on a minutes cap
# without being listed on the injury report. That is the signal README calls
# the edge, it is genuinely unstructured, and nobody sells it as a field.
#
# THREE THINGS MAKE AN LLM AWKWARD HERE, AND EACH IS HANDLED
#
# 1. NON-DETERMINISM vs THE AUDIT TRAIL. This project rests on seeded runs and
#    an append-only log. A model call is not reproducible. So every extraction
#    is cached by (article id, prompt version, model id) into a COMMITTED
#    file, and an article already in the cache is never queried again. The
#    pipeline is reproducible; the cache is itself the audit artifact.
#
# 2. LOOK-AHEAD THROUGH THE TRAINING CUTOFF. A model may know how a game
#    actually turned out and answer from memory rather than from the text.
#    Regex cannot do that. Two defences: the prompt forbids outside knowledge
#    and requires a quoted sentence for every signal, and callers must pass
#    articles through news_before() first. Running this over historical
#    articles to build a backtest is NOT supported -- see the honesty warning
#    at the top of 06.
#
# 3. IT MIGHT NOT HELP. So it does not replace the rules, it runs beside them,
#    and compare_extractors() reports where they disagree. That comparison is
#    the evidence for whether this file earns its place.
#
# Needs a key in .env:   ANTHROPIC_API_KEY=sk-ant-...
# ---------------------------------------------------------------------------

if (!exists("CFG")) source("R/00_setup.R")
if (!exists("news_before")) source("R/06_news_signals.R")

# Bump on ANY change to the prompt or the schema. It is part of the cache key,
# so a bumped version re-queries rather than silently mixing outputs from two
# different instructions in one file.
LLM_PROMPT_VERSION <- "strategy-v1"

SIGNAL_TYPES <- c("minutes_restriction", "rotation_change", "load_management",
                  "role_change", "return_timeline", "none")

# ===========================================================================
# 1. The contract
# ===========================================================================
# Structured outputs, so the response is schema-validated server-side rather
# than parsed hopefully. Note the JSON-schema limits that apply: no recursion,
# no numeric bounds, and additionalProperties must be false on every object.

llm_news_schema <- function() {
  list(
    type = "object",
    properties = list(
      signals = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            player      = list(type = "string",
                               description = "Player's full name exactly as written in the article."),
            signal_type = list(type = "string", enum = as.list(SIGNAL_TYPES)),
            direction   = list(type = "string", enum = list("increase", "decrease", "unclear"),
                               description = "Whether this points to MORE or LESS playing time / production."),
            confidence  = list(type = "number",
                               description = "0 to 1. How clearly the article states this, not how likely it is to be true."),
            evidence    = list(type = "string",
                               description = "The exact sentence from the article. Verbatim, no paraphrase."),
            attributed_to = list(type = "string",
                               description = "Who said it -- coach, team, reporter, or 'unattributed'.")
          ),
          required = list("player", "signal_type", "direction", "confidence",
                          "evidence", "attributed_to"),
          additionalProperties = FALSE
        )
      )
    ),
    required = list("signals"),
    additionalProperties = FALSE
  )
}

llm_system_prompt <- function() {
  paste0(
    "You extract coach-revealed rotation and workload intentions from NBA news text.\n\n",
    "You are NOT extracting injury status. Another system already handles who is ",
    "ruled out, questionable, doubtful, or active, and it does so from structured ",
    "tags. Do not duplicate it. Ignore plain injury-report language unless it also ",
    "reveals an intention about playing time.\n\n",
    "Extract only these, and only when the article states them:\n",
    "  minutes_restriction  an explicit cap or limit on a player's minutes\n",
    "  rotation_change      a change to who plays, who starts, or how units are used\n",
    "  load_management      planned rest, staggering, or sitting a healthy player\n",
    "  role_change          a change in role, usage, or responsibility\n",
    "  return_timeline      a stated timeline for a player's return to normal load\n\n",
    "RULES, all of them binding:\n",
    "1. Use ONLY the article text provided. You may know things about these players ",
    "or how their games turned out. That knowledge is off limits -- it would be ",
    "information the bettor could not have had, and using it silently corrupts the ",
    "result. If the text does not say it, it is not a signal.\n",
    "2. Every signal MUST quote the exact sentence it came from, verbatim, in ",
    "`evidence`. If you cannot quote it, do not report it.\n",
    "3. `confidence` measures how plainly the article states the thing, NOT how ",
    "likely you think it is to happen. A coach saying it directly is high. A ",
    "reporter speculating is low.\n",
    "4. Speculation, rumour, and hypotheticals are low confidence, not omissions -- ",
    "report them with the hedge reflected in the score.\n",
    "5. Return an empty `signals` array when the article contains none. Most ",
    "articles contain none. That is the expected outcome and is not a failure.\n",
    "6. Do not infer a signal from a player merely being mentioned, being injured, ",
    "or playing well."
  )
}

llm_user_content <- function(article) {
  paste0(
    "Published: ", format(article$published), "\n",
    "Headline: ", article$headline %||% "", "\n\n",
    article$description %||% "", "\n"
  )
}

# ===========================================================================
# 2. The call
# ===========================================================================
# Swappable via options(nba.llm_call = ...) so the surrounding machinery --
# caching, parsing, comparison -- is testable without a network or a key.

llm_api_key <- function() {
  key <- Sys.getenv(CFG$llm$api_key_env, "")
  if (!nzchar(key))
    stop("No ", CFG$llm$api_key_env, ". Put it in .env at the project root:\n",
         "      ANTHROPIC_API_KEY=sk-ant-...\n",
         "  (.env is gitignored.)", call. = FALSE)
  key
}

default_llm_call <- function(body) {
  if (!requireNamespace("httr2", quietly = TRUE))
    stop('install.packages("httr2")', call. = FALSE)
  resp <- httr2::request(CFG$llm$base) |>
    httr2::req_headers(`x-api-key` = llm_api_key(),
                       `anthropic-version` = CFG$llm$version,
                       `content-type` = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(120) |>
    httr2::req_perform()
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

llm_call <- function(body) (getOption("nba.llm_call", default_llm_call))(body)

llm_build_body <- function(article) {
  list(
    model = CFG$llm$model,
    max_tokens = CFG$llm$max_tokens,
    # The system prompt and schema are identical on every article, so they are
    # the stable cache prefix; the article itself is the only thing that varies.
    system = list(list(type = "text", text = llm_system_prompt(),
                       cache_control = list(type = "ephemeral"))),
    output_config = list(effort = CFG$llm$effort,
                         format = list(type = "json_schema",
                                       schema = llm_news_schema())),
    messages = list(list(role = "user", content = llm_user_content(article)))
  )
}

# Pull the signals out of a response, refusing to guess when the model did not
# actually finish.
llm_parse_response <- function(resp) {
  stop_reason <- resp$stop_reason %||% NA_character_
  if (identical(stop_reason, "refusal"))
    return(list(ok = FALSE, reason = "refusal", signals = list()))
  if (identical(stop_reason, "max_tokens"))
    return(list(ok = FALSE, reason = "max_tokens", signals = list()))

  txt <- NULL
  for (b in resp$content %||% list())
    if (identical(b$type, "text")) { txt <- b$text; break }
  if (is.null(txt) || !nzchar(txt))
    return(list(ok = FALSE, reason = "empty", signals = list()))

  parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$signals))
    return(list(ok = FALSE, reason = "unparseable", signals = list()))

  list(ok = TRUE, reason = "ok", signals = parsed$signals,
       model = resp$model %||% CFG$llm$model)
}

# ===========================================================================
# 3. The cache -- what makes this reproducible
# ===========================================================================
# Newline-delimited JSON rather than a binary blob, so the file diffs, and it
# lives outside data/processed/ so it is committed rather than ignored. A
# logged extraction can therefore be shown to have preceded the game, exactly
# like a logged prediction.

llm_cache_key <- function(article_id, prompt_version = LLM_PROMPT_VERSION,
                          model = CFG$llm$model) {
  paste(article_id, prompt_version, model, sep = "|")
}

llm_cache_read <- function(path = CFG$llm$cache) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  out <- list()
  for (ln in lines) {
    rec <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (!is.null(rec) && !is.null(rec$key)) out[[rec$key]] <- rec
  }
  out
}

llm_cache_append <- function(rec, path = CFG$llm$cache) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  cat(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null"), "\n",
      file = path, append = TRUE, sep = "")
  invisible(rec)
}

# ===========================================================================
# 4. Extraction
# ===========================================================================

# Turn one cached record into rows.
llm_record_rows <- function(rec) {
  sig <- rec$signals %||% list()
  if (!length(sig)) return(tibble())
  map_dfr(sig, function(s) tibble(
    article_id  = rec$article_id  %||% NA_character_,
    player      = s$player        %||% NA_character_,
    signal_type = s$signal_type   %||% NA_character_,
    direction   = s$direction     %||% NA_character_,
    confidence  = as.numeric(s$confidence %||% NA),
    evidence    = s$evidence      %||% NA_character_,
    attributed_to = s$attributed_to %||% NA_character_,
    headline    = rec$headline    %||% NA_character_,
    published   = rec$published   %||% NA_character_,
    model       = rec$model       %||% NA_character_,
    prompt_version = rec$prompt_version %||% NA_character_,
    extracted_at   = rec$extracted_at   %||% NA_character_
  ))
}

# The main entry point.
#
# `news` must already have passed through news_before() -- this function will
# not let you extract from an article published after the cutoff you intend to
# act at, because that is the one mistake the cache cannot undo.
extract_strategy_signals <- function(news, cutoff = Sys.time(),
                                     use_cache = TRUE, dry_run = FALSE) {
  if (!NROW(news)) { info("no articles supplied"); return(tibble()) }

  before <- nrow(news)
  news <- news_before(news, cutoff)
  if (!nrow(news)) { warn("every article was published at or after the cutoff"); return(tibble()) }
  if (nrow(news) < before)
    info("look-ahead gate dropped ", before - nrow(news), " article(s)")

  cache <- if (use_cache) llm_cache_read() else list()
  hits <- 0L; calls <- 0L; failures <- 0L
  recs <- list()

  for (i in seq_len(nrow(news))) {
    art <- news[i, ]
    key <- llm_cache_key(art$article_id)

    if (!is.null(cache[[key]])) {
      recs[[length(recs) + 1L]] <- cache[[key]]; hits <- hits + 1L; next
    }
    if (dry_run) next

    resp <- tryCatch(llm_call(llm_build_body(art)), error = function(e) {
      warn("call failed for article ", art$article_id, ": ", conditionMessage(e)); NULL
    })
    calls <- calls + 1L
    if (is.null(resp)) { failures <- failures + 1L; next }

    parsed <- llm_parse_response(resp)
    if (!parsed$ok) {
      warn("article ", art$article_id, ": ", parsed$reason)
      failures <- failures + 1L
      next
    }

    rec <- list(
      key = key, article_id = art$article_id,
      headline = art$headline, published = format(art$published),
      url = art$url, model = parsed$model,
      prompt_version = LLM_PROMPT_VERSION,
      extracted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      signals = parsed$signals
    )
    llm_cache_append(rec)
    recs[[length(recs) + 1L]] <- rec
  }

  info(hits, " from cache, ", calls, " new call(s), ", failures, " failure(s)")
  if (dry_run && !length(recs)) {
    info("dry run -- ", nrow(news) - hits, " article(s) would be sent")
    return(tibble())
  }

  out <- map_dfr(recs, llm_record_rows)
  if (!nrow(out)) { info("no strategy signals found in this batch"); return(out) }

  out <- out %>% filter(.data$signal_type != "none") %>% arrange(desc(.data$confidence))
  info(nrow(out), " signal(s); ",
       sum(out$confidence >= CFG$llm$min_confidence), " at confidence >= ",
       CFG$llm$min_confidence)
  out
}

# Actionable subset, and a reminder that this is a forward-test-only signal.
strategy_signals_actionable <- function(sig, min_confidence = CFG$llm$min_confidence) {
  if (!NROW(sig)) return(sig)
  sig %>% filter(.data$confidence >= min_confidence,
                 .data$direction %in% c("increase", "decrease"))
}

# ===========================================================================
# 5. Does it earn its place?
# ===========================================================================
# The rules and the model are asked different questions, so full agreement is
# not the goal. What matters is whether the model finds anything real that the
# rules miss. Run this on a batch you have read yourself -- it reports where
# they diverge, and the sentences are there so you can judge who is right.

compare_extractors <- function(news, cutoff = Sys.time(), gazetteer = NULL) {
  step("Rules vs model on the same articles")
  if (!NROW(news)) { warn("no articles"); return(invisible(NULL)) }

  rules <- extract_player_status(news_before(news, cutoff), gazetteer = gazetteer)
  llm   <- extract_strategy_signals(news, cutoff = cutoff)

  info("rules: ", nrow(rules), " availability statement(s)")
  info("model: ", nrow(llm), " strategy signal(s)")

  rp <- if (nrow(rules)) unique(rules$player) else character()
  lp <- if (nrow(llm))   unique(llm$player)   else character()

  only_llm   <- setdiff(lp, rp)
  only_rules <- setdiff(rp, lp)
  both       <- intersect(lp, rp)

  message("")
  info("both  : ", length(both), " player(s)")
  info("model only: ", length(only_llm), " -- candidates for what the rules cannot see")
  info("rules only: ", length(only_rules), " -- availability without a stated intention")

  if (length(only_llm)) {
    message("\n   Model-only signals (read the evidence and judge for yourself):")
    ex <- llm %>% filter(.data$player %in% only_llm) %>% head(8)
    for (i in seq_len(nrow(ex)))
      message(sprintf("     %-22s %-20s %-8s %.2f  \"%s\"",
                      substr(ex$player[i], 1, 22), ex$signal_type[i],
                      ex$direction[i], ex$confidence[i],
                      substr(ex$evidence[i], 1, 90)))
  }
  message("\n   A model-only signal is a lead, not a finding. Until these are")
  message("   graded on the forward record they are unverified, and the whole")
  message("   point of keeping the rules alongside is that this stays checkable.")

  invisible(list(rules = rules, llm = llm,
                 only_llm = only_llm, only_rules = only_rules, both = both))
}

# ---------------------------------------------------------------------------
step("09_llm_news.R loaded")
info("news <- news_before(fetch_news(50))    look-ahead gate FIRST")
info("extract_strategy_signals(news)         cached, schema-validated")
info("strategy_signals_actionable(sig)       above the confidence floor")
info("compare_extractors(news)               rules vs model, side by side")
warn("Forward-test only. Do not run this over historical articles to build a backtest.")
