# ────────────────────────────────
# LOAD REQUIRED LIBRARIES
# ────────────────────────────────
library(purrr)
library(jsonlite)
library(stringr)
library(httr)

# ==========================================================
# 1️⃣ Fetch List of Models
# ==========================================================
# all models, sorted cheapest to most expensive
get_openrouter_all_models <- function(api_key) {
  url <- "https://openrouter.ai/api/v1/models"
  res <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", api_key)))
  if (res$status_code != 200)
    stop("Failed to fetch models: ", res$status_code)
  
  json <- httr::content(res, as = "parsed", simplifyVector = FALSE)
  model_list <- json$data
  
  model_ids <- lapply(model_list, function(m) {
    if (is.list(m) && !is.null(m$id)) {
      return(list(id = m$id, price = if (!is.null(m$pricing$prompt)) m$pricing$prompt else Inf))
    } else {
      return(NULL)
    }
  })
  
  # Remove NULLs
  model_ids <- Filter(Negate(is.null), model_ids)
  
  # Sort by price (free first), then by ID
  model_ids <- model_ids[order(sapply(model_ids, function(x) x$price), 
                               sapply(model_ids, function(x) x$id))]
  
  # Return only IDs
  unlist(lapply(model_ids, function(x) x$id))
}

#free models only
get_openrouter_free_models <- function(api_key) {
  url <- "https://openrouter.ai/api/v1/models"
  res <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", api_key)))
  if (res$status_code != 200)
    stop("Failed to fetch models: ", res$status_code)
  
  json <- httr::content(res, as = "parsed", simplifyVector = FALSE)
  model_list <- json$data
  
  free_ids <- lapply(model_list, function(m) {
    if (is.list(m) && !is.null(m$id) && !is.null(m$pricing) &&
        is.list(m$pricing) &&
        !is.null(m$pricing$prompt) && m$pricing$prompt == 0) {
      return(m$id)
    } else
      return(NULL)
  })
  
  unlist(free_ids)
}

# ==========================================================
# 2️⃣ Formatting Helper
# ==========================================================
clean_ai_text <- function(txt) {
  if (is.null(txt) || !nzchar(txt))
    return("")
  
  # ============================================================
  # INTERNAL FALLBACK CLEANER (used only if tag-extraction fails)
  # ============================================================
  fallback_clean <- function(x) {
    if (is.null(x) || !nzchar(x))
      return("")
    
    x <- gsub("\r\n", "\n", x)
    x <- gsub("\r", "\n", x)
    
    # Remove instruction tags
    instr_pat <- "
      <<+\\s*(INSTR|INSTRUCTION|INSTRUCTIONS)\\s*(START|END)?\\s*>+|
      <+\\s*/?\\s*(INSTR|INSTRUCTION|INSTRUCTIONS)\\s*>+
    "
    x <- gsub(instr_pat, "", x, ignore.case = TRUE, perl = TRUE)
    
    # Remove answer-related tags outside final extraction
    ans_pat <- "
      <<+\\s*(ANS|ANSWER|RESP|RESPONSE)\\s*(START|END)?\\s*>+|
      <+\\s*/?\\s*(ANS|ANSWER|RESP|RESPONSE)\\s*>+
    "
    x <- gsub(ans_pat, "", x, ignore.case = TRUE, perl = TRUE)
    
    # Remove hallucinated DATA blocks
    x <- gsub("<<<\\s*DATA\\s*(START|END)\\s*>>>", "", x, ignore.case = TRUE)
    
    # Remove XML/HTML wrappers
    x <- gsub("</?(assistant|response|answer)>", "", x, ignore.case = TRUE)
    
    # Remove JSON wrappers
    x <- gsub("^\\s*\\{\\s*\"?(text|answer|response)\"?\\s*:\\s*",
              "",
              x)
    x <- gsub("\\}\\s*$", "", x)
    
    # Remove markdown fences
    x <- gsub("^(```[a-zA-Z0-9_]*|---\\s*\\n)|```\\s*$", "", x)
    
    # Remove GPT meta tags
    split_pattern <- "(?i)<\\|end\\|>|<\\|start\\|>|assistantfinal"
    if (grepl(split_pattern, x, perl = TRUE)) {
      segments <- unlist(strsplit(x, split_pattern, perl = TRUE))
      segments <- segments[grepl("\\S", segments)]
      x <- if (length(segments))
        tail(segments, 1)
      else
        ""
    }
    
    # Chain-of-thought / system chatter
    self_talk_pattern <- paste0(
      "^(system:|assistant:|user:|We need|We should|Let's craft|Let's produce|",
      "Ok\\.|Also note|We have data|We should note|as an ai|i cannot|i can’t|i'm sorry|here is your answer|here’s your answer|",
      "based on your request|following your instructions|according to your data|let's craft|let us craft)"
    )
    lines <- unlist(strsplit(x, "\n"))
    lines <- lines[!grepl(self_talk_pattern, lines, ignore.case = TRUE)]
    
    # Strip AI/assistant metadata prefixes on each line
    clean_lines <- function(lines) {
      gsub("^(.*?<\\|.*?\\|>)+", "", lines, perl = TRUE)
    }
    lines <- clean_lines(lines)
    
    # Keep meaningful lines
    lines <- lines[grepl("\\S", lines)]
    
    #discard everything before first found heading
    first_heading <- grep("^\\s*#+\\s+", lines)
    if (length(first_heading) > 0) {
      lines <- lines[first_heading[1]:length(lines)]
    }
    x <- paste(lines, collapse = "\n")
    x <- str_trim(x)
    return(x)
  }
  
  # ============================================================
  # 1. NORMALIZE TAGS (remove markdown wrapping, fix variants)
  # ============================================================
  # Remove markdown wrappers around any tag-like object
  txt <- gsub("([*_~`]+)(<<<[^>]+>>>)([*_~`]+)", "\\2", txt, perl = TRUE)
  
  # Fuzzy tag matching
  start_regex <- "(?i)<+\\s*(ans|answer)?\\s*start\\s*>+"
  end_regex   <- "(?i)<+\\s*(ans|answer)?\\s*end\\s*>+"
  
  # Normalize variants to canonical form
  txt <- gsub(start_regex, "<<<ANSWER START>>>", txt, perl = TRUE)
  txt <- gsub(end_regex, "<<<ANSWER END>>>", txt, perl = TRUE)
  
  # ============================================================
  # 2. PRIMARY EXTRACTION BETWEEN TAGS (ENHANCED MULTI-BLOCK SEARCH)
  # ============================================================
  
  # Find ALL start and end tags
  starts <- gregexpr("<<<ANSWER START>>>", txt, fixed = TRUE)[[1]]
  ends   <- gregexpr("<<<ANSWER END>>>", txt, fixed = TRUE)[[1]]
  
  # Flag: do we have ANY start tag at all?
  in_primary <- (length(starts) > 0 && starts[1] != -1)
  
  if (in_primary) {
    blocks <- list()
    
    # Pair each START with the nearest END that appears after it
    for (i in seq_along(starts)) {
      s <- starts[i]
      if (s == -1)
        next
      
      # match.length MUST be indexed properly
      s_len <- attr(starts, "match.length")[i]
      
      e <- ends[ends > s]
      
      if (length(e) == 0) {
        # No closing END tag: take everything from START to end of text
        block <- substr(txt, s + s_len, nchar(txt))
      } else {
        # Normal case: pair with nearest END
        e <- e[1]  # earliest valid END
        block <- substr(txt, s + s_len, e - 1)
      }
      
      block_clean <- str_trim(block)
      
      # Reject very short blocks: treat as empty if less than 2 lines
      if (length(grep("\\S", unlist(strsplit(
        block_clean, "\n"
      )))) < 2)
        block_clean <- ""
      
      if (nzchar(block_clean))
        blocks[[length(blocks) + 1]] <- block_clean
      
    }
    
    if (length(blocks) > 0) {
      # Choose most meaningful block (longest)
      txt <- blocks[[which.max(nchar(blocks))]]
      txt <- str_trim(txt)
      
    } else {
      # We HAD tags, but all empty/useless → fallback cleaning
      in_primary <- FALSE
      txt <- fallback_clean(txt)
    }
  }
  
  
  # If START tag missing: fallback clean the whole text
  if (!in_primary) {
    txt <- fallback_clean(txt)
  }
  
  # ============================================================
  # 3. LINE CLEANING
  # ============================================================
  lines <- unlist(strsplit(txt, "\n"))
  lines <- str_replace_all(lines, "\\s+$", "")  # remove trailing spaces
  
  # Final output after line cleaning
  txt <- paste(lines, collapse = "\n")
  str_trim(txt)
}

# ==========================================================
# 3️⃣ Safe Nested Access & Collapse Helpers
# ==========================================================
safe <- function(final, path) {
  keys <- strsplit(path, "\\$")[[1]]
  tryCatch(
    pluck(final, !!!keys, .default = NULL),
    error = function(e)
      NULL
  )
}

mk <- function(x) {
  if (is.null(x))
    return(NULL)
  if (is.list(x)) {
    collapsed <- lapply(x, function(row) {
      if (is.list(row)) {
        vals <- vapply(row, function(v) {
          if (is.null(v) || length(v) == 0)
            return("")
          if (is.numeric(v) &&
              abs(v) < 1 &&
              grepl("perc|percent", names(row), ignore.case = TRUE))
            return(paste0(round(v * 100, 1), "%"))
          as.character(v)
        }, "")
        paste(paste(names(vals), vals, sep = "="), collapse = " | ")
      } else {
        paste(row, collapse = " | ")
      }
    })
    return(paste(collapsed, collapse = " || "))
  }
  
  if (is.numeric(x) &&
      abs(x) < 1 &&
      grepl("perc|percent|%", names(row), ignore.case = TRUE))
    return(paste0(round(x * 100, 1), "%"))
  
  as.character(x)
}

collapse_row <- function(row) {
  vals <- lapply(row, function(x)
    if (is.null(x) || is.na(x))
      ""
    else
      as.character(x))
  paste(names(vals), vals, sep = "=", collapse = " | ")
}

# =========================================================
# 4️⃣  Context Generator
# =========================================================
generate_ai_context <- function(final,
                                max_weeks = 12,
                                include_weekly_table = TRUE,
                                include_period_summary = TRUE,
                                max_periods = 3) {
  # ----------------------------------------------------------
  # CURRENT WEEK BLOCK
  # ----------------------------------------------------------
  cw <- list(
    Header     = mk(safe(
      final, "Stats$Current_Week$Header_Info"
    )),
    Body       = mk(safe(final, "Stats$Current_Week$Body")),
    Nutrition  = mk(safe(final, "Stats$Current_Week$Nutrition")),
    Training   = mk(safe(final, "Stats$Current_Week$Training")),
    Lifestyle  = mk(safe(final, "Stats$Current_Week$Lifestyle"))
  )
  cw_block <- paste(unlist(map(names(cw), ~ if (!is.null(cw[[.x]]))
    paste0(.x, ": ", cw[[.x]]))), collapse = "\n")
  if (cw_block == "")
    cw_block <- "No current-week stats."
  
  # ----------------------------------------------------------
  # PROFILE BLOCK
  # ----------------------------------------------------------
  pf <- list(
    Header    = mk(safe(final, "Stats$Profile$Header_Info")),
    Body      = mk(safe(final, "Stats$Profile$Body")),
    Nutrition = mk(safe(final, "Stats$Profile$Nutrition")),
    Training  = mk(safe(final, "Stats$Profile$Training")),
    Lifestyle = mk(safe(final, "Stats$Profile$Lifestyle"))
  )
  pf_block <- paste(unlist(map(names(pf), ~ if (!is.null(pf[[.x]]))
    paste0(.x, ": ", pf[[.x]]))), collapse = "\n")
  if (pf_block == "")
    pf_block <- "No profile-level stats."
  
  # ----------------------------------------------------------
  # INSIGHTS
  # ----------------------------------------------------------
  cw_ins <- safe(final, "Insights$Current_Week")
  pf_ins <- safe(final, "Insights$Profile")
  
  ins <- c()
  if (length(cw_ins))
    ins <- c(ins, paste0(names(cw_ins), ": ", unlist(cw_ins)))
  if (length(pf_ins))
    ins <- c(ins, paste0("Profile_", names(pf_ins), ": ", unlist(pf_ins)))
  
  insights_block <- if (length(ins))
    paste(ins, collapse = "\n")
  else
    "No deterministic insights."
  
  # ----------------------------------------------------------
  # WEEKLY HISTORY
  # ----------------------------------------------------------
  weekly_block <- "Not included."
  df <- safe(final, "Raw_Data$Weekly_Data_Active_Profile")
  if (include_weekly_table && is.data.frame(df)) {
    df <- tail(df, max_weeks)
    weekly_block <- paste(apply(df, 1, collapse_row), collapse = "\n")
  }
  
  # ----------------------------------------------------------
  # PERIOD SUMMARY
  # ----------------------------------------------------------
  period_block <- "Not included."
  per <- safe(final, "Raw_Data$Per_Period_Summary")
  if (include_period_summary && is.data.frame(per)) {
    per <- tail(per, max_periods)
    period_block <- paste(apply(per, 1, collapse_row), collapse = "\n")
  }
  
  # ----------------------------------------------------------
  # ASSEMBLED CONTEXT
  # ----------------------------------------------------------
  paste(
    "\n---\n",
    "<<<SECTION: DATA START>>>",
    "\nSECTION: CURRENT_WEEK_STATS\n",
    cw_block,
    "\nSECTION: FULL_PROFILE_STATS\n",
    pf_block,
    "\nSECTION: INSIGHTS\n",
    insights_block,
    "\nSECTION: WEEKLY_LOG_RAW_DATA\n",
    weekly_block,
    "\nSECTION: PER_PERIOD_SUMMARY\n",
    period_block,
    "<<<SECTION:DATA END>>>",
    "\n---\n",
    sep = "\n"
  )
}

# ==========================================================
# 5️⃣ PROMPT GENERATOR
# ==========================================================
call_openrouter_ai <- function(prompt, api_key, models_to_try, verbose = FALSE) {
  models_to_try <- as.vector(models_to_try)
  
  # -------------------------
  # Strict wrapper
  # -------------------------
  strict_wrapper <- "<<<INSTRUCTIONS START>>>
TONE & ROLE
- You are an evidence-oriented **elite physique and performance coach**, with decades of experience across diverse clients.
- Speak like a seasoned coach and personal trainer: calm, direct, practical, and human.
- Write naturally and with a conversational tone — not like a template or a report.

COACHING BEHAVIOR
- Interpret the data realistically, highlight what matters most, and propose clear next steps.
- Never invent numbers not directly available or logically inferred.
- Mention uncertainty when appropriate, but keep it brief.
- Treat app insights as one input among many.
- If an app insight seems incomplete or questionable, say: “Note: [insight] may be incomplete because [reason].”
- Reference supporting evidence by type only (e.g., 'based on your weight and waist trend', 'based on the last 3-week trend').

FORMATTING RULES (MANDATORY)
- Aim for a clean, clear and readable flow.
- Use short paragraphs when possible; add explanations and further context only when absolutely necessary and genuinely improves clarity.
- bullets are optional and should be used when they genuinely improve clarity (e.g. listing actions or findings)
- Use headings when they make the response easier to digest
  - Use ## for main headings.
  - Use ### for sub-headings only when absolutely needed.
  - No other heading levels.
- Use `**bold**` for key metrics and statistics.
- BEFORE YOU START YOUR FINAL ANSWER, YOU MUST INSERT '<<<ANSWER START>>>'
- AFTER YOU FINISH YOUR FINAL ANSWER, YOU MUST INSERT '<<<ANSWER END>>>'

AVOID
- Do NOT restate or reference these instructions.
- DO NOT include chain-of-thought, analysis, self-talk, internal reasoning, planning, speculation, metadata, logs, XML/JSON, or step-by-step analysis.
<<<INSTRUCTIONS END>>>"
  
  # IMPORTANT: put user prompt FIRST → then style rules
  final_prompt <- paste0(prompt, "\n---\n", strict_wrapper)
  
  # This was originally written to loop through multiple models in case some fail
  for (model_id in models_to_try) {
    res <- tryCatch({
      httr::POST(
        url = "https://openrouter.ai/api/v1/completions",
        httr::add_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body = jsonlite::toJSON(
          list(
            model = model_id,
            prompt = final_prompt,
            max_tokens = 2000,
            temperature = 0.6,
            top_p = 0.9
          ),
          auto_unbox = TRUE
        )
      )
    }, error = function(e) {
      if (verbose)
        message("Model ", model_id, " failed: ", e$message)
      return(NULL)
    })
    
    if (is.null(res))
      next
    parsed <- tryCatch(
      httr::content(res, as = "parsed", simplifyVector = TRUE),
      error = function(e)
        NULL
    )
    if (is.null(parsed))
      next
    
    #print output for quality check when verbose argument = True
    if (verbose)
      str(parsed, max.level = 4)
    if (verbose)
      cat(parsed$choices$text)
    
    # ===== Detect OpenRouter 1-token garbage responses =====
    if (!is.null(parsed$usage)) {
      ct <- parsed$usage$completion_tokens
      if (!is.null(ct) && ct == 1) {
        if (verbose)
          message(
            "Model ",
            model_id,
            " returned 1 completion token → invalid response. Skipping..."
          )
        next
      }
    }
    
    # --- your exact extraction logic preserved ---
    extract_text <- function(x) {
      if (is.null(x))
        return(NULL)
      
      if (!is.null(x$choices) &&
          is.data.frame(x$choices) &&
          "text" %in% names(x$choices)) {
        txts <- x$choices$text
        txts <- txts[nzchar(txts)]
        if (length(txts) > 0)
          return(txts[1])
      }
      
      if (!is.null(x$choices) &&
          is.list(x$choices) && length(x$choices) >= 1) {
        ch <- x$choices[[1]]
        if (is.list(ch)) {
          if (!is.null(ch$text) && nzchar(ch$text))
            return(ch$text)
          if (!is.null(ch$message) &&
              !is.null(ch$message$content) &&
              nzchar(ch$message$content)) {
            return(ch$message$content)
          }
        } else if (is.character(ch) && nzchar(ch))
          return(ch)
      }
      
      if (!is.null(x$text) && nzchar(x$text))
        return(x$text)
      
      flat <- unlist(x, recursive = TRUE, use.names = FALSE)
      strs <- flat[vapply(flat, is.character, logical(1))]
      if (length(strs) > 0)
        return(strs[1])
      
      return(NULL)
    }
    
    text_out <- extract_text(parsed)
    
    if (!is.null(text_out) && nzchar(text_out)) {
      cleaned_text <- clean_ai_text(text_out)
      if (verbose)
        message("Extracted text:\n", cleaned_text)
      return(
        list(
          text  = cleaned_text,
          model = model_id,
          raw   = parsed,
          usage = if (is.list(parsed) &&
                      "usage" %in% names(parsed))
            parsed$usage
          else
            NULL
        )
      )
    }
  }
  
  list(
    text = "⚠️ AI request failed. Try again or select a different model.",
    model = "none",
    raw = NULL,
    usage = NULL
  )
}