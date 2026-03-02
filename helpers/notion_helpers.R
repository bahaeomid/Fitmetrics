# ────────────────────────────────────────────────
#  NOTION API — POLISHED HELPER FUNCTIONS (R)
#  Purpose: Reusable utilities for querying multiple Notion databases
# ────────────────────────────────────────────────

# ===========================
# 0. INITIAL SETTINGS
# ===========================

#token <- ""   # Your Notion integration token
#urls <- list("https://www.notion.so/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx?v=xxxxxxxxxxxxxxxxxxxxxx")

# ===========================
# 1. LOAD REQUIRED LIBRARIES
# ===========================
library(notionapi)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# ===========================
# 2. URL PROCESSING UTILITIES
# ===========================

extract_notion_db_id <- function(url) {
  clean <- sub("^https://www.notion.so/", "", url)
  db_id <- sub("\\?.*$", "", clean)   # strip view ID & params
  db_id
}

extract_notion_view_id <- function(url) {
  m <- regmatches(url, regexpr("v=([A-Za-z0-9]+)", url))
  sub("v=", "", m)
}

# ===========================
# 3. NOTION CLIENT
# ===========================

get_notion_client <- function(token) {
  notion_client(token)
}

# ===========================
# 4. PAGE → CLEAN R ROW
# ===========================
# 4a. HELPER: GENERAL ROLLUP EXTRACTOR
# ===========================

extract_rollup_value <- function(rollup) {
  if (is.null(rollup)) return(NA)
  
  # Numeric rollup
  if (!is.null(rollup$number)) return(rollup$number)
  
  # Array rollup
  if (!is.null(rollup$array)) {
    values <- purrr::map_chr(rollup$array, function(item) {
      if (is.null(item)) return(NA)
      
      item_type <- item$type
      if (!is.null(item[[item_type]])) {
        if (item_type %in% c("rich_text", "title")) {
          paste(purrr::map_chr(item[[item_type]], "plain_text"), collapse = " ")
        } else if (item_type %in% c("number", "checkbox", "url", "email")) {
          item[[item_type]]
        } else {
          NA
        }
      } else {
        NA
      }
    })
    paste(values, collapse = ", ")
  } else {
    NA
  }
}

# ===========================
# 4b. HELPER: GENERAL RELATION EXTRACTOR
# ===========================

extract_relation_values <- function(related_ids, preloaded_dbs) {
  purrr::map_chr(related_ids, function(x) {
    found <- NULL
    
    for (db in preloaded_dbs) {
      page_index <- which(purrr::map_chr(db$raw_pages, "id") == x)
      
      if (length(page_index) > 0) {
        page <- db$raw_pages[[page_index]]
        
        # extract first title property dynamically
        title_prop <- page$properties %>% purrr::keep(~ .$type == "title") %>% .[[1]]
        
        if (!is.null(title_prop)) {
          found <- paste(purrr::map_chr(title_prop$title, "plain_text"), collapse = " ")
        }
        break
      }
    }
    
    ifelse(is.null(found), x, found)
  }) %>% paste(collapse = ", ")
}

# ===========================
# 4c. PAGE → CLEAN R ROW (rollups & relations supported)
# ===========================

notion_page_to_row <- function(page, preloaded_dbs = list()) {
  props <- page$properties
  data <- list()
  
  for (p in names(props)) {
    value <- props[[p]]
    
    col <- tryCatch({
      switch(
        value$type,
        
        # Basic types
        title       = paste(purrr::map_chr(value$title, "plain_text"), collapse = " "),
        rich_text   = paste(purrr::map_chr(value$rich_text, "plain_text"), collapse = " "),
        
        number = if (!is.null(value$number)) {
          num <- value$number
          
          # --- Percent handling ---
          is_percent <- FALSE
          
          if (!is.null(value$number_format) && value$number_format == "percent") {
            is_percent <- TRUE
          } else if (!is.null(value$format) && !is.null(value$format$number) &&
                     value$format$number == "percent") {
            is_percent <- TRUE
          }
          
          if (is_percent) {
            num <- num * 100
          }
          
          # fallback: column name contains % or "percent"
          if (grepl("perc|percent|%", p, ignore.case = TRUE) && abs(num) < 1) {
            num <- num * 100
          }
          
          num
        } else NA,
        
        checkbox    = value$checkbox,
        url         = value$url,
        email       = value$email,
        phone_number= value$phone_number,
        
        # Choice types
        select       = if (!is.null(value$select)) value$select$name else NA,
        multi_select = paste(purrr::map_chr(value$multi_select, "name"), collapse = ", "),
        status       = if (!is.null(value$status)) value$status$name else NA,
        date         = if (!is.null(value$date)) value$date$start else NA,
        files        = paste(purrr::map_chr(value$files, "name"), collapse = ", "),
        people       = paste(purrr::map_chr(value$people, "name"), collapse = ", "),
        
        # Relations
        relation = {
          related_ids <- purrr::map_chr(value$relation, "id")
          
          if (length(preloaded_dbs) > 0) {
            extract_relation_values(related_ids, preloaded_dbs)
          } else {
            paste(related_ids, collapse = ", ")
          }
        },
        
        # Formula
        formula = value$formula[[value$formula$type]],
        
        # Rollups
        rollup = extract_rollup_value(value$rollup),
        
        # Fallback
        value
      )
    },
    error = function(e) NA)
    
    data[[p]] <- col
  }
  
  tibble::as_tibble(data)
}

# ===========================
# 5. DB → TIBBLE
# ===========================

get_notion_db_as_tibble <- function(db_id, client, verbose = TRUE) {
  results <- list()
  next_cursor <- NULL
  total_pages <- 0
  
  repeat {
    res <- tryCatch({
      client$databases$query(database_id = db_id, start_cursor = next_cursor)
    }, error = function(e) {
      warning("Error querying DB: ", db_id, " — ", e$message)
      return(NULL)
    })
    
    if (is.null(res)) break
    
    results <- c(results, res$results)
    total_pages <- length(results)
    
    if (is.null(res$next_cursor)) break
    next_cursor <- res$next_cursor
  }
  
  if (verbose) message(glue::glue("DB {db_id}: Retrieved {total_pages} pages"))
  
  tibble_rows <- purrr::map(results, notion_page_to_row) %>% bind_rows()
  
  list(
    tibble = tibble_rows,
    raw_pages = results
  )
}

# ===========================
# 6. MULTI-DB LOADER
# ===========================

load_multiple_notion_dbs <- function(urls, token, verbose = TRUE) {
  client <- get_notion_client(token)
  
  db_ids <- purrr::map_chr(urls, extract_notion_db_id)
  db_data <- list()
  
  # First pass: load all DBs
  for (db_id in db_ids) {
    db_raw <- get_notion_db_as_tibble(db_id, client, verbose)
    
    title <- tryCatch({
      client$databases$retrieve(db_id)$title[[1]]$plain_text
    }, error = function(e) db_id)
    
    db_data[[title]] <- list(
      tibble = db_raw$tibble,
      raw_pages = db_raw$raw_pages
    )
  }
  
  # Second pass: resolve relations → titles
  for (title in names(db_data)) {
    db <- db_data[[title]]
    
    df <- purrr::map_dfr(
      db$raw_pages,
      function(page) notion_page_to_row(page, preloaded_dbs = db_data)
    )
    
    # ensure all Notion props appear as columns
    all_props <- unique(unlist(purrr::map(db$raw_pages, ~ names(.x$properties))))
    missing_cols <- setdiff(all_props, names(df))
    
    if (length(missing_cols) > 0) {
      for (col in missing_cols) df[[col]] <- NA
    }
    
    df <- clean_notion_tibble(df)
    db_data[[title]]$tibble <- df
  }
  
  db_data
}

# ============================================================
# CLEAN & FORMAT NOTION TIBBLE
# ============================================================

clean_notion_tibble <- function(
    df,
    round_digits = 2,
    convert_strings_to_numeric = TRUE,
    normalize_dates = TRUE,
    trim_text = TRUE,
    convert_select_to_factor = TRUE,
    detect_currencies = TRUE
) {
  
  df <- df %>% dplyr::mutate(
    dplyr::across(everything(), function(col) {
      
      # 0. List handling (rollups)
      if (is.list(col)) {
        col <- purrr::map_chr(col, ~ {
          if (length(.) == 1 && !is.list(.)) return(as.character(.))
          if (is.null(.)) return(NA_character_)
          paste0(unlist(.), collapse = ", ")
        })
      }
      
      # 1. Character cleanup
      if (is.character(col)) {
        
        if (trim_text) col <- trimws(col)
        col[col == ""] <- NA
        
        # detect ISO dates
        if (normalize_dates && all(grepl("^\\d{4}-\\d{2}-\\d{2}$", na.omit(col)))) {
          suppressWarnings(parsed <- as.Date(col))
          return(parsed)
        }
        
        # detect currencies
        if (detect_currencies && any(grepl("[$€£]|AED|USD|EUR|GBP", col))) {
          cleaned <- gsub("[$€,£]|AED|USD|EUR|GBP", "", col)
          cleaned <- gsub(",", "", cleaned)
          suppressWarnings(num <- as.numeric(cleaned))
          if (sum(!is.na(num)) > 0) return(round(num, round_digits))
        }
        
        # numeric strings → numeric
        if (convert_strings_to_numeric) {
          suppressWarnings(numeric_col <- as.numeric(col))
          if (sum(!is.na(numeric_col)) > 0) return(round(numeric_col, round_digits))
        }
        
        # lightweight factor detection
        if (convert_select_to_factor &&
            length(unique(col)) < 50 &&
            !any(grepl(" ", col))) {
          return(factor(col))
        }
      }
      
      # 2. Numeric cleanup
      if (is.numeric(col)) {
        return(round(col, round_digits))
      }
      
      # 3. Fallback
      col
    })
  )
  
  df
}

# ============================================================
# EXPORT DB TIBBLES
# ============================================================

export_db_tibbles <- function(
    db_list,
    prefix = NULL,
    envir = .GlobalEnv,
    if_exists = c("overwrite", "skip", "update")
) {
  if_exists <- match.arg(if_exists)
  
  for (db_name in names(db_list)) {
    tibble_data <- db_list[[db_name]]$tibble
    
    var_name <- gsub("[^a-zA-Z0-9_]", "_", db_name)
    if (!is.null(prefix)) var_name <- paste0(prefix, "_", var_name)
    
    if (exists(var_name, envir = envir)) {
      if (if_exists == "skip") {
        message("Skipping existing tibble: ", var_name)
        next
      }
      if (if_exists == "update") {
        message("Updating existing tibble: ", var_name)
        assign(var_name, tibble_data, envir = envir)
        next
      }
      if (if_exists == "overwrite") {
        message("Overwriting existing tibble: ", var_name)
        assign(var_name, tibble_data, envir = envir)
        next
      }
    }
    
    assign(var_name, tibble_data, envir = envir)
    message("Created tibble: ", var_name, " (", nrow(tibble_data), " rows)")
  }
}

# ============================================================
# LOAD & EXPORT IN ONE GO
# ============================================================

load_and_export_dbs <- function(urls, token, prefix = "notion") {
  dbs <- load_multiple_notion_dbs(urls, token)
  export_db_tibbles(dbs, prefix)
  dbs
}

# ============================================================
# STANDARDIZE DB NAMES (OPTIONAL)
# ============================================================

standardize_db_names <- function(db_list) {
  names(db_list) <- gsub(" ", "_", names(db_list))
  db_list
}

# ────────────────────────────────────────────────
#  END OF FILE
# ────────────────────────────────────────────────



# ────────────────────────────────────────────────
#  MULTI-DB CHEAT SHEET — USER REFERENCE
# ────────────────────────────────────────────────

# Assume your multi-DB structure is stored as:
# db_data <- load_multiple_notion_dbs(urls, token)

# Each database is a list containing:
# db_data[["DB Name"]]$tibble      # Flattened, human-readable tibble
# db_data[["DB Name"]]$raw_pages   # Full Notion API page objects

# -------------------------------------------------
# 1️⃣ Access flattened tibble
# -------------------------------------------------
# All relations are replaced with page titles
# All rollups are extracted and concatenated if arrays
# All standard properties are preserved

# Example:
# my_tibble <- db_data[["Tasks"]]$tibble
# head(my_tibble)

# -------------------------------------------------
# 2️⃣ Access original page IDs
# -------------------------------------------------
# Useful for referencing Notion pages for API updates

# Example:
# my_raw_pages <- db_data[["Tasks"]]$raw_pages
# page_ids <- purrr::map_chr(my_raw_pages, "id")

# -------------------------------------------------
# 3️⃣ Retrieve relation values manually
# -------------------------------------------------
# Sometimes you may want the original IDs instead of flattened titles

# Example:
# related_ids <- purrr::map_chr(my_raw_pages[[1]]$properties$Project$relation, "id")
# # Convert IDs to titles using helper:
# extract_relation_values(related_ids, db_data)

# -------------------------------------------------
# 4️⃣ Retrieve rollup values manually
# -------------------------------------------------
# Use the helper to extract numeric or array/text rollups

# Example:
# raw_rollup <- my_raw_pages[[1]]$properties$Summary$rollup
# extract_rollup_value(raw_rollup)

# -------------------------------------------------
# 5️⃣ Multi-item relations and rollups
# -------------------------------------------------
# Relations flattened as: "Task A, Task B, Task C"
# Rollups (array) flattened as: "Note 1, Note 2, Note 3"
# Numeric rollups remain numeric

# To split multi-item strings if needed:
# related_titles <- strsplit(my_tibble$Project[1], ",\\s*")[[1]]

# -------------------------------------------------
# 6️⃣ Recommended workflow
# -------------------------------------------------
# 1. Load all DBs:
# db_data <- load_multiple_notion_dbs(urls, token)
# 2. Access main tibble for analysis:
# tasks <- db_data[["Tasks"]]$tibble
# 3. For raw IDs or API operations, use raw_pages
# 4. Relations and rollups are auto-extracted; no manual mapping needed

# -------------------------------------------------
# 7️⃣ Levels of access summary
# -------------------------------------------------
# | Level     | Content                          | Use case                  |
# |-----------|---------------------------------|---------------------------|
# | tibble    | Flattened, human-readable       | Analytics, reporting      |
# | raw_pages | Full Notion API objects         | API updates, debugging    |
# | helpers   | extract_relation_values(),      | Custom processing, merges |
# |           | extract_rollup_value()          |                           |
