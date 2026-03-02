# ────────────────────────────────
###Calls "core_processing.R", from Helpers folder
###Calls "notion_helpers.R", from Helpers folder
###Calls "ai_helpers.R", from Helpers folder
# ────────────────────────────────

# ────────────────────────────────
# LOAD REQUIRED LIBRARIES (install if missing)
# ────────────────────────────────
library(shiny)
library(notionapi)
library(dplyr)
library(tidyr)
library(zoo)
library(purrr)
library(lubridate)
library(stringr)
library(ggplot2)
library(gridExtra)
library(broom)
library(DT)
library(plotly)
library(httr)
library(jsonlite)
library(commonmark)
library(markdown)
library(pagedown)

# ────────────────────────────────
# LOAD REQUIRED SCRIPTS
# ────────────────────────────────
source("helpers/notion_helpers.R")
source("helpers/core_processing.R")
source("helpers/ai_helpers.R")
readRenviron(".Renviron")  

# ────────────────────────────────
# HELPERS FOR THE CURRENT SCRIPT ONLY
# ────────────────────────────────
# Helper: to render list of tibbles as sub-tabs
render_section_tabs <- function(data_list, exclude_sections = NULL) {
  # Get all section names, optionally excluding some
  section_names <- setdiff(names(data_list), exclude_sections)
  
  section_tabs <- lapply(section_names, function(section) {
    df <- data_list[[section]]
    
    tabPanel(section, fluidRow(lapply(names(df), function(col) {
      column(
        width = 4,
        # 3 per row
        div(
          class = "overview-card",
          div(class = "card-title", gsub("_", " ", col)),
          div(class = "card-value", df[[col]][1])
        )
      )
    })))
  })
  
  do.call(tabsetPanel, section_tabs)
}

# Helper: format output in the AI chat box
format_ai_markdown <- function(md_text) {
  #initial check if the text exists in the chat box
  if (is.null(md_text) || md_text == "")
    return("")
  
  #split all lines individually
  lines <- unlist(strsplit(md_text, "\n", fixed = TRUE))
  
  # ====================================================
  # Process non-table lines
  # ====================================================
  # Track previous line marker type and depth (to be used in normalize_line below)
  prev_type <- NULL
  prev_depth <- 0
  
  # Helper for treating multiple indentation levels
  normalize_line <- function(line) {
    # List items
    if (grepl("^\\s*([-*]|\\d+\\.)\\s+", line)) {
      # Start list item on new line
      if (!is.null(prev_type))
        line <- paste0("\n", line)
      prev_type <<- "list"
      line
    } else {
      # Non-list lines: ensure each ends with a newline for proper paragraph separation
      prev_type <<- NULL
      paste0(line, "\n")
    }
  }
  
  # ====================================================
  # Detect table blocks and build an HTML table
  # ====================================================
  in_table <- FALSE
  table_buffer <- character(0)
  processed_lines <- character(0)
  
  #Helper to build table
  flush_table <- function(buffer) {
    if (length(buffer) < 2)
      return("")
    if (grepl("^\\|?\\s*-{3,}", buffer[2]))
      buffer <- buffer[-2]
    
    # Build HTML rows
    rows <- lapply(buffer, function(row) {
      cells <- strsplit(gsub("^\\||\\|$", "", row), "\\|")[[1]]
      cells <- trimws(cells)
      cells_html <- sapply(cells, function(cell) {
        html <- commonmark::markdown_html(cell)
        gsub("^<p>|</p>\n?$", "", html)
      })
      paste0("<td>", cells_html, "</td>", collapse = "")
    })
    
    row_html <- sapply(rows, function(r)
      paste0("<tr>", r, "</tr>"))
    
    paste0(
      "<div class='ai-md-table-wrapper'>",
      "<table class='ai-md-table'>",
      paste(row_html, collapse = ""),
      "</table>",
      "</div>"
    )
    
  }
  
  # ====================================================
  # process all lines (table and non-table)
  # ====================================================
  for (line in lines) {
    if (grepl("^\\|", line)) {
      table_buffer <- c(table_buffer, line)
      in_table <- TRUE
    } else {
      if (in_table) {
        processed_lines <- c(
          processed_lines,
          flush_table(table_buffer),
          "<br>"   # <-- force one blank line after table
        )
        
        table_buffer <- character(0)
        in_table <- FALSE
      }
      processed_lines <- c(processed_lines, normalize_line(line))
    }
  }
  
  # Flush table if file ends with a table
  if (in_table)
    processed_lines <- c(
      processed_lines,
      flush_table(table_buffer),
      "<br>"   # <-- force one blank line after table
    )
  
  # Rebuild text for final output
  md_text <- paste(processed_lines, collapse = "\n")
  md_text <- gsub("\\n{3,}", "\n\n", md_text, perl = TRUE)
  
  # Convert Markdown to HTML
  html <- commonmark::markdown_html(md_text)
  htmltools::HTML(html)
}

# ────────────────────────────────
# API Keys & Tokens
# ────────────────────────────────
# Need to place the keys in .Renviron file in the home directory - check normalizePath("~") for Home Directory
token <- Sys.getenv("NOTION_TOKEN")
api_key <- Sys.getenv("OPENROUTER_API_KEY")

# optional safety check
if (token == "" ||
    api_key == "")
  stop("API keys not set! Check .Renviron")

# ────────────────────────────────
# NOTION API INPUTS
# ────────────────────────────────
urls <- list(
  "https://www.notion.so/2a964bc18fb28033815aff79a356c899?v=2a964bc18fb28016801b000c5a315559&pvs=25",
  "https://www.notion.so/2a964bc18fb28050b0e7dfe0a0d9e374?v=2a964bc18fb280188ad2000caf882ff2&pvs=25",
  "https://www.notion.so/2a664bc18fb280a6b793c7c5ab5e79d0?v=2a664bc18fb280b29f84000c60a0836e&pvs=25",
  "https://www.notion.so/2a964bc18fb2807ea55bfc2c74b477de?v=2a964bc18fb280b0973c000c0e0d57a4",
  "https://www.notion.so/2a964bc18fb2801bafa3ce1c0f35035f?v=2a964bc18fb280e8820e000cad95c6e9"
)

# ────────────────────────────────
# AI PROVIDER (OpenRouter) API INPUTS
# ────────────────────────────────
#list of the best free models
#free_models_candidates <- c(
#  "mistralai/mistral-7b-instruct:free",
# "google/gemma-3n-e2b-it:free",
#  "qwen/qwen3-4b:free",
#  "nvidia/nemotron-nano-9b-v2:free"
#)

#list of models
all_models <- get_openrouter_all_models(api_key)
free_models <- all_models[grepl("free", all_models, ignore.case = TRUE)]

# ────────────────────────────────
# SHINY UI
# ────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "

/* -----------------------------
   Global heading styling for Shiny UI
----------------------------- */
h1, h2, h3, h4, h5, h6 {
  color: #1B4F72 !important;   /* deep blue, consistent with HTML */
  font-weight: 700;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
  color: #000000;
}

/* -----------------------------
   MAIN SIDEBAR PANEL
----------------------------- */
 .row > .col-sm-2 {
  min-width: 230px !important;
  flex: 0 0 230px !important;
}

/* Sidebar buttons */
.sidebar .btn, .btn {
  background-color: #D6EAF8;
  color: #1B4F72;
  border: 1px solid #A9CCE3;
  border-radius: 6px;
  font-weight: 600;
  margin-bottom: 10px;
  width: 100%;
  transition: all 0.2s;
}

.sidebar .btn:hover, .btn:hover {
  background-color: #AED6F1;
  color: #1B4F72;
  box-shadow: 0 2px 5px rgba(0,0,0,0.1);
}

/* Sidebar selectInput styling */
.sidebar .form-control {
  border-radius: 6px;
  border: 1px solid #D6EAF8;
  background-color: #FBFCFC;
  color: #1B4F72;
  padding: 4px 8px;
}

/* Progress message styling */
#progress_ui > div {
  background-color: #EBF5FB;
  border: 1px solid #D6EAF8;
  border-radius: 6px;
  padding: 6px 10px;
  font-weight: 600;
  color: #1B4F72;
  text-align: center;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05);
  margin-top: 8px;
}

/* -----------------------------
   TAB HEADERS (TOP LEVEL)
----------------------------- */
.nav.nav-tabs > li > a {
  font-size: 16px;
  font-weight: 700;
  color: #2C3E50;
  padding: 8px 12px;
}
.nav.nav-tabs > li.active > a {
  background-color: #D6EAF8;
  color:#1B4F72;
  border-radius: 4px 4px 0 0;
  border-bottom: 2px solid #1B4F72;
}

/* -----------------------------
   UNIFIED SUBHEADINGS
----------------------------- */
.subheading {
  font-size: 16px;
  font-weight: 700;
  color:#1A5276;
  margin-top: 18px;
  margin-bottom: 8px;
  border-bottom: 1px solid #D6EAF8;
  padding-bottom: 4px;
}

/* -----------------------------
   INSIGHTS BULLETS - CARD STYLE
----------------------------- */
.insight-group {
  background-color: #FDFDFD;        /* subtle fill */
  border: 1px solid #D6EAF8;
  border-radius: 6px;
  padding: 6px 10px;                /* reduced padding */
  margin-bottom: 10px;
  box-shadow: 0 1px 2px rgba(0,0,0,0.05);
}

.insight-group .subheading {
  font-size: 14px;
  font-weight: 600;
  color: #1B4F72;
  margin: 0 0 4px 0;                /* remove top margin, reduce bottom margin */
  padding: 0;
}

.insight-line {
  font-size: 13px;
  font-weight: 500;
  color: #34495E;
  margin: 0 0 2px 0;                /* remove top margin, tiny bottom margin */
  padding-left: 8px;
  border-left: 3px solid #A9CCE3;
  line-height: 1.3;
}

/* -----------------------------
   METRIC LINES (Stats, Overview)
----------------------------- */
.metric {
  font-size: 14px;
  margin-top: 4px;
  margin-bottom: 6px;
  margin-left: 8px;
  color:#34495E;
}

/* -----------------------------
   SECTION SEPARATION
----------------------------- */
.tab-content {
  margin-top: 12px;
  border-top: 1px solid #D6EAF8;
  padding-top: 12px;
}

/* -----------------------------
   TAB PANEL CARD EFFECT
----------------------------- */
/* Only outer tab-pane (direct child of tabsetPanel) gets background */
.tab-content > .tab-pane {
  background-color: #F8F9F9;
  border: 1px solid #D6EAF8;
  border-radius: 6px;
  padding: 15px;
  box-shadow: 0 1px 4px rgba(0,0,0,0.05);
  margin-bottom: 10px;
}

/* Nested tab-panes inherit no background */
.tab-pane .tab-pane {
  background-color: transparent;
  border: none;
  box-shadow: none;
  padding: 0;
  margin: 0;
}

/* -----------------------------
   BUTTON TILES
----------------------------- */
.tile-btn {
  margin-bottom: 8px;
  font-size: 14px;
}

/* -----------------------------
   DATATABLE FONT CONTROL
----------------------------- */
table.dataTable tbody td {
  font-size: 13px;
}
table.dataTable thead th {
  font-size: 14px;
}

/* -----------------------------
   COLLAPSIBLE HEADER - SLEEK
----------------------------- */
details.overview-header {
  border: 1px solid #D6EAF8;
  border-radius: 6px;
  padding: 6px 12px;
  margin: 16px 0;
  background-color: #FDFDFD;
  box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  transition: all 0.2s;
}

details.overview-header[open] {
  padding-bottom: 12px;
}

details.overview-header summary {
  font-size: 14px;
  font-weight: 600;
  cursor: pointer;
  margin-bottom: 8px;
  list-style: none;
  color: #1B4F72;
  display: inline-block;       /* hug content */
  padding: 4px 8px;
  background-color: #EBF5FB;  /* subtle fill */
  border-radius: 4px;
}

details.overview-header summary::-webkit-details-marker {
  display:none;
}

details.overview-header summary::after {
  content: '\\25BC';
  float: right;
  font-size: 12px;
  color: #1B4F72;
  transition: transform 0.2s;
}

details.overview-header[open] summary::after {
  content: '\\25B2';
}

/* -----------------------------
   HEADER CARDS - MINIMALISTIC
----------------------------- */
.overview-card {
  background-color: #F8F9F9;
  border: 1px solid #D6EAF8;
  border-radius: 6px;
  padding: 8px 6px;
  margin-bottom: 8px;
  text-align: center;
  box-shadow: 0 1px 2px rgba(0,0,0,0.05);
  font-size: 13px;
  transition: all 0.2s;
}
.overview-card:hover {
  box-shadow: 0 2px 5px rgba(0,0,0,0.1);
}
.overview-card .card-title {
  font-weight: 600;
  color: #1B4F72;
  font-size: 12px;
  margin-bottom: 2px;
  background-color: #D6EAF8;
  border-radius: 3px;
  padding: 2px 4px;
}
.overview-card .card-value {
  font-weight: 500;
  color: #34495E;
  font-size: 14px;
  margin-top: 4px;
}

/* -----------------------------
   PLOTS TAB STYLING
----------------------------- */
.plot-section {
  margin-top: 12px;
}

#selected_plot_ui {
  margin-top: 16px;
}

/* plotly Hover labels */
.plotly .hovertext {
  fill: #ffffff !important;      /* white text */
  font-weight: 600 !important;
  font-size: 12px !important;
}

/* plotly Annotation / text inside plot (like direct labels) */
.plotly .annotation text {
  fill: #1B4F72 !important;      /* dark blue */
  font-weight: 600 !important;
  font-size: 12px !important;
}

/* -----------------------------
   AI COACH
----------------------------- */

/* AI SIDEBAR PANEL SLEEK STYLE */
.sidebar .well, .well {
  background-color: #FDFDFD;        /* subtle light fill */
  border: 1px solid #D6EAF8;       /* soft border */
  border-radius: 8px;
  padding: 14px 12px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05);
  margin-bottom: 0px !important;
}

/* PROMPT BOX */
.user-message {
  background-color:#D6EAF8;
  border-left:4px solid #1B4F72;
  padding:6px 8px;
  border-radius:4px;
  margin-bottom:10px;
}

/* CHAT BOX */
.ai-message {
  background-color:#FDFDFD;
  border: 1px solid #A9CCE3;
  border-radius: 6px;
  padding: 6px 10px;
  margin-bottom: 8px;
  width: 100%;
  max-width: 100%;
  overflow-x: auto;  /* allow scrolling */
  overflow-y: visible;
  word-wrap: break-word;
  white-space: normal;
  color: #000000;
  line-height: 1.3;   /* tighter line spacing for text and bullets */
}

.btn-block {
  width: 100%;
  margin-bottom: 4px; /* tighter spacing */
  font-size: 13px;
}

/* MODELS DROPDOWN */
.ai-model-wrapper {
  flex-wrap: nowrap;      /* prevent stacking */
  align-items: baseline; /* align text baselines perfectly */
  gap: 8px;              /* spacing between label and dropdown */
  margin-top: 0px;
  pointer-events: auto;  /* ensure children are clickable */
}

.ai-model-wrapper .selectize-control.single {
flex-shrink: 0;             /* prevent shrinking */
vertical-align: baseline;   /* align with text baseline */
}

.ai-model-wrapper .selectize-control.single .selectize-input {
  border-radius: 6px;
  border: 1.5px solid #A9CCE3;  /* slightly darker/more visible border */
  background-color: #FBFCFC;
  color: #1B4F72;
  font-weight: 400;
  font-size: 14px;
  padding: 6px 10px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05);
  transition: all 0.2s;
  height: 28px;        /* match your label font-size + padding */
  line-height: 1.2;    /* match label baseline */
}

/* Prevent dropdown shrinking while typing */
.selectize-control.single .selectize-input input {
  width: 100% !important;     /* the key fix */
  min-width: 100% !important; /* ensures full width */
  flex: 1 1 auto !important;  /* allow it to grow, not shrink */
}

.ai-model-wrapper .selectize-control.single .selectize-input .item {
  white-space: nowrap;       /* keep it on one line */
  overflow: hidden;          /* hide overflow */
  text-overflow: ellipsis;   /* show ... for long text */
  max-width: 180px;          /* adjust width as needed */
  display: inline-block;
  vertical-align: middle;
}

/* Hover/focus effect (slightly stronger) */
.ai-model-wrapper .selectize-control.single .selectize-input.focus,
.ai-model-wrapper .selectize-control.single .selectize-input:hover {
  background-color: #EBF5FB;
  border-color: #7FB3D5;
  box-shadow: 0 2px 5px rgba(0,0,0,0.08);
}

/* Dropdown items */
.ai-model-wrapper .selectize-dropdown-content div.option {
  color: #1B4F72;
  font-weight: 400;
  font-size: 14px;
  background-color: #FBFCFC;
  white-space: normal;     /* allow wrapping */
  word-break: break-word;  /* break long words if necessary */
}

/* Highlighted option */
.ai-model-wrapper .selectize-dropdown-content div.option.active {
  background-color: #D6EAF8;
  color: #1B4F72;
  font-weight: 400;
}

.selectize-dropdown {
max-height: 300px;
overflow-y: auto;
width: auto !important;   /* remove fixed width */
min-width: 100%;          /* ensure at least parent width */
max-width: 100%;          /* don’t exceed parent */
}

/* Label + count inline, never wrap */
.ai-model-wrapper label {
  display: flex;
  align-items: center;   /* perfectly vertically aligned */
  gap: 4px;              /* spacing between label and count */
  white-space: nowrap;   /* prevents count from wrapping to next line */
  margin: 0;
  padding: 0;
}

/* Main label text */
.ai-model-label-text {
  font-weight: 600;
  color: #1B4F72;
  font-size: 13px;
}

/* Inline compact count */
.ai-model-count-inline {
  font-size: 9px;
  color: #1B4F72;
  font-weight: 400;
  line-height: 1;
}

/* AI MARKDOWN TABLE */
.ai-md-table-wrapper {
  overflow-x: auto;        /* horizontal scroll if needed */
  width: 100%;             /* take full width of container */
  max-width: 100%;         /* prevent flex parent from expanding */
}

.ai-md-table {
  border-collapse: collapse;
  width: max-content;    /* natural width, let it grow horizontally */
  table-layout: auto;    /* let columns size naturally */
}

/* Style only the FIRST row as a header */
.ai-md-table tr:first-child td {
  background-color: #D6EAF8 !important;
  color: #1B4F72 !important;
  font-weight: 800 !important;
  border-bottom: 2px solid #1B4F72 !important;
  text-align: left;
  white-space: nowrap;
  padding: 8px 12px !important;
}

.ai-md-table td,
.ai-md-table th {
  border: 1px solid #1B4F72;
  padding: 6px 10px;
  font-size: 13px;
  word-wrap: break-word;    /* break long content inside cells */
  white-space: normal;      /* allow wrapping */
}

/* Zebra rows for cleaner readability */
.ai-md-table tr:nth-child(even) {
  background: #F7FBFE;
}

/* prevent zebra-striping from affecting header row */
.ai-md-table tr:first-child {
  background-color: #D6EAF8 !important;
}

/* Let bold & markdown render naturally inside cells */
.ai-md-table td strong {
  font-weight: 600;
}

")
    )
    
  ),
  
  
  tags$script(
    HTML(
      "
/* -----------------------------
 AI COACH
----------------------------- */
Shiny.addCustomMessageHandler('scroll_ai_chat', function(message) {
const chatDiv = document.getElementById('ai_chat_container');

// Wait for DOM update AFTER message is inserted
setTimeout(() => {
  if (chatDiv) {
    chatDiv.scrollTop = chatDiv.scrollHeight;
  }
}, 60);  // small delay ensures it scrolls to the real bottom
});
"
  )
),
  
  titlePanel("🥇 FitMetrics"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      div(
        class = "sidebar",
        actionButton("fetch_btn", "🔄 Fetch Notion DBs"),
        hr(),
        uiOutput("identity_selector"),
        hr(),
        actionButton("rerun_btn", "⚡ Re-run Analysis"),
        hr(),
        uiOutput("progress_ui")
      )
    )
    ,
    
    mainPanel(
      # Collapsible header using <details>
      tags$details(
        class = "overview-header",
        tags$summary("☰ Header Info (click to expand)"),
        uiOutput("overview_header")  # cards will render here
      )
      
      ,
      
      # Tabs (Insights first)
      tabsetPanel(
        id = "tabs",
        selected = "💡 INSIGHTS",
        # default outer tab
        tabPanel("💡 INSIGHTS", uiOutput("insights_ui")),
        tabPanel("♾️ STATS", uiOutput("stats_ui")),
        tabPanel("📊 CHARTS", uiOutput("plots_ui")),
        
        tabPanel(
          "✨ AI COACH",
          div(
            style = "display: flex; flex-wrap: nowrap; gap: 12px; align-items: stretch;",
            # flex row, wrap on small screens
            
            # ============================
            #   LEFT SIDEBAR (ICON TILES)
            # ============================
            div(
              style = "flex: 0 0 40px; display: flex; flex-direction: column; gap: 6px;",
              
              # Spacer to align top tile with 'Your Question'
              div(style = "height: 40px;"),
              
              # 1) Short Summary
              actionButton(
                "ai_btn_summary",
                HTML(
                  '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                     <rect x="3" y="4" width="18" height="18" rx="2" ry="2"/>
                     <line x1="3" y1="10" x2="21" y2="10"/>
                     <line x1="7" y1="2" x2="7" y2="6"/>
                     <line x1="17" y1="2" x2="17" y2="6"/>
                   </svg>'
                ),
                title = 'Provide Short Summary',
                class = "tile-btn",
                style = "width:100%; height:40px; display:flex; justify-content:center; align-items:center; padding:0;"
              ),
              
              # 2) Weight Change Explainer
              actionButton(
                "ai_btn_delta",
                HTML(
                      '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                         <polygon points="12 4 20 20 4 20"/>
                       </svg>'
                ),
                title = 'Explain Weight Change',
                class = "tile-btn",
                style = "width:100%; height:40px; display:flex; justify-content:center; align-items:center; padding:0;"
              ),
              
              # 3) Long term trend Analyzer
              actionButton(
                "ai_btn_longterm",
                HTML(
                  '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                <circle cx="12" cy="12" r="10"/>
                <circle cx="12" cy="12" r="6"/>
                <circle cx="12" cy="12" r="2"/>
                  </svg>'
                ),
                title = 'Assess Long-Term Alignment',
                class = "tile-btn",
                style = "width:100%; height:40px; display:flex; justify-content:center; align-items:center; padding:0;"
              ),
              
              # 4) Progress Rate
              actionButton(
                "ai_btn_progressrate",
                HTML(
                  '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                     <polyline points="4 16 10 10 14 14 20 6" />
                     <circle cx="20" cy="6" r="2" />
                   </svg>'
                ),
                title = 'Evaluate Progress Rate',
                class = "tile-btn",
                style = "width:100%; height:40px; display:flex; justify-content:center; align-items:center; padding:0;"
              ),
              
              # 5) Strength Progress (barbell icon)
              actionButton(
                "ai_btn_signal",
                HTML(
                  '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                    <path d="M3 12h6M15 12h6M12 3v6M12 15v6M5.5 5.5l5 5M18.5 18.5l-5-5"/>
                  </svg>'
                ),
                title = 'Assess Contradictory Signals',
                class = "tile-btn",
                style = "width:100%; height:40px; display:flex; justify-content:center; align-items:center; padding:0;"
              ),
              
              # 6) Strength & Recovery Guidance
              actionButton(
                "ai_btn_strength_recovery",
                HTML(
                  '<svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                        <line x1="4" y1="12" x2="20" y2="12"/>
                                        <line x1="6" y1="8" x2="6" y2="16"/>
                                        <line x1="18" y1="8" x2="18" y2="16"/>
                                      </svg>'
                ),
                title = 'Evaluate Strength & Recovery',
                class = "tile-btn",
                style = "width:100%; height:40px; display:flex; justify-content:center; align-items:center; padding:0;"
              )
              
            ),
            # ============================
            # Middle column: Prompt + Buttons + Model dropdown
            # ============================
            div(
              style = "flex: 1; min-width: 330px; display: flex; flex-direction: column; gap: 4px;",
              div("💬 Your Question", class = "subheading", style =
                    "margin-top:0; padding-top:0;"),
              
              # Prompt input
              div(
                class = "well",
                style = "flex:1; display:flex; flex-direction: column; padding:10px 12px;",
                textAreaInput(
                  "ai_prompt",
                  label = NULL,
                  placeholder = "Why is my weight dropping?",
                  width = "100%",
                  rows = 14
                )
              ),
              
              # Buttons
              div(
                style = "display:flex; gap: 10px; margin-top: 4px;",
                actionButton("ai_send", "။၊ Ask Coach", class = "btn-primary"),
                actionButton("ai_clear", "🧹 Clear Chat")
              ),
              
              # Inline label + dropdown, always single line
              div(
                class = "ai-model-wrapper",
                style = "display: flex; align-items: baseline; gap: 8px; white-space: nowrap;",
                
                # Inline label + count
                tags$span(
                  style = "display: inline-flex; align-items: baseline; gap: 4px; white-space: nowrap; flex-shrink: 0;",
                  tags$span(class = "ai-model-label-text", "Select Model"),
                  tags$span(class = "ai-model-count-inline", paste0("(", length(free_models),"/",length(all_models), ")"))
                ),
                
                # Dropdown
                selectizeInput(
                  inputId = "ai_model_select",
                  label = NULL,
                  choices = all_models,
                  selected = "mistralai/mistral-7b-instruct:free",
                  options = list(create = TRUE)   # <-- allow free text input
                )
              )
              
            )
            
            ,
            
            # ============================
            #   RIGHT COLUMN (CHAT HISTORY)
            # ============================
            div(
              style = "flex: 2;min-width: 400px; display:flex; flex-direction: column;",
              div("🗨️ Chat History", class = "subheading", style =
                    "margin-top:0; padding-top:0;"),
              div(
                class = "well",
                id = "ai_chat_container",
                style = "flex:1 1 auto; min-height:450px; max-height:450px; overflow-x: hidden; overflow-y:auto; padding:10px 12px; scroll-behavior:smooth;",
                htmlOutput("ai_chat_ui")
              )
            )
          )
        ),
        
        tabPanel("⬇️ EXPORT", uiOutput("rawdata_ui")),
        
      )
      
    )
  )
)

# ────────────────────────────────
# SHINY SERVER
# ────────────────────────────────
server <- function(input, output, session) {
  # --------------------------
  # Initialize reactive values
  # --------------------------
  rv <- reactiveValues(
    db_list = NULL,
    final = NULL,
    progress_msg = "",
    selected_plot = NULL,
    selected_raw = NULL,
    #AI RVs
    ai_history = list(),
    # store past prompts/responses
    ai_loading = FALSE,
    # track API request in progress
    ai_latest = ""
  )
  
  # --------------------------
  # Progress messages
  # --------------------------
  output$progress_ui <- renderUI({
    req(rv$progress_msg)
    div(style = "padding:5px; border-radius:5px; background-color:#D6EAF8; color:#1B4F72; font-weight:bold;", rv$progress_msg)
  })
  
  update_progress <- function(msg) {
    rv$progress_msg <- msg
  }
  
  # --------------------------
  # Identity selector
  # --------------------------
  output$identity_selector <- renderUI({
    selectInput("identity_select", "🏃 Select Identity", choices = character(0))
  })
  
  # --------------------------
  # Fetch DBs
  # --------------------------
  observeEvent(input$fetch_btn, {
    withProgress(message = "Fetching Notion DBs...", value = 0, {
      # Step 1: Fetch DBs
      rv$db_list <- tryCatch({
        incProgress(0.3, detail = "Loading Notion DBs...")
        load_multiple_notion_dbs(urls, token) %>% standardize_db_names()
      }, error = function(e) {
        showNotification("⚠️ Error fetching Notion DBs.", type = "error")
        return(NULL)
      })
      
      req(rv$db_list)
      
      # Step 2: Populate identity selector
      identities <- rv$db_list$Training_Profiles$tibble$Identity %>% unique()
      updateSelectInput(session,
                        "identity_select",
                        choices = identities,
                        selected = identities[1])
      
      # Step 3: Run default analysis
      incProgress(0.6, detail = "Running analysis for default identity...")
      rv$final <- tryCatch({
        process_all(rv$db_list, identities[1])
      }, error = function(e) {
        showNotification("⚠️ Error: No data found for default identity or an error occurred.",
                         type = "error")
        return(NULL)
      })
      
      # Final step: done
      incProgress(0.1, detail = "✅ Done fetching & processing DBs")
      
      update_progress("✅ Fetching & processing DBs complete.")
    })  # end withProgress
  })
  
  # --------------------------
  # Re-run analysis
  # --------------------------
  observeEvent(input$rerun_btn, {
    req(rv$db_list, input$identity_select)
    
    #clear ai chat box
    rv$ai_history <- list()   # clears chat box
    
    withProgress(
      message = paste0("📡 Re-running analysis for ", input$identity_select, "..."),
      value = 0,
      {
        # Step 1: Start analysis
        incProgress(0.7, detail = "Processing data...")
        rv$final <- tryCatch({
          process_all(rv$db_list, input$identity_select)
        }, error = function(e) {
          showNotification(
            paste0(
              "⚠️ Error: No data found for ",
              input$identity_select,
              " or an error occurred."
            ),
            type = "error"
          )
          return(NULL)
        })
        
        # Step 2: Done
        if (!is.null(rv$final)) {
          incProgress(0.3, detail = "✅ Done re-running analysis")
          update_progress(paste0(
            "✅ Re-running analysis for ",
            input$identity_select,
            " complete."
          ))
        }
        
      }
    )  # end withProgress
  })
  
  # --------------------------
  # Overview: header info
  # --------------------------
  output$overview_header <- renderUI({
    req(rv$final)
    
    header_data <- c(rv$final$Stats$Profile$Header_Info,
                     rv$final$Stats$Current_Week$Header_Info)
    
    metric_names <- names(header_data)
    n_per_row <- 3
    rows <- split(metric_names, ceiling(seq_along(metric_names) / n_per_row))
    
    tagList(lapply(rows, function(row_metrics) {
      fluidRow(lapply(row_metrics, function(m) {
        column(width = floor(12 / n_per_row),
               div(
                 class = "overview-card",
                 div(class = "card-title", gsub("_", " ", m)),
                 div(class = "card-value", header_data[[m]][1])
               ))
      }))
    }))
  })
  
  # --------------------------
  # Stats: nested tabs
  # --------------------------
  output$stats_ui <- renderUI({
    req(rv$final)
    
    # helper: render a tibble as overview cards
    render_tibble_as_cards <- function(df) {
      fluidRow(lapply(names(df), function(col) {
        column(
          width = 4,
          # 3 per row
          div(
            class = "overview-card",
            div(class = "card-title", gsub("_", " ", col)),
            div(class = "card-value", df[[col]][1])
          )
        )
      }))
    }
    
    tabsetPanel(
      tabPanel(
        "📅 CURRENT WEEK",
        render_section_tabs(rv$final$Stats$Current_Week, exclude_sections = "Header_Info")
      ),
      tabPanel(
        "🧭 PROFILE",
        render_section_tabs(rv$final$Stats$Profile, exclude_sections = "Header_Info")
      )
    )
    
  })
  
  # --------------------------
  # Insights: improved bullets
  # --------------------------
  output$insights_ui <- renderUI({
    req(rv$final)
    
    clean_title <- function(x) {
      gsub("_", " ", x)
    }
    
    render_insight_group <- function(title, statements) {
      clean <- statements[statements != ""]
      tagList(tags$div(
        class = "insight-group",
        tags$div(clean_title(title), class = "subheading"),
        lapply(clean, function(s) {
          str_split(s, "\\s*\\|\\s*")[[1]] %>%   # split by pipe
            lapply(function(line) {
              tags$p(line, class = "insight-line")
            })
        })
      ))
    }
    
    render_insights_tab <- function(insights_list) {
      tagList(lapply(names(insights_list), function(section_name) {
        render_insight_group(section_name, insights_list[[section_name]])
      }))
    }
    
    tabsetPanel(
      type = "tabs",
      selected = "📅 CURRENT WEEK",
      tabPanel(
        "📅 CURRENT WEEK",
        br(),
        render_insights_tab(rv$final$Insights$Current_Week)
      ),
      tabPanel(
        "🧭 PROFILE",
        br(),
        render_insights_tab(rv$final$Insights$Profile)
      )
    )
  })
  
  # --------------------------
  # Plots
  # --------------------------
  # icon displayed in the plot tiles
  icon_svg <- '<svg width="20" height="20" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
  <line x1="4" y1="20" x2="4" y2="14"/>
  <line x1="10" y1="20" x2="10" y2="10"/>
  <line x1="16" y1="20" x2="16" y2="16"/>
  <line x1="22" y1="20" x2="22" y2="12"/>
  </svg>'
  
  # generating plot tiles
  output$plots_ui <- renderUI({
    req(rv$final)
    plot_names <- names(rv$final$Plots$Profile)
    
    tagList(div(class = "plot-section", fluidRow(lapply(plot_names, function(pn) {
      column(
        2,
        actionButton(
          paste0("plot_btn_", pn),
          HTML(
            paste0(
              icon_svg,
              '<span style="margin-left:6px;">',
              pn,
              '</span>'
            )
          ),
          width = "100%",
          class = "tile-btn",
          style = "height:40px; display:flex; align-items:center; justify-content:center; padding:0;"
        )
      )
    })), uiOutput("selected_plot_ui")))
  })
  
  observe({
    req(rv$final)
    plot_names <- names(rv$final$Plots$Profile)
    
    lapply(plot_names, function(pn) {
      observeEvent(input[[paste0("plot_btn_", pn)]], {
        rv$selected_plot <- pn
      })
    })
  })
  
  output$selected_plot_ui <- renderUI({
    req(rv$selected_plot)
    div(
      style = "padding-top: 16px; padding-bottom: 12px;",
      # Plot
      plotlyOutput(
        "selected_plot_render",
        height = "500px",
        width = "100%"
      ),
      
      # ---- Notes / Caption below the plot ----
      div(
        style = "margin-top: 4px; font-size: 12px; color: #6e6e6e; font-style: italic; text-align: left;",
       htmlOutput("plot_notes")
      )
    )
  })
  
  output$selected_plot_render <- renderPlotly({
    req(rv$selected_plot)
    
    p <- rv$final$Plots$Profile[[rv$selected_plot]]
    
    # Subset data to last 12 weeks
    df_filtered <- p$data
    last_12_weeks <- tail(sort(unique(df_filtered$Week_Start)), 12)
    df_filtered <- df_filtered[df_filtered$Week_Start %in% last_12_weeks, ]
    p$data <- df_filtered   # Replace the plot's data with the filtered data
    p <- p + scale_x_date(limits = range(last_12_weeks))   # restrict x-axis scale
    
    ggplotly(p) %>%
      layout(
        legend = list(
          x = 1,
          # position top-right inside the plot
          y = 1,
          xanchor = "right",
          yanchor = "top",
          bgcolor = "rgba(255,255,255,0.6)",
          # semi-transparent background
          bordercolor = "white",
          borderwidth = 0
        ),
        margin = list(
          t = 50,
          b = 50,
          l = 60,
          r = 50
        ),
        xaxis = list(
          rangeslider = list(
            visible = TRUE,
            thickness = 0.01,
            bgcolor = "rgba(255,255,255,1)",
            borderwidth = 0
          )
        )
      )
  })
  
  output$plot_notes <- renderText({
    req(rv$selected_plot)
    p <- rv$final$Plots$Profile[[rv$selected_plot]]
    # Pull the caption from your ggplot object or define manually
    p <- rv$final$Plots$Profile[[rv$selected_plot]]
    caption_text <- p$labels$caption
    
    # Replace bullet points with <br> and &bull; for HTML rendering
    caption_html <- gsub("•", "&bull;", caption_text)
    caption_html <- gsub("\n", "<br>", caption_html)
    
    HTML(caption_html)
  })
  
  # --------------------------
  # AI Coach
  # --------------------------
  # AI chat UI
  output$ai_chat_ui <- renderUI({
    if (length(rv$ai_history) == 0)
      return(NULL)
    
    tagList(lapply(seq_along(rv$ai_history), function(i) {
      msg <- rv$ai_history[[i]]
      
      # Determine class for user vs AI
      user_class <- "user-message"
      ai_class   <- "ai-message"
      
      tagList(
        # User prompt
        div(
          class = user_class,
          style = "background-color:#D6EAF8; border-left:4px solid #1B4F72; padding:6px 8px; border-radius:4px; margin-bottom:4px;",
          tags$strong("💬 You: "),
          msg$prompt
        ),
        # AI response
        div(
          class = ai_class,
          style = "background-color:#FDFDFD; border-left:4px solid #A9CCE3; padding:6px 8px; border-radius:4px; margin-bottom:8px;",
          tags$strong(paste0("🧠 FitMetrics AI (", msg$model, "): ")),
          format_ai_markdown(msg$response)
          
        )
      )
    }))
  })
  
  # Clear chat
  observeEvent(input$ai_clear, {
    rv$ai_history <- list()
  })
  
  # ---- question template buttons (ai_prompt input by user) ----
  # 1) AI Coach Summary Button ----
  observeEvent(input$ai_btn_summary, {
    updateTextAreaInput(session, "ai_prompt", value = "Give a clear summary of my current week and period. Highlight the most important observations and findings. Based on these, suggest practical adjustments to periodization, goal setting, nutrition, training, or recovery.")
  })
  
  # 2) Weight Change Explainer Button ----
  observeEvent(input$ai_btn_delta, {
    updateTextAreaInput(session, "ai_prompt", value = "Analyze my recent weight changes. Determine whether they were driven by calorie intake adherence, TDEE inaccuracies, activity/NEAT changes, or physiological adaptation. Provide actionable guidance to adjust my plan going forward.")
  })
  
  # 3) Trend Analyzer Button ----
  observeEvent(input$ai_btn_longterm, {
    updateTextAreaInput(session, "ai_prompt", value = "Based on my overall progress to date, evaluate whether I am on track to reach my long-term ideals as set in my Profile. Highlight gaps or adjustments needed in periodization, goal setting, nutrition, training, or recovery to stay aligned with these goals")
  })
  
  # 4) Progress Rate Guidance ----
  observeEvent(input$ai_btn_progressrate, {
    updateTextAreaInput(session, "ai_prompt", value = "Based on my current FFMI, body fat %, weight, and waist trends, assess whether my rate of progress is reasonable. Suggest actionable adjustments to periodization, goal setting, nutrition, training, or recovery to maximize lean mass while minimizing waist and fat gain.")
  })
  
  # 5) Signal Assessment ----
  observeEvent(input$ai_btn_signal, {
    updateTextAreaInput(session, "ai_prompt", value = "Evaluate my trends in FFMI, body fat %, waist, weight, training intensity, and recovery. Identify whether the gain/loss signals are complementary or contradictory. Assess if my calorie and protein intake are sufficient and aligned with my goals.")
  })
  
  # 6) Strength & Recovery Guidance ----
  observeEvent(input$ai_btn_strength_recovery, {
    updateTextAreaInput(session, "ai_prompt", value = "Evaluate my recent strength progress, highlighting both positives and negatives. Assess whether my training frequency, volume, and intensity support optimal recovery, and provide clear guidance on adjustments to periodization, goal setting, nutrition, training, or recovery strategies to continue progressing efficiently.")
  })
  
  # Sending AI prompt
  observeEvent(input$ai_send, {
    req(input$ai_prompt)
    if (rv$ai_loading)
      return()
    if (is.null(rv$final)) {
      showNotification("⚠️ No processed data available. Fetch Notion DBs first.",
                       type = "error")
      return()
    }
    
    rv$ai_loading <- TRUE
    
    # Wrap the AI call in a minimalistic progress
    withProgress(message = "⏳ Sending prompt to AI Coach...", value = 0, {
      # Increment once right away to show something
      incProgress(0.1)
      
      ai_context <- generate_ai_context(rv$final)
      if (is.null(ai_context) || ai_context == "") {
        showNotification("⚠️ AI context is empty. Check processed data.", type =
                           "error")
        rv$ai_loading <- FALSE
        return()
      }
      
      # Generate complete prompt
      full_prompt <- paste0(ai_context,
                            "\n\n<<<USER QUESTION>>>:\n",
                            input$ai_prompt)
      
      # Send prompt to model
      req(input$ai_model_select)  # make sure a model is selected
      res <- call_openrouter_ai(full_prompt, api_key, input$ai_model_select)
      
      # Increment again for effect (optional)
      incProgress(0.2)
      
      # Safely extract and clean the AI response text
      response_text <- if (!is.null(res$text) &&
                           nzchar(res$text))
        res$text
      else
        "⚠️ AI returned no text."
      response_model <- if (!is.null(res$model))
        res$model
      else
        "unknown"
      
      # Save cleaned response to chat history
      rv$ai_history <- append(rv$ai_history, list(
        list(
          prompt = input$ai_prompt,
          response = response_text,
          model = res$model
        )
      ))
      
      rv$ai_latest <- response_text
      
      session$sendCustomMessage("scroll_ai_chat", list())
      
      # Final increment to complete progress
      incProgress(0.7)
    })
    
    rv$ai_loading <- FALSE
    update_progress("")
  })
  
  
  # --------------------------
  # EXPORT: PDF Report + Raw DAta
  # --------------------------
  output$rawdata_ui <- renderUI({
    req(rv$final, rv$db_list)
    
    raw_names <- c(names(rv$final$Raw_Data), paste0("Notion_", names(rv$db_list)))
    
    tagList(
      # ===== TOP SECTION: Full App PDF Export (Slim, subtle grey-blue fill, top-right box) =====
      div(
        style = "text-align: right; margin-bottom: 16px;",
        div(
          style = "display: inline-block;
               background-color: #EFF3F6;       /* subtle grey-blue fill */
               border: 1px solid #D6EAF8;
               border-radius: 8px;
               padding: 10px 14px;
               width: 200px;                     /* slim & sleek */
               box-shadow: 0 2px 6px rgba(0,0,0,0.08);
               text-align: center;",
          h4(style = "font-size:14px; font-weight:600; color:#1B4F72; margin-bottom:8px;", "📉 Full User Report"),
          downloadButton(
            "export_pdf_btn",
            " Download Report",
            class = "btn btn-primary tile-btn",
            style = "width:auto; padding:6px 12px; font-size:13px;
                              background-color:#1B4F72; border-color:#154360; color:#FFFFFF;
                              box-shadow: 0 2px 4px rgba(0,0,0,0.2);"
          )
        )
      )
      
      ,
      
      # ===== BOTTOM SECTION: Current Datatable UI =====
      div(
        class = "datatable-section",
        selectInput("raw_select", "📂 Select Dataset", choices = raw_names),
        DT::dataTableOutput("selected_raw_table")
      )
    )
  })
  
  #data tables
  output$selected_raw_table <- DT::renderDataTable({
    req(input$raw_select)
    
    # choose from final processed or raw DBs
    data_to_show <- if (input$raw_select %in% names(rv$final$Raw_Data)) {
      rv$final$Raw_Data[[input$raw_select]]
    } else {
      # remove "Notion_" prefix to access the list element
      db_name <- sub("^Notion_", "", input$raw_select)
      rv$db_list[[db_name]]$tibble
    }
    
    # Identify numeric columns for formatting
    num_cols <- names(data_to_show)[sapply(data_to_show, is.numeric)]
    
    # Build table
    dt_obj <- DT::datatable(
      data_to_show,
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', filename = input$raw_select),
          list(extend = 'csv', filename = input$raw_select),
          list(extend = 'excel', filename = input$raw_select)
        ),
        columnDefs = list(list(
          className = 'dt-nowrap', targets = "_all"
        ))
      )
    )
    
    # Apply uniform 2-decimal formatting to numeric columns
    if (nrow(data_to_show) > 0 && length(num_cols) > 0) {
      dt_obj <- dt_obj %>% DT::formatRound(columns = num_cols, digits = 2)
    }
    
    dt_obj
    
  })
  
  ###---due to issues with shiny timeout, this block has been replaced with an HTML output (see following block)
  # #pdf downloader
  # output$export_pdf_btn <- downloadHandler(
  #   filename = function() {
  #     paste0(input$identity_select,
  #            "_FitMetrics_Report_",
  #            Sys.Date(),
  #            ".pdf")
  #   },
  #   content = function(file) {
  #     req(rv$final, input$identity_select)
  #
  #     withProgress(message = "📄 Generating your PDF report...", value = 0, {
  #       incProgress(0.1, detail = "Preparing data...")
  #
  #       # Step 1: Render HTML quietly
  #       temp_html <- tempfile(fileext = ".html")
  #       capture.output(suppressMessages(suppressWarnings(
  #         rmarkdown::render(
  #           input = "helpers/report.Rmd",
  #           output_file = temp_html,
  #           params = list(
  #             final = rv$final,
  #             ai_history = rv$ai_history,
  #             identity = input$identity_select
  #           ),
  #           quiet = TRUE,
  #           envir = new.env(parent = globalenv())
  #         )
  #       )), file = NULL)
  #
  #       incProgress(0.6, detail = "Converting to PDF...")
  #
  #       # Step 2: Convert HTML → PDF silently
  #       capture.output(suppressMessages(suppressWarnings(
  #         pagedown::chrome_print(input = temp_html, output = file)
  #       )), file = NULL)
  #
  #       incProgress(0.3, detail = "Finalizing...")
  #     })
  #
  #     update_progress("✅ PDF REPORT GENERATED")
  #   }
  # )
  
  # HTML Downloader (safe PDF alternative)
  output$export_pdf_btn <- downloadHandler(
    filename = function() {
      paste0(
        input$identity_select,
        "_FitMetrics_Report_",
        format(Sys.time(), "%Y-%m-%d %H:%M"),
        ".html"
      )
    },
    content = function(file) {
      req(rv$final, input$identity_select)
      
      withProgress(message = "📄 Generating your report...", value = 0, {
        incProgress(0.1, detail = "Preparing data...")
        
        # Step 1: Render Rmd → HTML
        temp_html <- tempfile(fileext = ".html")
        
        capture.output(suppressMessages(suppressWarnings(
          rmarkdown::render(
            input = "helpers/report.Rmd",
            output_file = temp_html,
            params = list(
              final       = rv$final,
              ai_history  = rv$ai_history,
              identity    = input$identity_select
            ),
            quiet = TRUE,
            envir = new.env(parent = globalenv())
          )
        )), file = NULL)
        
        incProgress(0.7, detail = "Finalizing...")
        
        # Step 2: Move rendered HTML to output location
        file.copy(temp_html, file, overwrite = TRUE)
      })
      
      update_progress("✅ HTML Report Generated")
    }
  )
  
}

# ────────────────────────────────
# SHINY APP
# ────────────────────────────────
shinyApp(ui, server)
