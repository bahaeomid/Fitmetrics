# ────────────────────────────────────────────────
# FitMetrics - Install Dependencies Script
# Purpose: Install all required R packages for the FitMetrics application
# ────────────────────────────────────────────────

# ────────────────────────────────
# INSTALL REQUIRED PACKAGES
# ────────────────────────────────
cat("🚀 Installing FitMetrics dependencies...\n\n")

# Define required packages with their installation commands
packages <- c(
  # Core Shiny and UI packages
  "shiny",
  "DT",        # DataTables for interactive data tables
  "plotly",    # Interactive charts
  "htmltools", # HTML generation utilities
  
  # Data processing and manipulation
  "dplyr",     # Data manipulation
  "tidyr",     # Data tidying
  "purrr",     # Functional programming
  "zoo",       # Rolling functions and time series
  "lubridate", # Date handling
  "stringr",   # String manipulation
  "glue",      # String interpolation
  
  # Notion API integration
  "notionapi", # Notion API client
  
  # Statistical analysis
  "ggplot2",   # Data visualization
  "gridExtra", # Grid layout for plots
  "broom",     # Convert statistical objects to tidy data
  
  # API and data handling
  "httr",      # HTTP requests
  "jsonlite",  # JSON parsing
  
  # Markdown and reporting
  "commonmark", # Markdown processing
  "markdown",   # Markdown conversion
  "pagedown",   # PDF generation from HTML
  "rmarkdown"   # Report generation
)

# ────────────────────────────────
# INSTALL PACKAGES
# ────────────────────────────────
cat("📦 Installing", length(packages), "required packages...\n\n")

# Track installation progress
installed_count <- 0
total_packages <- length(packages)

for (package in packages) {
  cat(paste0("Installing ", package, "... "))
  
  # Try to install the package
  result <- tryCatch({
    install.packages(package, quiet = TRUE)
    cat("✅\n")
    installed_count <- installed_count + 1
  }, error = function(e) {
    cat("❌ Error: ", e$message, "\n")
  })
}

# ────────────────────────────────
# INSTALL OPTIONAL PACKAGES (if needed)
# ────────────────────────────────
cat("\n📦 Installing optional packages for enhanced functionality...\n\n")

optional_packages <- c(
  # Development and debugging
  "devtools",  # For package development
  
  # Advanced plotting
  "ggpubr",    # Enhanced ggplot2 features
  
  # Data import/export
  "readr",     # Fast CSV reading/writing
  "writexl",   # Excel export
  
  # Advanced markdown
  "flextable", # Advanced table formatting
  
  # Additional utilities
  "knitr",     # Report generation
  "Rcpp",      # Performance enhancement
  "magrittr"   # Pipe operator (though usually base R)
)

for (package in optional_packages) {
  cat(paste0("Installing (optional) ", package, "... "))
  
  result <- tryCatch({
    install.packages(package, quiet = TRUE)
    cat("✅\n")
  }, error = function(e) {
    cat("❌ (optional - skipping)\n")
  })
}

# ────────────────────────────────
# INSTALL NOTIONAPI FROM GITHUB (if available)
# ────────────────────────────────
cat("\n🔄 Checking for notionapi package from GitHub...\n")

if (!requireNamespace("devtools", quietly = TRUE)) {
  cat("Installing devtools for GitHub package installation...\n")
  install.packages("devtools", quiet = TRUE)
}

if (packageVersion("notionapi") < "1.0") {
  cat("📦 Installing latest notionapi from GitHub...\n")
  tryCatch({
    devtools::install_github("colinfay/notionapi", quiet = TRUE)
    cat("✅ notionapi updated\n")
  }, error = function(e) {
    cat("❌ Could not update notionapi from GitHub, using CRAN version\n")
  })
}

# ────────────────────────────────
# VERIFY INSTALLATIONS
# ────────────────────────────────
cat("\n🔍 Verifying package installations...\n\n")

# Check core packages
core_packages <- c("shiny", "dplyr", "ggplot2", "notionapi", "plotly")
missing_core <- c()

for (pkg in core_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_core <- c(missing_core, pkg)
    cat("❌", pkg, " - MISSING\n")
  } else {
    cat("✅", pkg, " - Installed (Version:", packageVersion(pkg), ")\n")
  }
}

# ────────────────────────────────
# SUMMARY REPORT
# ────────────────────────────────
cat("\n" %R% paste(rep("═", 50), collapse = ""), "\n")
cat("🎯 FitMetrics Dependencies Installation Summary\n")
cat("═" %R% paste(rep("═", 50), collapse = ""), "\n\n")

cat("✅ Successfully installed:", installed_count, "of", total_packages, "packages\n")

if (length(missing_core) == 0) {
  cat("🎉 All core dependencies are installed!\n")
  cat("🚀 FitMetrics is ready to run!\n\n")
  
  cat("📝 Next steps:\n")
  cat("1. Set up your API keys in .Renviron file\n")
  cat("2. Configure your Notion database URLs\n")
  cat("3. Run the application: shiny::runApp()\n")
  
} else {
  cat("❌ Missing core packages:", paste(missing_core, collapse = ", "), "\n")
  cat("🔧 Please install these packages manually:\n")
  for (pkg in missing_core) {
    cat("   install.packages('", pkg, "')\n", sep = "")
  }
  cat("\n📋 You may need to install additional packages manually.\n")
}

cat("\n" %R% paste(rep("═", 50), collapse = ""), "\n")
cat("🎯 FitMetrics - Fitness Analytics Dashboard\n")
cat("🔗 https://github.com/bahaeomid/Fitmetrics\n")
cat("═" %R% paste(rep("═", 50), collapse = ""), "\n\n")

# ────────────────────────────────
# LAUNCH APPLICATION (OPTIONAL)
# ────────────────────────────────
cat("💡 Would you like to launch FitMetrics now? (y/n): ")
response <- readline()

if (tolower(response) == "y" || tolower(response) == "yes") {
  cat("\n🚀 Launching FitMetrics...\n")
  tryCatch({
    shiny::runApp()
  }, error = function(e) {
    cat("❌ Error launching app:", e$message, "\n")
    cat("💡 Make sure you're in the FitMetrics directory\n")
  })
} else {
  cat("✅ Installation complete! Run 'shiny::runApp()' when ready.\n")
}