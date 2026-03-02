# FitMetrics

A comprehensive fitness analytics dashboard built with R Shiny that processes training data from Notion databases and provides AI-powered coaching insights.

## 🎯 Features

### Data Processing
- **Multi-DB Integration**: Connects to multiple Notion databases (Training Weekly Log, Training Periodization, Training Profiles, Training Parameters, Training Programs)
- **Data Standardization**: Cleans and standardizes data from various sources into unified tibbles
- **Relation Resolution**: Automatically resolves Notion relations and rollups to human-readable titles
- **Historical Analysis**: Tracks progress over time with 3-week rolling averages for accurate trend analysis

### Analytics Dashboard
- **Current Week Overview**: Real-time stats on weight, body composition, nutrition, training, and lifestyle
- **Profile-Level Insights**: Multi-period analysis of fitness trends and progress
- **Compliance Tracking**: Monitors adherence to nutrition, training, sleep, and step goals
- **FFMI Classification**: Advanced body composition analysis using Fat-Free Mass Index
- **Velocity Classification**: Classifies weight change velocity (cut, bulk, maintenance) with precise %/week metrics

### AI Coach
- **Interactive Chat**: Real-time AI coaching with context-aware responses
- **Pre-built Templates**: Quick-access buttons for common analysis scenarios (summary, weight change, long-term trends, progress rate, signal assessment, strength recovery)
- **Context Generation**: Automatically generates comprehensive context from processed data
- **Model Selection**: Supports multiple AI models via OpenRouter API

### Visualization
- **Interactive Charts**: Plotly-powered charts with range sliders and hover details
- **Progress Tracking**: Visual representation of weight, body composition, and strength trends
- **Data Tables**: Exportable tables with filtering and sorting capabilities

### Export Features
- **PDF Reports**: Professional PDF reports with comprehensive analysis
- **Data Export**: Export raw data to CSV/Excel formats
- **HTML Reports**: Browser-compatible reports for easy sharing

## 🛠️ Tech Stack

- **Frontend**: R Shiny with custom CSS styling
- **Backend**: R with tidyverse (dplyr, tidyr, purrr)
- **Data Processing**: Notion API integration, zoo for rolling calculations
- **Visualization**: ggplot2, plotly
- **AI Integration**: OpenRouter API for language models
- **Data Export**: pagedown, rmarkdown

### 📦 Dependencies
The application uses multiple R packages for functionality, including:
- **Core**: shiny, dplyr, ggplot2, plotly, notionapi
- **Data Processing**: tidyr, purrr, zoo, lubridate, stringr
- **API/Integration**: httr, jsonlite, htmltools
- **Reporting**: rmarkdown, pagedown, commonmark

All dependencies are managed through the automated `install_dependencies.R` script.

## 📁 Project Structure

```
FitMetrics/
├── app.R                    # Main Shiny application
├── install_dependencies.R  # Automated dependency installation script
├── .Renviron               # Environment variables (API keys)
├── README.md              # This file
├── .gitignore             # Git ignore rules
├── LICENSE                # MIT License
├── helpers/               # Helper scripts
│   ├── notion_helpers.R   # Notion API utilities
│   ├── core_processing.R # Data processing logic
│   ├── ai_helpers.R      # AI integration utilities
│   ├── report.Rmd         # PDF report template
│   └── report.css        # Report styling
└── assets/               # Static assets (images, CSS)
```

## 🚀 Getting Started

### Prerequisites
- R ≥ 4.0.0
- RStudio (recommended)
- Notion integration token
- OpenRouter API key
- Internet connection for package installation

### Quick Start

1. **Clone the repository:**
   ```bash
   git clone https://github.com/bahaeomid/Fitmetrics.git
   cd Fitmetrics
   ```

2. **Install dependencies:**
   ```R
   # Run the automated installation script (recommended)
   source("install_dependencies.R")
   ```
   
   This script will automatically install all required packages, verify installations, and provide helpful feedback. It includes progress tracking and error handling for smooth setup.

2. **Install R packages:**
   ```R
   install.packages(c(
     "shiny", "notionapi", "dplyr", "tidyr", "zoo", "purrr",
     "lubridate", "stringr", "ggplot2", "gridExtra", "broom",
     "DT", "plotly", "httr", "jsonlite", "commonmark", "markdown",
     "pagedown", "rmarkdown", "glue", "htmltools"
   ))
   ```

3. **Set up API keys:**
   Create a `.Renviron` file in the project root:
   ```
   NOTION_TOKEN=your_notion_integration_token
   OPENROUTER_API_KEY=your_openrouter_api_key
   ```

### Quick Start

1. **Run the application:**
   ```R
   shiny::runApp()
   ```

2. **Configure Notion URLs:**
   Update the URLs in `app.R:188-194` with your Notion database URLs

3. **Fetch Data:**
   Click the "🔄 Fetch Notion DBs" button to load your data

4. **Select Identity:**
   Choose your profile from the dropdown menu

5. **Analyze:**
   Use the dashboard tabs to explore insights, charts, and AI coaching

## 🎨 UI Preview

### Main Dashboard
- **Sidebar**: Navigation with identity selector and progress indicators
- **Header**: Collapsible overview cards with key metrics
- **Tabs**: Organized sections for insights, stats, charts, AI coach, and export

### AI Coach Interface
- **Prompt Area**: Text input for custom questions
- **Quick Actions**: Pre-built analysis templates
- **Model Selector**: Choose from available AI models
- **Chat History**: Conversation log with formatted responses

### Data Visualization
- **Interactive Charts**: Plotly charts with zoom, pan, and hover details
- **Responsive Tables**: DataTables with export functionality
- **Progress Indicators**: Visual representations of trends and compliance

## ⚙️ Customization

### Notion Configuration
- **Database URLs**: Update the `urls` list in `app.R` with your Notion database links
- **Column Mapping**: Modify `notion_page_to_row` in `notion_helpers.R` for custom column names
- **Data Cleaning**: Adjust `clean_notion_tibble` for specific data formats

### AI Configuration
- **Model Selection**: Change default model in `ai_helpers.R`
- **Context Generation**: Modify `generate_ai_context` for different data emphasis
- **Prompt Templates**: Customize AI prompt templates in `ai_helpers.R`

### Styling
- **CSS Customization**: Modify the inline styles in `app.R` or add external CSS
- **Theme Options**: Adjust color schemes and typography in the style section

### Analytics
- **Metrics Calculation**: Update formulas in `core_processing.R`
- **Classification Thresholds**: Modify FFMI and velocity classification thresholds
- **Rolling Window**: Adjust rolling average windows for trend analysis

## 🔧 Configuration

### Environment Variables
Create a `.Renviron` file:
```
NOTION_TOKEN=your_notion_integration_token
OPENROUTER_API_KEY=your_openrouter_api_key
```

### Notion Setup
1. **Create Notion Integration**: Go to notion.so/my-integrations
2. **Get Token**: Copy the internal integration token
3. **Share Databases**: Share your Notion databases with the integration

### OpenRouter Setup
1. **Create Account**: Sign up at openrouter.ai
2. **Get API Key**: Copy your API key from the dashboard
3. **Model Selection**: Choose from available free/paid models

## 📚 API Endpoints

### Notion API
- **Base URL**: `https://api.notion.com/v1/`
- **Databases**: Query and retrieve database contents
- **Pages**: Access individual page data
- **Relations**: Resolve linked database relationships

### OpenRouter API
- **Base URL**: `https://openrouter.ai/api/v1/`
- **Models**: List available AI models
- **Completions**: Generate text completions
- **Pricing**: Token-based pricing with free tiers available

## 📄 Data Flow

1. **Data Fetching**: Notion databases → R tibbles
2. **Processing**: Clean, standardize, and join data
3. **Analysis**: Calculate metrics, classifications, and trends
4. **Visualization**: Generate charts and tables
5. **AI Integration**: Generate context and responses
6. **Export**: Create reports and data exports

## 🤝 Credit

- **Notion API**: For database integration capabilities
- **OpenRouter**: For affordable AI model access
- **Tidyverse**: For data manipulation and analysis
- **Plotly**: For interactive visualizations
- **Shiny**: For reactive web application framework

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- **Fitness Community**: For inspiration and validation of fitness analytics approaches
- **R Community**: For the comprehensive ecosystem of data science tools
- **Open Source Contributors**: For the libraries and frameworks that make this possible

## 🐛 Troubleshooting

### Common Issues

1. **API Key Not Set**
   - Error: "API keys not set! Check .Renviron"
   - Solution: Verify `.Renviron` file exists and contains valid keys

2. **Notion Connection Failed**
   - Error: "Error fetching Notion DBs"
   - Solution: Check Notion token, database sharing settings, and URLs

3. **AI Model Not Available**
   - Error: "AI request failed"
   - Solution: Verify OpenRouter API key, check model availability, try different model

4. **Data Not Loading**
   - Error: "No data found for identity"
   - Solution: Verify identity exists in Training Profiles database, check data completeness

### Performance Tips

- **Data Size**: Large databases may slow loading times
- **Rolling Calculations**: Adjust window sizes for performance vs. accuracy
- **Caching**: Consider caching processed data for repeated analysis
- **Model Selection**: Use faster models for quicker responses

## 📊 Support

For issues and questions:
1. Check the troubleshooting section above
2. Verify all configuration files are correctly set up
3. Ensure all required R packages are installed
4. Test API connections individually

## 📝 Changelog

- **Initial Release**: Complete fitness analytics dashboard with AI coaching
- **Features Added**: Multi-DB integration, FFMI classification, velocity analysis, PDF export
- **Improvements**: Enhanced UI styling, interactive charts, context-aware AI responses

---

**FitMetrics** - Your personal fitness analytics and AI coaching companion. 🚀🎓