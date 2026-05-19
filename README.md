# Carbon Flux Interactive Dashboard

An R Shiny dashboard for interactive exploration of multi-site carbon, water, and energy flux measurements. Built for researchers and stakeholders to compare flux dynamics across sites, seasons, and years without writing code.

## Live Demo

🌐 **[View the deployed CABBI SABR dashboard →](https://sabr.shinyapps.io/appSABR/)**

## Features

- **Multi-site comparison** — side-by-side flux plots across sites
- **Dynamic time filtering** — select custom date ranges interactively
- **Seasonal aggregation** — daily, monthly, or seasonal summaries
- **Variable selector** — switch between NEE, LE, H, and meteorological drivers
- **Downloadable plots** — export any figure as PNG or PDF

## Project Structure

```
carbon-flux-dashboard/
├── app.R          # Main Shiny application (UI + Server)
├── R/
│   └── helpers.R  # Reusable plotting and data functions
└── README.md
```

## Run Locally

```r
# Install dependencies
install.packages(c("shiny", "tidyverse", "ggplot2",
                   "lubridate", "plotly", "shinydashboard",
                   "DT", "scales"))

# Launch app
shiny::runApp("app.R")
```

## Screenshots

| Multi-site time series | Seasonal summary |
|---|---|
| *(see deployed version)* | *(see deployed version)* |

## Background

Developed for the [CABBI](https://cabbi.bio/) (Center for Advanced Bioenergy and Bioproducts Innovation) research program at Iowa State University. Supports data exploration across the Iowa SABR and Illinois Energy Farm sites.

## Author

**Rojda Guler Aslan Sungur, Ph.D.**  
Research Scientist III, Iowa State University  
rojdaaslan@gmail.com
