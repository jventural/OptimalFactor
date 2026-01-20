# Launch EFA-Boosting Studio - EFA-Boosting Interactive Analyzer

Opens EFA-Boosting Studio, an interactive Shiny web application for
performing Exploratory Factor Analysis (EFA) optimization using the
EFA-Boosting algorithm with real-time console output.

## Usage

``` r
run_efa_boosting(launch.browser = TRUE)
```

## Arguments

- launch.browser:

  Logical. If TRUE (default), opens the app in the default web browser.
  If FALSE, returns the app URL for manual navigation.

## Value

Invisibly returns the Shiny app object. The function is primarily called
for its side effect of launching the interactive application.

## Details

EFA-Boosting Studio provides:

- Modern UI with Bootstrap 5 theme

- Real-time console with auto-scroll (100ms refresh)

- Full EFA-Boosting algorithm with adaptive fit indices

- Global search option for multi-item removal

- Interactive results visualization

- CSV/Excel data import

- Export capabilities for final structure and logs

- Optional AI-powered item analysis via GPT

## Quick Start

    library(OptimalFactor)
    run_efa_boosting()

## Required Packages

The application requires these packages (installed automatically if
missing):

- shiny, bslib (UI framework)

- DT (interactive tables)

- readxl, readr (data import)

- ggplot2 (visualizations)

- future, promises (async execution)

## See also

[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch EFA-Boosting Studio
library(OptimalFactor)
run_efa_boosting()

# Or simply:
OptimalFactor::run_efa_boosting()
} # }
```
