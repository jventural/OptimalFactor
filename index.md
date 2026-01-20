# OptimalFactor

![OptimalFactor](https://github.com/jventural/OptimalFactor/blob/master/logo_optimalfactor.png)

A comprehensive package for optimal factor model refinement in both EFA
and CFA using machine-learning inspired boosting algorithms.  
[**Visit the author’s website**](https://joseventuraleon.com/)  
  

![CRAN
version](https://www.r-pkg.org/badges/version/OptimalFactor)[![Documentation](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://jventural.github.io/OptimalFactor/)

## Features

- **EFA-Boosting Algorithm**: Advanced iterative optimization for
  Exploratory Factor Analysis
- **Adaptive Fit Indices**: Dynamic weights based on df x N following
  Kenny, Shi & Savalei (2022)
- **Automatic Problem Detection**: Heywood cases, cross-loadings, and
  low loadings
- **Global Search**: Multi-item removal optimization
- **AI Integration**: Optional GPT-powered conceptual analysis of
  removed items
- **Interactive Shiny App**: EFA-Boosting Studio for user-friendly EFA
  optimization

## Installation

You can install the latest version of OptimalFactor from GitHub:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventural/OptimalFactor")
```

## Quick Start

### EFA-Boosting

``` r
library(OptimalFactor)

result <- efa_boosting(
  data = your_data,
  name_items = "item",
  n_factors = 3,
  verbose = TRUE
)
```

### EFA-Boosting Studio (Shiny App)

``` r
library(OptimalFactor)
run_efa_boosting()
```

## Main Functions

| Function                                                                                                          | Description                                  |
|-------------------------------------------------------------------------------------------------------------------|----------------------------------------------|
| [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)                           | EFA optimization with adaptive composite fit |
| [`run_efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/run_efa_boosting.md)                   | Launch EFA-Boosting Studio (Shiny app)       |
| [`cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md)                           | CFA optimization with modification indices   |
| [`print_conceptual_analysis()`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md) | Display AI-generated item analyses           |

## Examples

[Basic tutorial of the R OptimalFactor
library](https://rpubs.com/jventural/OptimalFactor)

## License

GPL-3

## Citation

Ventura-Leon, J. (2026). *OptimalFactor: Optimal Factor Analysis with
EFA-Boosting Algorithm* \[R package\]. GitHub.
<https://github.com/jventural/OptimalFactor>

## Author

Jose Ventura-Leon <jventuraleon@gmail.com>
