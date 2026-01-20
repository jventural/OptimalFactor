<p align="center"> <img src="https://github.com/jventural/OptimalFactor/blob/master/logo_optimalfactor.png" alt="OptimalFactor" width="200" height="200"/> </p> <h1 align="center">OptimalFactor</h1> <p align="center">
  A comprehensive package for optimal factor model refinement in both EFA and CFA using machine-learning inspired boosting algorithms. <br /> <a href="https://joseventuraleon.com/"><strong>Visit the author's website</strong></a> <br /><br /> </p> <p align="center"> <img src="https://www.r-pkg.org/badges/version/OptimalFactor" alt="CRAN version"/> </p>

## Features

- **EFA-Boosting Algorithm**: Advanced iterative optimization for Exploratory Factor Analysis
- **Adaptive Fit Indices**: Dynamic weights based on df x N following Kenny, Shi & Savalei (2022)
- **Automatic Problem Detection**: Heywood cases, cross-loadings, and low loadings
- **Global Search**: Multi-item removal optimization
- **AI Integration**: Optional GPT-powered conceptual analysis of removed items
- **Interactive Shiny App**: User-friendly interface for EFA optimization

## Installation

You can install the latest version of OptimalFactor from GitHub:

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventural/OptimalFactor")
```

## Quick Start

### EFA-Boosting

```r
library(OptimalFactor)

result <- efa_boosting(
  data = your_data,
  name_items = "item",
  n_factors = 3,
  verbose = TRUE
)
```

### Shiny Application

```r
# Launch the interactive EFA Optimizer
run_efa_optimizer()
```

## Main Functions

| Function | Description |
|----------|-------------|
| `efa_boosting()` | EFA optimization with adaptive composite fit |
| `efa_optimizer()` | Classic EFA optimization with RMSEA target |
| `cfa_boosting()` | CFA optimization with modification indices |
| `run_efa_optimizer()` | Launch interactive Shiny application |
| `print_conceptual_analysis()` | Display AI-generated item analyses |

## Examples

[Basic tutorial of the R OptimalFactor library](https://rpubs.com/jventural/OptimalFactor)

## License

GPL-3

## Citation

Ventura-Leon, J. (2026). _OptimalFactor: Optimal Factor Analysis with EFA-Boosting Algorithm_ [R package]. GitHub. https://github.com/jventural/OptimalFactor

## Author

Jose Ventura-Leon <jventuraleon@gmail.com>
