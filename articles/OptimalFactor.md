# Introduction to OptimalFactor

## Overview

**OptimalFactor** is a comprehensive R package for optimal factor model
refinement in both Exploratory Factor Analysis (EFA) and Confirmatory
Factor Analysis (CFA) using machine-learning inspired boosting
algorithms.

## Installation

You can install the latest version of OptimalFactor from GitHub:

``` r

if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventural/OptimalFactor")
```

## Main Features

### EFA-Boosting Algorithm

The
[`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
function implements an advanced iterative optimization algorithm for
EFA:

- **Greedy and global search strategies** for optimal item selection
- **Adaptive composite fit indices** (RMSEA, SRMR, CFI) with weights
  based on df x N
- **Automatic Heywood case** and near-Heywood detection
- **Cross-loading identification** and removal
- **Minimum items per factor** enforcement
- **Interfactor correlation verification**
- **Optional AI-assisted conceptual analysis** via GPT models

### CFA Optimization

The
[`cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md)
function provides CFA optimization:

- Modification indices-based model improvement
- Automatic covariance addition
- Fit index monitoring

## Quick Start

### Basic EFA-Boosting

``` r

library(OptimalFactor)

# Run EFA-Boosting
result <- efa_boosting(
  data = your_data,
  name_items = "item",
  n_factors = 3,
  verbose = TRUE
)

# View results
print(result$final_structure)
print(result$fit_indices)
```

## Theoretical Background

The EFA-Boosting algorithm uses adaptive fit indices based on the
research by Kenny, McCoach (2003) and Shi, Lee, & Maydeu-Olivares
(2019), which demonstrates that traditional cutoff values for fit
indices depend on model complexity (degrees of freedom) and sample size.

The adaptive weights are calculated as:

- **RMSEA weight**: Higher for larger models (more df)
- **SRMR weight**: Stable across model sizes
- **CFI weight**: Higher for smaller models (fewer df)

This adaptive approach ensures appropriate model evaluation across
different scales and sample sizes.

## References

- Kenny, D. A., & McCoach, D. B. (2003). Effect of the number of
  variables on measures of fit in structural equation modeling.
  *Structural Equation Modeling, 10*(3), 333-351.

- Shi, D., Lee, T., & Maydeu-Olivares, A. (2019). Understanding the
  model size effect on SEM fit indices. *Educational and Psychological
  Measurement, 79*(2), 310-334.
