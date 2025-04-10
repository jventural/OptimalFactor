<p align="center"> <img src="https://github.com/jventural/OptimalFactor/blob/master/logo_optimalfactor.png" alt="OptimalFactor" width="200" height="200"/> </p> <h1 align="center">OptimalFactor</h1> <p align="center">
  A comprehensive package for optimal factor model refinement in both EFA and CFA. <br /> <a href="https://joseventuraleon.com/"><strong>Visit the author's website »</strong></a> <br /><br /> </p> <p align="center"> <img src="https://www.r-pkg.org/badges/version/OptimalFactor" alt="CRAN version"/> </p>

## Installation
You can install the latest version of OptimalFactor from GitHub using the devtools package:

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventuraleon/OptimalFactor")
```
# Examples
EFA Model Refinement with OptimalFactor
Refine an Exploratory Factor Analysis model by iteratively removing problematic items based on RMSEA improvement and loading thresholds. For example:
```r
result <- stepwise_efa_removal_structure(
  data = your_data,
  n_factors = 3,
  name_items = "PPTQ",
  threshold_rmsea = 0.08,
  threshold_loading = 0.30,
  min_items_per_factor = 2,
  verbose = TRUE
)
print(result$steps_log)
cat("Final RMSEA:", result$final_rmsea, "\n")
cat("Removed Items:", paste(result$removed_items, collapse = ", "), "\n")
```

## CFA Model Improvement with OptimalFactor
Improve a Confirmatory Factor Analysis model by sequentially incorporating modification indices and removing items with low loadings. For instance:
```r
result_cfa <- stepwise_cfa_improvement(
  initial_model = model1,
  data = your_data,
  rmsea_threshold = 0.08,
  mi_threshold = 3.84,
  max_steps = 10,
  verbose = TRUE,
  debug = FALSE
)
print(result_cfa$log)
```
# License
GPL-3

# Citation
Ventura-León, J. (2024). _OptimalFactor_ [Software]. GitHub. https://github.com/jventuraleon/OptimalFactor

# Author
José Ventura-León jventuraleon@gmail.com

Packaged: 2025-04-10 10:50:11 UTC; José Ventura-León
