# Confirmatory Factor Analysis Optimization with AI Assistance

Automatically refines a user-specified Confirmatory Factor Analysis
(CFA) model by iteratively evaluating fit and modification indices. At
each step, the function:

1\. Fits the CFA model and computes the scaled RMSEA. 2. If RMSEA \<=
`rmsea_threshold`, stops. 3. Otherwise, identifies all modification
indices (MI) \> `mi_threshold`. 4. Selects the largest MI and compares
the standardized loadings for the two involved items; removes the item
with the smaller loading. 5. Records the modification, RMSEA and MI,
updates the model syntax by dropping the removed item, and repeats until
RMSEA is acceptable, no MI exceed threshold, or `max_steps` is reached.

Optionally, when `analyze_removed = TRUE`, it uses the OpenAI API to
generate concise justifications for exclusion or retention of each item,
based on provided `item_definitions` and `factor_definitions`.

## Usage

``` r
optimal_cfa_with_ai(
  initial_model,
  data,
  rmsea_threshold      = 0.08,
  mi_threshold         = 3.84,
  max_steps            = 10,
  verbose              = TRUE,
  debug                = FALSE,
  filter_expr          = NULL,
  exclude_items        = character(0),
  analyze_removed      = FALSE,
  api_key              = NULL,
  item_definitions     = NULL,
  domain_name          = "Dominio por Defecto",
  scale_title          = "Título de la Escala por Defecto",
  construct_definition = "",
  model_name           = "Modelo CFA",
  gpt_model            = "gpt-3.5-turbo",
  factor_definitions   = NULL,
  ...
)
```

## Arguments

- initial_model:

  A character string with the lavaan-style CFA model syntax.

- data:

  A `data.frame` containing the observed variables.

- rmsea_threshold:

  Numeric. Maximum acceptable scaled RMSEA to stop refinement.

- mi_threshold:

  Numeric. Minimum modification index value to consider.

- max_steps:

  Integer. Maximum number of refinement iterations.

- verbose:

  Logical. If `TRUE`, prints progress messages.

- debug:

  Logical. If `TRUE`, prints additional debugging information.

- filter_expr:

  An expression to subset `data` before fitting (optional).

- exclude_items:

  Character vector of items to never remove.

- analyze_removed:

  Logical. If `TRUE`, performs AI-driven conceptual analysis.

- api_key:

  String. OpenAI API key (required if `analyze_removed = TRUE`).

- item_definitions:

  Named list mapping each item to its textual content for AI prompts.

- domain_name:

  Character. Domain or factor label used in AI prompts.

- scale_title:

  Character. Scale title used in AI prompts.

- construct_definition:

  Character. Brief construct definition for AI context.

- model_name:

  Character. Label for the CFA model in AI prompts.

- gpt_model:

  Character. Name of the ChatGPT model (e.g., `"gpt-3.5-turbo"`).

- factor_definitions:

  Named list of factor descriptions, indexed by factor name.

- ...:

  Additional arguments passed to
  [`cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html).

## Details

The function proceeds through these phases:

1.  **Initialization:** Parses the initial model syntax into factor and
    item lists.

2.  **Filtering:** Optionally subsets `data` via `filter_expr`.

3.  **Iterative refinement (up to `max_steps`):**

    1.  Fit the current CFA model.

    2.  Compute scaled RMSEA; if \<= `rmsea_threshold`, stop.

    3.  Extract all MI \> `mi_threshold`; if none, stop.

    4.  Select the single largest MI, compare the two involved items’
        standardized loadings, and remove the item with the lower
        loading.

    5.  Update the model syntax by dropping that item and log the step.

4.  **Final fit:** Stores the last fitted `lavaan` object and final
    RMSEA/CFI.

5.  **Conceptual analysis (optional):** For each removed and retained
    item, calls the OpenAI API to generate justification text.

## Value

A list with elements:

- final_model:

  Character. The refined CFA model syntax.

- final_fit:

  A `lavaan` object of the last fitted model.

- log:

  Data frame recording each refinement step (`step`, `modification`,
  `mi_value`, `rmsea`).

- removed_items:

  Character vector of items removed.

- alternative_rmsea:

  Numeric. Final scaled RMSEA.

- final_cfi:

  Numeric. Final scaled CFI.

- conceptual_analysis:

  List with sublists `removed` and `kept` containing AI-generated texts,
  or `NULL` if not performed.

## Author

Dr. José Ventura-León

## Examples

``` r
data(Data_Personality)
model <- '
F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ4 + PPTQ5
F2 =~ PPTQ6 + PPTQ7 + PPTQ8 + PPTQ9 + PPTQ10
F3 =~ PPTQ11 + PPTQ12 + PPTQ13 + PPTQ14 + PPTQ15
'
# Run the optimization without AI
res_cfa <- optimal_cfa_with_ai(
  initial_model   = model,
  data            = Data_Personality,
  rmsea_threshold = 0.08,
  max_steps       = 5
)
#> Paso 1 
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Eliminando F1 (carga 0 < 0.366). 
#> Paso 2 
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Eliminando F1 (carga 0 < 0.366). 
#> Paso 3 
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Eliminando F1 (carga 0 < 0.366). 
#> Paso 4 
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Eliminando F1 (carga 0 < 0.366). 
#> Paso 5 
#> Warning: lavaan->lavaan():  
#>    the first indicator of the following latent variable(s) is a poor item; 
#>    switching to another marker item (to set the metric) to avoid convergence 
#>    problems; use bad.marker.crit = 0 to switch off this behavior: F2 (PPTQ6 
#>    -> PPTQ10)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F2
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F1 F3
#> Warning: lavaan->lav_start_check_cov():  
#>    starting values imply a correlation larger than 1; variables involved are: 
#>    F2 F3
#> Eliminando F1 (carga 0 < 0.366). 
res_cfa$removed_items
#> [1] "F1" "F1" "F1" "F1" "F1"
res_cfa$log
#>   step modification mi_value      rmsea
#> 1    1 F1 =~ PPTQ15  15.5407 0.08866544
#> 2    2 F1 =~ PPTQ15  15.5407 0.08866544
#> 3    3 F1 =~ PPTQ15  15.5407 0.08866544
#> 4    4 F1 =~ PPTQ15  15.5407 0.08866544
#> 5    5 F1 =~ PPTQ15  15.5407 0.08866544
cat(res_cfa$final_model, "\n")
#> F1 =~ PPTQ1 + PPTQ2 + PPTQ3 + PPTQ4 + PPTQ5
#> F2 =~ PPTQ6 + PPTQ7 + PPTQ8 + PPTQ9 + PPTQ10
#> F3 =~ PPTQ11 + PPTQ12 + PPTQ13 + PPTQ14 + PPTQ15 

if (FALSE) { # \dontrun{
# Run with AI-driven analysis (requires an OpenAI-compatible API key)
res_cfa_ai <- optimal_cfa_with_ai(
  initial_model      = model,
  data               = Data_Personality,
  analyze_removed    = TRUE,
  api_key            = Sys.getenv("OPENAI_API_KEY"),
  item_definitions   = as.list(stats::setNames(
    paste("Item", 1:15, "content"), paste0("PPTQ", 1:15))),
  factor_definitions = list(F1 = "Factor 1 description",
                            F2 = "Factor 2 description",
                            F3 = "Factor 3 description")
)
res_cfa_ai$conceptual_analysis
} # }
```
