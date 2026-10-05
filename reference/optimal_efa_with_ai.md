# Exploratory Factor Analysis Optimization with AI Assistance

Automatically refines an Exploratory Factor Analysis (EFA) solution by
combining model-fit criteria (scaled RMSEA) and item-loading quality,
and-when requested- integrates AI-generated conceptual analyses and
factor naming. At each iteration it:

1.  Estimates the specified EFA model on the current set of items.

2.  Computes the scaled RMSEA and checks whether it is \<=
    `threshold_rmsea` and every factor retains at least
    `min_items_per_factor` items.

3.  If fit or structure criteria are not met, identifies and removes the
    item whose exclusion yields the greatest RMSEA improvement or that
    exhibits structural issues (cross-loading or no loading).

4.  Repeats steps 1-3 until both fit and structure criteria are
    satisfied or `max_steps` iterations are reached.

Optionally, when `analyze_removed = TRUE` and `item_definitions` are
provided, it calls the OpenAI API to generate concise justifications for
both exclusion and retention of each item, using the provided
definitions and the final loading structure as context. If
`generate_factor_names = TRUE`, it also prompts the AI to propose brief
(1-2 word) tentative names for each factor, taking into account the
specified `domain_name`, `scale_title`, and `construct_definition`.

## Usage

``` r
optimal_efa_with_ai(
  data,
  items                    = NULL,
  n_factors                = 5,
  n_items                  = NULL,
  name_items               = "PPTQ",
  estimator                = "WLSMV",
  rotation                 = "oblimin",
  threshold_rmsea          = 0.08,
  threshold_loading        = 0.30,
  min_items_per_factor     = 2,
  apply_threshold          = TRUE,
  max_steps                = NULL,
  verbose                  = TRUE,
  exclude_items            = character(0),
  analyze_removed          = FALSE,
  api_key                  = NULL,
  item_definitions         = NULL,
  domain_name              = "Dominio por Defecto",
  scale_title              = "Título de la Escala por Defecto",
  construct_definition     = "",
  model_name               = "Modelo EFA",
  gpt_model                = "gpt-3.5-turbo",
  generate_factor_names    = FALSE,
  ...
)
```

## Arguments

- data:

  A `data.frame` containing the observed variables for the EFA.

- items:

  Character vector of item names to include; if `NULL`, names are
  generated using `name_items` and `n_items`.

- n_factors:

  Integer. Number of factors to extract (default `5`).

- n_items:

  Integer. Number of items per factor when `items` is `NULL`.

- name_items:

  Character. Prefix for item names (default `"PPTQ"`).

- estimator:

  Character. Estimator to use (e.g., `"WLSMV"`).

- rotation:

  Character. Rotation method (e.g., `"oblimin"`).

- threshold_rmsea:

  Numeric. Maximum allowable scaled RMSEA to stop refinement (default
  `0.08`).

- threshold_loading:

  Numeric. Minimum absolute loading to consider an item well-loaded
  (default `0.30`).

- min_items_per_factor:

  Integer. Minimum items required per factor (default `2`).

- apply_threshold:

  Logical. If `TRUE`, zeros out loadings below `threshold_loading` in
  the final solution.

- max_steps:

  Integer or `NULL`. Maximum number of iterations; if `NULL`, set to
  `length(items) - 1`.

- verbose:

  Logical. If `TRUE`, prints progress and removal decisions.

- exclude_items:

  Character vector of items to exclude from the start.

- analyze_removed:

  Logical. If `TRUE`, performs conceptual analysis for each removed and
  conserved item via the OpenAI API.

- api_key:

  String. OpenAI API key (required if `analyze_removed = TRUE` or
  `generate_factor_names = TRUE`).

- item_definitions:

  Named list mapping each item to its text definition for AI prompts.

- domain_name:

  Character. Domain or factor context used in AI prompts.

- scale_title:

  Character. Scale title used in AI prompts.

- construct_definition:

  Character. Brief definition of the construct used in AI prompts.

- model_name:

  Character. Label for the EFA model in AI prompts.

- gpt_model:

  Character. Name of the ChatGPT model to use (e.g., `"gpt-3.5-turbo"`).

- generate_factor_names:

  Logical. If `TRUE`, triggers factor naming via AI.

- ...:

  Additional arguments passed to the package's internal EFA engine.

## Details

The function proceeds as follows:

1.  Fits the exploratory models with the package's internal EFA engine.

2.  Determines the initial set of items from `items` or from
    `name_items` and `n_items`.

3.  Enters an iterative loop:

    1.  Estimates the EFA model with the current items.

    2.  Computes the scaled RMSEA.

    3.  Evaluates the factor-loading structure for cross-loadings or
        lack of loadings.

    4.  If RMSEA \<= `threshold_rmsea` and each factor has \>=
        `min_items_per_factor`, stops.

    5.  Otherwise, removes the item whose exclusion most improves RMSEA
        or that has the worst structural issue.

4.  If `analyze_removed = TRUE`, for each excluded and conserved item,
    generates via OpenAI concise justifications based on the final
    structure.

5.  If `generate_factor_names = TRUE` and `api_key` provided, proposes
    tentative names for each factor via AI, considering the specified
    `domain_name`, `scale_title`, and `construct_definition`.

## Value

A list with components:

- final_structure:

  A `data.frame` of final item loadings and factor assignments.

- removed_items:

  Character vector of items removed during refinement.

- steps_log:

  A `data.frame` recording each step: `step`, `removed_item`, `reason`,
  and `rmsea`.

- iterations:

  Integer. Total number of iterations performed.

- final_rmsea:

  Numeric. Final scaled RMSEA after the last iteration.

- bondades_original:

  Original fit indices and other model information from the package's
  internal EFA engine.

- specifications:

  Model specifications returned by the package's internal EFA engine.

- conceptual_analysis:

  A list with elements `removed` and `kept`, each a named list of
  AI-generated texts per item, or `NULL` if `analyze_removed = FALSE`.

- factor_names:

  Named character vector or list of AI-proposed factor names, or `NULL`
  if `generate_factor_names = FALSE`.

## Examples

``` r
# \donttest{
data(Data_Expectativas)
# Run the optimized EFA without AI
res_efa <- optimal_efa_with_ai(
  data       = Data_Expectativas,
  items      = paste0("EAF", 1:10),
  name_items = "EAF",
  n_factors  = 2,
  verbose    = FALSE
)
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.201524e-16) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -4.163431e-18) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -8.897683e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.666993e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.845765e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.288591e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.891747e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 5.362139e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.055497e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.146405e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.276730e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 1.634545e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -7.082614e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.576738e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.889844e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.162264e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.271269e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -3.696436e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -4.266688e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 2.160986e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -6.877305e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 2.612719e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -8.897683e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.666993e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -2.039337e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 3.648523e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
res_efa$removed_items
#> [1] "EAF1" "EAF7"
res_efa$final_structure
#>   Items        f1        f2
#> 1  EAF6 0.8488669 0.0000000
#> 2  EAF3 0.7337076 0.0000000
#> 3  EAF5 0.6924926 0.0000000
#> 4  EAF9 0.0000000 0.9562023
#> 5  EAF8 0.0000000 0.9091483
#> 6 EAF10 0.0000000 0.8093829
#> 7  EAF4 0.0000000 0.6706350
#> 8  EAF2 0.0000000 0.6649148
# }
if (FALSE) { # \dontrun{
# With AI-assisted conceptual analysis (requires an OpenAI-compatible API key)
construct_def <- paste("Positive consequences of completing a degree, such as",
                       "prestige, income and self-evaluation.")
item_defs <- as.list(stats::setNames(
  paste("Content of item", 1:10), paste0("EAF", 1:10)))
res_efa_ai <- optimal_efa_with_ai(
  data                  = Data_Expectativas,
  items                 = paste0("EAF", 1:10),
  name_items            = "EAF",
  n_factors             = 2,
  analyze_removed       = TRUE,
  api_key               = Sys.getenv("OPENAI_API_KEY"),
  item_definitions      = item_defs,
  construct_definition  = construct_def,
  scale_title           = "EAF Scale",
  model_name            = "EFA Multidimensional",
  generate_factor_names = TRUE,
  domain_name           = "Academic outcome expectations"
)
res_efa_ai$conceptual_analysis
res_efa_ai$factor_names
} # }
```
