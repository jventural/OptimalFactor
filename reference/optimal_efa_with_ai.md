# Exploratory Factor Analysis Optimization with AI Assistance

Automatically refines an Exploratory Factor Analysis (EFA) solution by
combining model‐fit criteria (scaled RMSEA) and item‐loading quality,
and—when requested— integrates AI-generated conceptual analyses and
factor naming. At each iteration it:

1.  Estimates the specified EFA model on the current set of items.

2.  Computes the scaled RMSEA and checks whether it is ≤
    `threshold_rmsea` and every factor retains at least
    `min_items_per_factor` items.

3.  If fit or structure criteria are not met, identifies and removes the
    item whose exclusion yields the greatest RMSEA improvement or that
    exhibits structural issues (cross‐loading or no loading).

4.  Repeats steps 1–3 until both fit and structure criteria are
    satisfied or `max_steps` iterations are reached.

Optionally, when `analyze_removed = TRUE` and `item_definitions` are
provided, it calls the OpenAI API to generate concise justifications for
both exclusion and retention of each item, using the provided
definitions and the final loading structure as context. If
`generate_factor_names = TRUE`, it also prompts the AI to propose brief
(1–2 word) tentative names for each factor, taking into account the
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

  Numeric. Minimum absolute loading to consider an item well‐loaded
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

  Additional arguments passed to
  [`EFA_modern`](https://rdrr.io/pkg/PsyMetricTools/man/EFA_modern.html).

## Details

The function proceeds as follows:

1.  Installs and loads `PsyMetricTools` if not already available.

2.  Determines the initial set of items from `items` or from
    `name_items` and `n_items`.

3.  Enters an iterative loop:

    1.  Estimates the EFA model with the current items.

    2.  Computes the scaled RMSEA.

    3.  Evaluates the factor‐loading structure for cross‐loadings or
        lack of loadings.

    4.  If RMSEA ≤ `threshold_rmsea` and each factor has ≥
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

  Original fit indices and other model information from
  [`EFA_modern`](https://rdrr.io/pkg/PsyMetricTools/man/EFA_modern.html).

- specifications:

  Model specifications returned by
  [`EFA_modern`](https://rdrr.io/pkg/PsyMetricTools/man/EFA_modern.html).

- conceptual_analysis:

  A list with elements `removed` and `kept`, each a named list of
  AI-generated texts per item, or `NULL` if `analyze_removed = FALSE`.

- factor_names:

  Named character vector or list of AI‐proposed factor names, or `NULL`
  if `generate_factor_names = FALSE`.

## Examples
