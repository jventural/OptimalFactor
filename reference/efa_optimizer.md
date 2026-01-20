# Exploratory Factor Analysis Optimizer with Optional LLM Support

Automatically refines an Exploratory Factor Analysis (EFA) solution by
combining global fit (scaled RMSEA), item–level loading quality, and
explicit detection of Heywood/near-Heywood cases. At each iteration the
routine re-fits the model, removes the worst offending item (by
structural fault or RMSEA improvement), and stops when predefined
fit/structure criteria are met or no admissible improvement remains.
Optionally, it can call a Large Language Model (LLM) to generate
plain-language justifications for removing/keeping items, with language
control and detail level.

## Usage

``` r
efa_optimizer(
  data,
  name_items,
  item_range = NULL,
  n_factors = 3,
  exclude_items = NULL,
  # Thresholds (includes Heywood tolerances)
  thresholds = list(
    rmsea = 0.08,
    loading = 0.30,
    min_items_per_factor = 3,
    heywood_tol = 1e-6,
    near_heywood = 0.015
  ),
  # Model configuration
  model_config = list(
    estimator = "WLSMV",
    rotation  = "oblimin"
  ),
  # AI (optional)
  use_ai_analysis = FALSE,
  ai_config = list(
    api_key = NULL,
    generate_names = FALSE,
    only_removed = TRUE,
    gpt_model = "gpt-3.5-turbo",
    language = "english",
    analysis_detail = "detailed",
    domain_name = "Default Domain",
    scale_title = "Default Scale Title",
    construct_definition = "",
    model_name = "EFA Model",
    item_definitions = NULL
  ),
  verbose = TRUE, ...
)
```

## Arguments

- `data`:

  A `data.frame` containing observed variables.

- `name_items`:

  Common item prefix (e.g., `"DP"` or `"PPTQ"`).

- `item_range`:

  Integer vector of length 2 with the first/last item indices; if
  `NULL`, items are discovered by the prefix followed by a trailing
  integer.

- `n_factors`:

  Number of factors to extract.

- `exclude_items`:

  Character vector of items to exclude at the start.

- `thresholds`:

  List of decision thresholds:

  `rmsea`

  :   Maximum acceptable scaled RMSEA.

  `loading`

  :   Minimum salient absolute loading.

  `min_items_per_factor`

  :   Minimum items retained per factor (based on primary loading).

  `heywood_tol`

  :   Tolerance for detecting negative uniqueness (\\\psi \<
      -\\`heywood\_tol`) or impossible loadings.

  `near_heywood`

  :   Band around zero uniqueness treated as near-Heywood.

- `model_config`:

  List of EFA fitting options:

  `estimator`

  :   Estimator (e.g., `"WLSMV"`).

  `rotation`

  :   Rotation method (e.g., `"oblimin"`).

- `use_ai_analysis`:

  Logical; if `TRUE`, requests LLM justifications without affecting the
  optimization path.

- `ai_config`:

  List with LLM settings:

  `api_key`

  :   API key (character).

  `generate_names`

  :   If `TRUE`, may also request short factor names (if implemented).

  `only_removed`

  :   If `TRUE`, justify only removed items; otherwise also justify
      retained items.

  `gpt_model`

  :   LLM model identifier.

  `language`

  :   Language for justifications; supports at least `"english"` and
      `"spanish"`.

  `analysis_detail`

  :   Verbosity level for the LLM answer; one of `"brief"`,
      `"standard"`, `"detailed"`.

  `domain_name`, `scale_title`, `construct_definition`, `model_name`

  :   Context strings injected into prompts.

  `item_definitions`

  :   Named list mapping item IDs to plain-language item
      stems/descriptions used by the LLM.

- `verbose`:

  Logical; if `TRUE`, prints progress, current structure, counts per
  factor, and a progress bar during LLM analysis.

- `...`:

  Additional arguments forwarded to
  [`PsyMetricTools::EFA_modern()`](https://rdrr.io/pkg/PsyMetricTools/man/EFA_modern.html).

## Details

**Iteration logic.**

1.  Fit an EFA using
    [`PsyMetricTools::EFA_modern`](https://rdrr.io/pkg/PsyMetricTools/man/EFA_modern.html)
    on the current item set (oblique rotation by default).

2.  Extract the loading matrix \\L\\ and the factor correlation matrix
    \\\Phi\\ (identity if unavailable).

3.  Derive communalities \\h^2 = \mathrm{diag}(L \Phi L')\\ and
    uniquenesses \\\psi = 1 - h^2\\. Flag:

    - Heywood (\\\psi \< -\\`heywood\_tol` or impossible loadings),

    - near-Heywood (\\\psi\\ within `[-near\_heywood, near\_heywood]`),

    - no salient loading (all \\\|loading\| \<\\ `loading`),

    - cross-loadings (2+ salient loadings; severity scored by the gap
      between the two largest absolute loadings).

4.  Enforce `min_items_per_factor` using the primary (largest absolute)
    loading; rows with no salient loading do not count toward any
    factor.

5.  Removal policy (priority order):

    1.  Remove the most severe Heywood item.

    2.  Else remove the worst near-Heywood item.

    3.  Else, if RMSEA exceeds `thresholds$rmsea`, simulate single-item
        removals (that preserve `min\_items\_per\_factor`) and drop the
        candidate yielding the best RMSEA improvement.

    4.  Else remove the structurally worst item (by diagnostic score).

6.  Stop when RMSEA \\\le\\ threshold and all factors meet
    `min\_items\_per\_factor`, or when no admissible removal improves
    fit/structure, or when the maximum number of steps is reached.

**LLM justifications (optional).** If `use_ai_analysis = TRUE` and
`item_definitions` are provided, the function can call an LLM via httr
to draft plain-language justifications. The language
(`ai_config$language`) and verbosity (`ai_config$analysis_detail`)
control the style and length. The request includes, when available,
item-level technical statistics (primary loading, \\h^2\\, reason for
removal, and RMSEA at the removal step). Transient server errors (e.g.,
HTTP 502/503/504) and rate limits (HTTP 429) are handled with up to
three attempts and exponential backoff. This analysis is purely
descriptive and does not influence the optimization path.

**Dependencies.** If LLM is used, httr and jsonlite are required; the
function attempts to install them if missing.

## Value

A list with:

- `final_structure`:

  Data frame with the final, thresholded loadings and item labels.

- `removed_items`:

  Character vector of removed items, in order.

- `steps_log`:

  Data frame with `step`, `removed_item`, `reason`, `rmsea` (value at
  that step).

- `iterations`:

  Number of iterations executed.

- `final_rmsea`:

  Scaled RMSEA at termination.

- `bondades_original`:

  Fit indices returned by `EFA_modern`.

- `specifications`:

  Model specifications returned by `EFA_modern`.

- `inter_factor_correlation`:

  Estimated \\\Phi\\ (identity if unavailable or `n_factors == 1`).

- `last_h2`:

  Vector of communalities from the last iteration.

- `last_psi`:

  Vector of uniquenesses from the last iteration.

- `last_flags`:

  List with logical vectors `heywood` and `near` for the last iteration.

- `conceptual_analysis`:

  If LLM used, a list with:

  `removed`

  :   Named list of justifications for removed items (or `NULL`).

  `kept`

  :   Named list of justifications for retained items if
      `only_removed = FALSE` (or `NULL`).

  `item_stats`

  :   Named list of per-item statistics passed to the LLM (primary
      loading, \\h^2\\, removal reason, RMSEA at removal).

- `config_used`:

  Echo of `thresholds`, `model_config`, `use_ai_analysis`, and
  `ai_config` (including `language` and `analysis_detail`).

## See also

[`PsyMetricTools::EFA_modern`](https://rdrr.io/pkg/PsyMetricTools/man/EFA_modern.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal runnable illustration (without LLM)
set.seed(123)
X <- as.data.frame(matrix(rnorm(300 * 9), ncol = 9))
names(X) <- paste0("DP", 1:9)

res <- efa_optimizer(
  data = X,
  name_items = "DP",
  item_range = c(1, 9),
  n_factors = 3,
  thresholds = list(
    rmsea = 0.08, loading = 0.30, min_items_per_factor = 3,
    heywood_tol = 1e-6, near_heywood = 0.015
  ),
  model_config = list(estimator = "WLSMV", rotation = "oblimin"),
  verbose = TRUE
)

print(res$final_structure)
print(res$steps_log)
res$final_rmsea

# LLM justification sketch (requires a valid API key and item definitions):
# res_ai <- efa_optimizer(
#   data = X,
#   name_items = "DP",
#   item_range = c(1, 9),
#   n_factors = 3,
#   use_ai_analysis = TRUE,
#   ai_config = list(
#     api_key = Sys.getenv("OPENAI_API_KEY"),
#     item_definitions = as.list(setNames(paste("Item", 1:9, "content"), names(X))),
#     language = "spanish",
#     analysis_detail = "standard",
#     only_removed = TRUE
#   ),
#   verbose = TRUE
# )
# str(res_ai$conceptual_analysis, max.level = 1)
} # }
```
