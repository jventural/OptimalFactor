# EFA-Boosting Optimization

Performs an iterative, machine-learning–inspired optimization of
Exploratory Factor Analysis (EFA).

The algorithm integrates: (1) greedy item-by-item elimination, (2)
optional global subset search (multi-item removal), (3) structural rule
enforcement (loadings, Heywood / near-Heywood, cross-loadings, minimum
items per factor), (4) adaptive minimum interfactor correlation checks,
and (5) a composite fit index whose weights adapt dynamically to degrees
of freedom *and* sample size (df × N), following recommendations by
Kenny, Shi, Savalei, and related literature.

The function handles ordinal data (WLSMV) and includes robust
corrections, adaptive weighting rules, and an optional GPT-based
conceptual analysis of removed (and optionally retained) items.

When `verbose = TRUE`, the function prints a complete *EFA-Boosting
Optimization Summary Report* including:

\(a\) iteration diagnostics (RMSEA, SRMR, CFI, df, composite loss), (b)
identification and elimination of structural problems, (c) global-search
progress bars and decisions, (d) thresholded final structure, (e)
interfactor-correlation compliance check, and (f) AI-based conceptual
interpretations.

Returns a complete list of results invisibly.

## Usage

``` r
efa_boosting(
  data,
  name_items,
  item_range = NULL,
  n_factors = 3,
  n_sample = NULL,
  exclude_items = NULL,
  thresholds = list(...),
  model_config = list(...),
  performance = list(...),
  use_global = FALSE,
  global_opt = list(...),
  fit_config = list(...),
  use_ai_analysis = FALSE,
  ai_config = list(...),
  verbose = TRUE,
  ...
)
```

## Arguments

- `data`:

  Data frame of item responses.

- `name_items`:

  Item prefix (e.g., `"IT"` produces IT1, IT2, …). Used for
  auto-detection.

- `item_range`:

  Integer vector `c(start, end)`. If `NULL`, items are auto-detected.

- `n_factors`:

  Number of factors.

- `n_sample`:

  Sample size. If `NULL`, auto-detected as `nrow(data)`. Used for df × N
  adaptive weighting of the composite fit index.

- `exclude_items`:

  Items excluded before starting optimization.

- `thresholds`:

  Rules governing structural decisions:

  - `loading`: Minimum acceptable loading.

  - `min_items_per_factor`: Structural protection rule.

  - `heywood_tol`, `near_heywood`: Detection thresholds.

  - `min_interfactor_correlation`: Minimum acceptable factor
    correlation.

- `model_config`:

  EFA estimation configuration:

  - `estimator = "WLSMV"`

  - `rotation = "oblimin"`

- `performance`:

  Performance and timeout settings:

  - `max_candidates_eval`: If set, only this many candidates are
    evaluated in greedy mode.

  - `timeout_efa`: Timeout per EFA run (requires `R.utils`).

  - `timeout_optimization`: Global timeout.

  - `use_timeouts`: Enable/disable timeout protection.

- `use_global`:

  Enable global subset search (multi-item removal).

- `global_opt`:

  Configuration for global search:

  - `max_drop`: Maximum subset size *k*.

  - `max_global_combinations`: Hard cap to avoid explosion.

  - `verbose`: Print global-search diagnostics.

  - `progress_bar`: Show a visual progress bar.

- `fit_config`:

  Configuration of the adaptive composite fit index:

  - `targets`: Target values for RMSEA, SRMR, CFI.

  - `margins`: Tolerances for each index.

  - `base_weights`: Default RMSEA/SRMR/CFI weights.

  - `critical_weights`: df \< 5 and N \< 200 (RMSEA nearly useless).

  - `df_low_n_high_weights`: df \< 5 and N \>= 200.

  - `df_mid_n_low_weights`: df 5–19 and N \< 200.

  - `df_mid_n_high_weights`: df 5–19 and N \>= 200.

  - `critical_df_cut`, `moderate_df_cut`: df boundaries.

  - `small_n_cut`: N-boundary for weighting.

  - `wlsmv_boost`: Multiplicative corrections for WLSMV.

  - `use_pclose_if_available`: Enables p-close bonus.

  - `pclose_bonus`: Amount subtracted from composite loss.

- `use_ai_analysis`:

  Enable GPT-based conceptual analysis of removed (and optionally
  retained) items.

- `ai_config`:

  AI analysis configuration:

  - `api_key`: OpenAI API key.

  - `gpt_model`: Model (e.g., `"gpt-4"`, `"gpt-3.5-turbo"`).

  - `language`: "spanish" or "english".

  - `analysis_detail`: "brief", "standard", "detailed".

  - `domain_name`, `scale_title`, `model_name`: Metadata included in
    prompts.

  - `construct_definition`: Theoretical definition of the latent
    construct.

  - `item_definitions`: Named list with item wording.

  - `only_removed`: If FALSE, retained items are also analyzed.

- `verbose`:

  If TRUE, prints the full diagnostic report, global-search bars, item
  maps, interfactor-correlation warnings, and AI progress bars.

## Details

**1. Optimization strategy.**

The algorithm follows a strict hierarchical rule system:

- Remove Heywood items (ψ \< –tol or \|loading\| \> 1).

- Remove near-Heywood items (ψ ≈ 0).

- Resolve cross-loadings by removing items with smallest ambiguity gap.

- Enforce minimum items per factor.

- If structure is acceptable but RMSEA \> target: remove the
  weakest-loading item.

- If `use_global = TRUE`: evaluate all subsets up to `max_drop` using
  the composite loss.

**2. Adaptive composite loss (df × N).**

Weights for RMSEA / SRMR / CFI adapt dynamically according to:

- degrees of freedom (df),

- sample size (N),

- estimator (WLSMV boosters),

- p-close ≥ .05 (bonus).

This allows stable decisions even when RMSEA is unreliable (df \< 5).

**3. Interfactor correlation rule.**

After each iteration and at finalization, factor correlations are
checked:

- If any \|φ_ij\| \< `min_interfactor_correlation`, a warning is
  printed.

**4. Global subset search.**

If enabled:

- evaluates all subsets of size 1 … k,

- uses safe-combination caps,

- shows a progress bar,

- accepts subset removals only if composite loss strictly improves.

**5. AI conceptual analysis.**

If `use_ai_analysis = TRUE`:

- removed items receive a narrative justification using loadings,
  ambiguity gaps, h², ψ, RMSEA-at-removal, and algorithmic reason,

- retained items can also be evaluated,

- exponential-backoff retry logic ensures robustness,

- a detailed elimination timeline is integrated into the prompt.

## Value

A list containing:

- `final_structure`: Thresholded loading matrix.

- `removed_items`: Items removed (in chronological order).

- `steps_log`: Full elimination log with the following columns:

  - `step`: Iteration number

  - `removed_item`: Name of the removed item

  - `reason`: Reason for removal

  - `rmsea`: RMSEA value at the moment of removal (scaled when WLSMV is
    used)

  - `srmr`: SRMR value at the moment of removal

  - `cfi`: CFI value at the moment of removal (scaled when WLSMV is
    used)

- `iterations`: Number of optimization iterations.

- `final_rmsea`: Final RMSEA.

- `bondades_original`: Fit indices from the final model.

- `inter_factor_correlation`: Final φ matrix.

- `interfactor_check`: Information about violations.

- `last_h2`, `last_psi`: Final communalities and uniquenesses.

- `last_flags`: Heywood and near-Heywood indicators.

- `conceptual_analysis`: GPT-based narrative analyses.

- `config_used`: Full configuration list.

## See also

[`print_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md),
[`export_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/export_conceptual_analysis.md)

## Examples

``` r
if (FALSE) { # \dontrun{
X <- replicate(12, rnorm(500))
X <- as.data.frame(X); names(X) <- paste0("IT",1:12)

res <- efa_boosting(
  data = X,
  name_items = "IT",
  n_factors = 3,
  n_sample = 500,
  use_global = TRUE,
  verbose = TRUE
)
} # }
```
