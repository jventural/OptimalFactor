# EFA-Boosting Optimization

Performs an iterative, machine-learning-inspired optimization of
Exploratory Factor Analysis (EFA).

The algorithm integrates: (1) greedy item-by-item elimination, (2)
optional global subset search (multi-item removal), (3) structural rule
enforcement (loadings, Heywood / near-Heywood, cross-loadings, minimum
items per factor), (4) adaptive minimum interfactor correlation checks,
and (5) a composite fit index whose weights adapt dynamically to degrees
of freedom *and* sample size (df x N), following Kenny and McCoach
(2003) and Shi, Lee and Maydeu-Olivares (2019).

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
  thresholds = list(loading = 0.3, min_items_per_factor = 3,
    heywood_tol = 1e-06, near_heywood = 0.015,
    min_interfactor_correlation = 0.32, min_omega = NULL),
  model_config = list(estimator = "WLSMV", rotation = "oblimin"),
  performance = list(max_candidates_eval = 12, smart_pruning = TRUE,
    timeout_efa = 30, timeout_optimization = 120, use_timeouts = FALSE,
    emit_progress = TRUE, fit_target_only = TRUE),
  use_global = FALSE,
  global_opt = list(max_drop = 2, max_global_combinations = 5000,
    verbose = TRUE, progress_bar = TRUE),
  fit_config = list(targets = list(rmsea = 0.08, srmr = 0.06, cfi = 0.95),
    margins = list(rmsea = 0.03, srmr = 0.03, cfi = 0.03),
    base_weights = list(rmsea = 0.5, srmr = 0.25, cfi = 0.25),
    small_df_cut = 5,
    small_df_weights = list(rmsea = 0.15, srmr = 0.45, cfi = 0.4),
    wlsmv_boost = list(rmsea = 0.8, srmr = 1.2, cfi = 1.1),
    use_pclose_if_available = TRUE, pclose_bonus = 0.1),
  use_ai_analysis = FALSE,
  ai_config = list(api_key = NULL, generate_names = FALSE,
    only_removed = TRUE, gpt_model = "gpt-3.5-turbo", language = "english",
    analysis_detail = "detailed", domain_name = "Default Domain",
    scale_title = "Default Scale Title", construct_definition = "",
    model_name = "EFA Model", item_definitions = NULL),
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  Data frame of item responses.

- name_items:

  Item prefix (e.g., `"IT"` produces IT1, IT2, ...). Used for
  auto-detection.

- item_range:

  Integer vector `c(start, end)`. If `NULL`, items are auto-detected.

- n_factors:

  Number of factors.

- n_sample:

  Sample size. If `NULL`, auto-detected as `nrow(data)`. Used for df x N
  adaptive weighting of the composite fit index.

- exclude_items:

  Items excluded before starting optimization.

- thresholds:

  Rules governing structural decisions:

  - `loading`: Minimum acceptable loading.

  - `min_items_per_factor`: Structural protection rule.

  - `heywood_tol`, `near_heywood`: Detection thresholds.

  - `min_interfactor_correlation`: Minimum acceptable factor
    correlation.

  - `min_omega`: Reliability floor. When set (e.g. `0.80`), an item is
    removed only if every factor keeps McDonald's omega at or above this
    value, so the algorithm cannot buy fit at the cost of internal
    consistency. Omega is a guard rail here, never a term in the loss:
    reliability is only interpretable once the model fits, so it vetoes
    individual removals instead of competing with the fit indices. A
    factor that already sits below the floor is not frozen - its
    effective bar becomes its current omega, so removals that do not
    reduce reliability are still allowed. Heywood cases are exempt,
    since an inadmissible solution must be fixed regardless. `NULL`
    (default) disables the check and reproduces the behaviour of
    versions prior to 1.3.0 exactly.

- model_config:

  EFA estimation configuration:

  - `estimator = "WLSMV"`

  - `rotation = "oblimin"`

- performance:

  Performance and timeout settings:

  - `max_candidates_eval`: If set, only this many candidates are
    evaluated in greedy mode.

  - `timeout_efa`: Timeout per EFA run (requires `R.utils`).

  - `timeout_optimization`: Global timeout.

  - `use_timeouts`: Enable/disable timeout protection.

  - `smart_pruning`: Rank candidates by smallest maximum loading before
    applying `max_candidates_eval`, so the items most likely to be
    removed are the ones evaluated.

  - `emit_progress`: Emit a
    [`message()`](https://rdrr.io/r/base/message.html) per iteration and
    per candidate, which is what lets a Shiny app show live progress.

  - `fit_target_only`: Fit only the `n_factors` solution while
    evaluating a candidate (default `TRUE`). The engine used to fit the
    1..k-1 solutions as well and discard them, since candidate
    evaluation reads only the target one. Results are identical,
    verified on `Data_Personality` and `Data_Expectativas` across
    structure, removed items, stop reason, RMSEA, omega and the fit
    table, with 38 percent fewer lavaan fits where the greedy loop
    evaluates candidates. `FALSE` restores the previous behaviour.

- use_global:

  Enable global subset search (multi-item removal).

- global_opt:

  Configuration for global search:

  - `max_drop`: Maximum subset size *k*.

  - `max_global_combinations`: Hard cap to avoid explosion.

  - `verbose`: Print global-search diagnostics.

  - `progress_bar`: Show a visual progress bar.

- fit_config:

  Configuration of the adaptive composite fit index:

  - `targets`: Target values for RMSEA, SRMR, CFI.

  - `margins`: Tolerances for each index.

  - `base_weights`: Default RMSEA/SRMR/CFI weights.

  - `critical_weights`: df \< 5 and N \< 200 (RMSEA nearly useless).

  - `df_low_n_high_weights`: df \< 5 and N \>= 200.

  - `df_mid_n_low_weights`: df 5-19 and N \< 200.

  - `df_mid_n_high_weights`: df 5-19 and N \>= 200.

  - `critical_df_cut`, `moderate_df_cut`: df boundaries.

  - `small_n_cut`: N-boundary for weighting.

  - `wlsmv_boost`: Multiplicative corrections for WLSMV.

  - `use_pclose_if_available`: Enables p-close bonus.

  - `pclose_bonus`: Amount subtracted from composite loss.

- use_ai_analysis:

  Enable GPT-based conceptual analysis of removed (and optionally
  retained) items.

- ai_config:

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

- verbose:

  If TRUE, prints the full diagnostic report, global-search bars, item
  maps, interfactor-correlation warnings, and AI progress bars.

- ...:

  Additional arguments passed to internal estimation routines.

## Details

**1. Optimization strategy.**

The algorithm follows a strict hierarchical rule system:

- Remove Heywood items (\\\psi\\ \< -tol or \|loading\| \> 1).

- Remove near-Heywood items (\\\psi\\ ~ 0).

- Resolve cross-loadings by removing items with smallest ambiguity gap.

- Enforce minimum items per factor.

- If structure is acceptable but RMSEA \> target: remove the
  weakest-loading item.

- If `use_global = TRUE`: evaluate all subsets up to `max_drop` using
  the composite loss.

**2. Adaptive composite loss (df x N).**

Weights for RMSEA / SRMR / CFI adapt dynamically according to:

- degrees of freedom (df),

- sample size (N),

- estimator (WLSMV boosters),

- p-close \>= .05 (bonus).

This allows stable decisions even when RMSEA is unreliable (df \< 5).

**3. Interfactor correlation rule.**

After each iteration and at finalization, factor correlations are
checked:

- If any \\\|\phi\_{ij}\|\\ \< `min_interfactor_correlation`, a warning
  is printed.

**4. Global subset search.**

If enabled:

- evaluates all subsets of size 1 ... k,

- uses safe-combination caps,

- shows a progress bar,

- accepts subset removals only if composite loss strictly improves.

**5. AI conceptual analysis.**

If `use_ai_analysis = TRUE`:

- removed items receive a narrative justification using loadings,
  ambiguity gaps, h^2, \\\psi\\, RMSEA-at-removal, and algorithmic
  reason,

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

- `stop_reason`: Why the loop ended (`"all_criteria_met"`,
  `"min_items_per_factor_protected"`, `"min_omega_protected"`,
  `"fit_target_reached"`, `"max_iterations"`, `"not_enough_items"`,
  `"timeout"`, `"efa_convergence_failed"`,
  `"fit_zero_no_structural_problem"`).

- `inter_factor_correlation`: Final phi matrix.

- `interfactor_check`: Information about violations.

- `omega_final`: McDonald's omega per factor in the final solution,
  computed from the primary loadings of the pattern matrix. Always
  reported, whether or not the floor is active.

- `omega_check`: The floor in force, whether it was met, and
  `blocked_items` (items the floor kept from being removed).

- `last_h2`, `last_psi`: Final communalities and uniquenesses.

- `last_flags`: Heywood and near-Heywood indicators.

- `conceptual_analysis`: GPT-based narrative analyses.

- `config_used`: Full configuration list.

## References

Kenny, D. A., & McCoach, D. B. (2003). Effect of the number of variables
on measures of fit in structural equation modeling. *Structural Equation
Modeling, 10*(3), 333-351.
[doi:10.1207/S15328007SEM1003_1](https://doi.org/10.1207/S15328007SEM1003_1)

Shi, D., Lee, T., & Maydeu-Olivares, A. (2019). Understanding the model
size effect on SEM fit indices. *Educational and Psychological
Measurement, 79*(2), 310-334.
[doi:10.1177/0013164418783530](https://doi.org/10.1177/0013164418783530)

## See also

[`print_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md),
[`export_conceptual_analysis`](https://jventural.github.io/OptimalFactor/reference/export_conceptual_analysis.md)

## Examples

``` r
# \donttest{
data(Data_Personality)
res <- efa_boosting(
  data       = Data_Personality,
  name_items = "PPTQ",
  n_factors  = 3,
  verbose    = TRUE
)
#> Sample size auto-detected: N = 100 
#> 
#> ╔════════════════════════════════════════════════════════════════╗
#> ║   EFA OPTIMIZER v4.1 (Greedy/Global + Objetivo compuesto)      ║
#> ╚════════════════════════════════════════════════════════════════╝
#> 
#> Items iniciales: 15 | Factores: 3 
#> Targets → RMSEA≤ 0.08  | SRMR≤ 0.06  | CFI≥ 0.95 
#> Modo de búsqueda: GREEDY 1×1 
#> Min ítems/factor: 3 | Loading umbral: 0.3 
#> 
#> Boosting iter 1: 15 items activos, 0 eliminados hasta ahora
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 8.540726e-18) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 1.397705e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 1.268967e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> 
#> ──────────────────────────────────────────────────────────────────────
#> ITERATION 0 | Loss: 0.010 | RMSEA: 0.056 | SRMR: 0.061 | CFI: 0.952 | df: 63 
#> Items per factor: 3 | 4 | 7 
#> ──────────────────────────────────────────────────────────────────────
#> 
#>     Items     f1     f2    f3
#> 1   PPTQ9  0.754  0.000 0.000
#> 2   PPTQ4  0.610  0.000 0.000
#> 3  PPTQ14 -0.452  0.347 0.000
#> 4  PPTQ12  0.000  0.668 0.000
#> 5   PPTQ2  0.000  0.627 0.000
#> 6   PPTQ6  0.000 -0.621 0.000
#> 7   PPTQ7 -0.387  0.429 0.000
#> 8   PPTQ5  0.000  0.000 0.799
#> 9  PPTQ10  0.000  0.000 0.661
#> 10 PPTQ15  0.000  0.000 0.639
#> 11  PPTQ3  0.000  0.000 0.566
#> 12  PPTQ1  0.000 -0.302 0.502
#> 13  PPTQ8  0.000  0.000 0.360
#> 14 PPTQ11  0.000  0.000 0.315
#> 15 PPTQ13  0.000  0.000 0.000
#> 
#> ❌ Removed PPTQ7 due to: Cross-loading (priority)
#> Boosting iter 2: 14 items activos, 1 eliminados hasta ahora
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -1.842400e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 2.161542e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 5.176050e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> 
#> ──────────────────────────────────────────────────────────────────────
#> ITERATION 1 | Loss: 0.000 | RMSEA: 0.061 | SRMR: 0.060 | CFI: 0.952 | df: 52 
#> Items per factor: 3 | 3 | 7 
#> ──────────────────────────────────────────────────────────────────────
#> 
#>     Items     f1     f2    f3
#> 1   PPTQ9  0.699  0.000 0.000
#> 2   PPTQ4  0.597  0.000 0.000
#> 3  PPTQ14 -0.528  0.400 0.000
#> 4  PPTQ12  0.000  0.647 0.000
#> 5   PPTQ2  0.000  0.644 0.000
#> 6   PPTQ6  0.000 -0.641 0.000
#> 7   PPTQ5  0.000  0.000 0.778
#> 8  PPTQ15  0.000  0.000 0.672
#> 9  PPTQ10  0.000  0.000 0.669
#> 10  PPTQ3  0.000  0.000 0.544
#> 11  PPTQ1  0.000  0.000 0.531
#> 12  PPTQ8  0.000  0.000 0.360
#> 13 PPTQ11  0.000  0.000 0.332
#> 14 PPTQ13  0.000  0.000 0.000
#> 
#> ⚠ Cross-loadings detected but protected by min_items_per_factor; RMSEA target reached, stopping.
#> 📐 STRATEGY: Structural optimization (non-cross-loading)
#> ❌ Removed PPTQ13 due to: No loading 
#> Boosting iter 3: 13 items activos, 2 eliminados hasta ahora
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= -4.983443e-17) 
#>    is smaller than zero. This may be a symptom that the model is not 
#>    identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 2.201994e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> Warning: lavaan->lav_model_vcov():  
#>    The variance-covariance matrix of the estimated parameters (vcov) does not 
#>    appear to be positive definite! The smallest eigenvalue (= 5.931251e-17) 
#>    is close to zero. This may be a symptom that the model is not identified.
#> 
#> ──────────────────────────────────────────────────────────────────────
#> ITERATION 2 | Loss: 0.113 | RMSEA: 0.077 | SRMR: 0.060 | CFI: 0.938 | df: 42 
#> Items per factor: 3 | 3 | 7 
#> ──────────────────────────────────────────────────────────────────────
#> 
#>     Items     f1     f2    f3
#> 1   PPTQ6 -0.671  0.000 0.000
#> 2  PPTQ12  0.645  0.000 0.000
#> 3   PPTQ2  0.640  0.000 0.000
#> 4   PPTQ9  0.000  0.659 0.000
#> 5   PPTQ4  0.000  0.607 0.000
#> 6  PPTQ14  0.364 -0.558 0.000
#> 7   PPTQ5  0.000  0.000 0.779
#> 8  PPTQ10  0.000  0.000 0.690
#> 9  PPTQ15  0.000  0.000 0.671
#> 10  PPTQ3  0.000  0.000 0.542
#> 11  PPTQ1  0.000  0.000 0.520
#> 12  PPTQ8  0.000  0.000 0.355
#> 13 PPTQ11  0.000  0.000 0.333
#> 
#> ⚠ Cross-loadings detected but protected by min_items_per_factor; RMSEA target reached, stopping.
#> 📐 STRATEGY: Structural optimization (non-cross-loading)
#> 
#> ⚠ Structural issue found (PPTQ14) but protected by min_items_per_factor; RMSEA target reached, stopping.
#> 
#> ╔════════════════════════════════════════════════════════════════╗
#> ║                  OPTIMIZATION COMPLETED                        ║
#> ╚════════════════════════════════════════════════════════════════╝
#> 
#> Total iterations: 2 
#> Items removed: PPTQ7, PPTQ13 
#> Final RMSEA: 0.077 
#> 
#> ⚠️  ADVERTENCIA: No se alcanzó el criterio de correlación mínima entre factores (>= 0.32)
#>     Correlaciones que no cumplen el criterio:
#>     - f1-f2: 0.178
#>     - f1-f3: 0.313
#>     - f2-f3: 0.286
#>     Correlación mínima encontrada: 0.178
#> 
#> Omega por factor: f1=0.690 | f2=0.638 | f3=0.764
#> 
#> ✅ Analysis finished successfully.
#> 
res$removed_items
#> [1] "PPTQ7"  "PPTQ13"
res$final_structure
#>     Items         f1         f2        f3
#> 1   PPTQ6 -0.6713424  0.0000000 0.0000000
#> 2  PPTQ12  0.6447529  0.0000000 0.0000000
#> 3   PPTQ2  0.6403550  0.0000000 0.0000000
#> 4   PPTQ9  0.0000000  0.6593586 0.0000000
#> 5   PPTQ4  0.0000000  0.6071600 0.0000000
#> 6  PPTQ14  0.3643084 -0.5578913 0.0000000
#> 7   PPTQ5  0.0000000  0.0000000 0.7788308
#> 8  PPTQ10  0.0000000  0.0000000 0.6900500
#> 9  PPTQ15  0.0000000  0.0000000 0.6706673
#> 10  PPTQ3  0.0000000  0.0000000 0.5421446
#> 11  PPTQ1  0.0000000  0.0000000 0.5200078
#> 12  PPTQ8  0.0000000  0.0000000 0.3547691
#> 13 PPTQ11  0.0000000  0.0000000 0.3328500
# }
```
