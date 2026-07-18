# OptimalFactor 1.2.2

## CRAN readiness

* `R CMD check --as-cran` now passes with 0 errors and 0 warnings (apart
  from the expected note about `PsyMetricTools` not yet being on CRAN):
  non-ASCII characters in R code were escaped to `\uXXXX`, runtime calls to
  `install.packages()` / `devtools::install_github()` were replaced by
  informative errors, `library()` calls inside functions were removed, and
  the `Remotes` field was dropped from `DESCRIPTION`.
* `imprimir_items_eliminados()` is now exported.
* Documentation fixes: usage sections synchronized with the real default
  arguments of `cfa_boosting()` and `efa_boosting()`, examples updated to
  the current `cfa_boosting(data, model)` signature, duplicated alias for
  `print.specification_search` removed, and the new `print` argument of
  `report_efa_results()` documented.
* `parallel` and `stats` are now declared in `Imports`.

# OptimalFactor 1.2.1

* All AI calls (conceptual analysis in `efa_boosting()`, `efa_optimizer()`,
  `optimal_efa_with_ai()` and `optimal_cfa_with_ai()`) now honour the
  `OPENAI_BASE_URL` environment variable, so they can be redirected to any
  OpenAI-compatible provider — e.g. the Hugging Face router
  (`https://router.huggingface.co/v1`) to use models such as
  `Qwen/Qwen2.5-72B-Instruct` with an `hf_` token. Default behaviour
  (variable unset) is unchanged.

# OptimalFactor 1.2.0

## Breaking changes

* **The Shiny apps moved out of the package.** `run_efa_boosting()` and
  `run_efa_boosting_wizard()` (plus `inst/shiny-apps/` and the two app
  vignettes) were removed: the interactive *OptimalFactor Wizard* is now
  distributed separately as a web application on Posit Connect Cloud.
  The package keeps the analysis engine only, which also trims the
  suggested dependencies (shiny, bslib, DT, officer, flextable, readxl,
  readr, commonmark, later, future, promises are no longer suggested).

## New functions

* **specification_search_theory()**: theory-guided specification search.
  Extends the MacCallum (1986) hill-climbing search with a
  theory-congruence term in the loss (penalizes moving items away from
  their theoretical factor and dropping theoretical items). A single
  `theory_weight` parameter grades how much theory counts relative to fit
  (`0` reproduces the fit-only search).
* **cross_validate_cfa()**: split-half cross-validation of a factor model.
* **bifactor_indices()**: bifactor statistical indices.
* **redundancy_short_form()**: redundancy-guided short form of a
  unidimensional scale.

## Deprecated

* **specification_search()** is deprecated in favour of
  `specification_search_theory()` and now emits a deprecation warning on
  every call. It is kept for backward compatibility.

# OptimalFactor 1.1.0

## New: Guided wizard for EFA-Boosting

* **run_efa_boosting_wizard()**: a second Shiny app that coexists with the
  original `run_efa_boosting()` studio. Provides a guided 5-phase flow
  (Data → Parallel diagnostic → EFA boosting → Reliability → External and
  convergent/discriminant validity), each with a "proposed action / what
  will happen" panel.

### Wizard features

* **Multi-method consensus for number of factors**: Kaiser, parallel
  analysis (Horn, 1965), MAP (Velicer, 1976) and BIC. The user can accept
  the consensus recommendation or override it with a theoretical value.
* **Trace tab**: captures the verbose stdout of `efa_boosting()` plus the
  `print_conceptual_analysis()` output so the entire item-purification
  process is auditable in a single pane. Downloadable as `.txt`.
* **AI integration**: optional autopilot mode that uses OpenAI
  (`gpt-4.1` default) to drive the wizard step by step. Includes a chat
  panel with Markdown rendering of replies (via `commonmark`).
* **Reliability**: omega (`semTools::compRelSEM`, `ord.scale = TRUE`) and
  Cronbach's alpha per factor, reported alongside CFI/TLI/RMSEA/SRMR of
  the EFA-derived CFA model.
* **Convergent / discriminant validity with multidimensional comparators**:
  - Automatic detection of comparison instruments from column names via
    hierarchical regex (e.g. `DERS_AC1`, `DERS_OB1` → multidimensional
    instrument with two subscales).
  - Mini parallel analysis on each candidate confirms or challenges the
    detected dimensionality.
  - AI auto-classification of each instrument as convergent or
    discriminant based on its label, using `response_format = "json_object"`.
  - Per-pair verdict using Cohen's (1988) magnitude conventions:
    `convergencia fuerte` / `moderada` / `débil` / `no significativa`
    for convergent expectations; `discrimina` / `dudosa` /
    `NO discrimina` for discriminant expectations.
  - Score construction preserves the comparator's internal structure
    (sub-totals when multidimensional, optional grand total).
  - Heatmap visualization (ggplot2 `geom_tile`) with asterisks for
    `p < .05`.
* **Downloadable session log (.txt)**: full audit trail of every phase,
  numerical detail, items removed, fit indices, omega/alpha and
  validity correlations.
* **Downloadable manuscript (.docx)**: AI-drafted APA-7 sections
  "2.4 Análisis de datos" and "3. Resultados" (subsections 3.1–3.6 when
  convergent instruments are present). Tables are inserted from real
  session data via placeholders (`{{TABLE_LOADINGS}}`, `{{TABLE_FIT}}`,
  `{{TABLE_OMEGA}}`, `{{TABLE_EXTERNAL}}`, `{{TABLE_CONVERGENT}}`). The
  AI never invents numbers — it only writes prose around the embedded
  tables. Requires an OpenAI API key.
* **Autopilot controls**: each step exposes `← Back`, `⏸ Pause autopilot`
  and `▶ Resume AI` in a persistent toolbar. The user can rewind to any
  previous phase (even from the "wizard completed" state) without
  losing previously computed results.

## Improvements to `efa_boosting()`

* Added `performance$max_candidates_eval` (default 12) and
  `performance$smart_pruning = TRUE`: ranks candidate items by their
  maximum factor loading and evaluates only the top-K, giving large
  speed-ups on long instruments (e.g. 32 items: >30 min → ~2–3 min).
* New return field `stop_reason` with canonical values
  (`all_criteria_met`, `min_items_per_factor_protected`, `max_iterations`,
  `not_enough_items`, `efa_convergence_failed`, `fit_target_reached`,
  `fit_zero_no_structural_problem`, `timeout`). Allows downstream code
  to give precise messages without inferring why the loop stopped.

## New helpers

* **report_efa_results()** / **report_cfa_results()**: structured
  text-and-data reports of the optimization process. Each returns an
  invisible list with a `$text` field suitable for embedding in
  manuscripts or printing to console.

# OptimalFactor 1.0.0

## Initial CRAN Release

### Main Features

* **efa_boosting()**: Advanced iterative EFA optimization algorithm
  - Greedy and global search strategies for optimal item selection
  - Adaptive composite fit indices (RMSEA, SRMR, CFI) with weights based on df x N
  - Automatic Heywood case and near-Heywood detection
  - Cross-loading identification and removal
  - Minimum items per factor enforcement
  - Interfactor correlation verification
  - Optional AI-assisted conceptual analysis via GPT models

* **run_efa_boosting()**: Launch EFA-Boosting Studio
  - Interactive Shiny application for EFA optimization
  - Real-time console output with auto-scroll
  - Modern Bootstrap 5 interface
  - CSV/Excel data import
  - Configurable fit targets and thresholds
  - Option to exclude specific items
  - Export results to CSV

* **cfa_boosting()**: Confirmatory Factor Analysis optimization
  - Modification indices-based model improvement
  - Automatic covariance addition
  - Fit index monitoring

* **print_conceptual_analysis()**: Formatted output for AI analyses
  - Bilingual support (English/Spanish)
  - Technical statistics display
  - Narrative formatting

* **export_conceptual_analysis()**: Export analysis to various formats
  - Word document export
  - Plain text export
  - Formatted tables

### Data

* `Data_Expectativas`: Example dataset for expectation scale
* `Data_Personality`: Example personality assessment dataset

### Dependencies

* Requires PsyMetricTools for EFA_modern function
* lavaan for structural equation modeling
* psych for factor analysis utilities

### References

* Kenny, D. A., & McCoach, D. B. (2003). Effect of the number of variables on measures of fit in structural equation modeling. Structural Equation Modeling, 10(3), 333-351.
* Shi, D., Lee, T., & Maydeu-Olivares, A. (2019). Understanding the model size effect on SEM fit indices. Educational and Psychological Measurement, 79(2), 310-334.
