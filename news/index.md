# Changelog

## OptimalFactor 1.3.0

### Breaking-free change: no more PsyMetricTools dependency

- **The package no longer depends on `PsyMetricTools`.** Every
  exploratory model was fitted through `PsyMetricTools::EFA_modern()`, a
  package that is not on CRAN, which blocked OptimalFactor’s own
  submission. The engine (model syntax → lavaan fit → fit measures →
  standardized pattern matrix) is now internal
  (`R/efa_modern_internal.R`) and calls
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html) with
  exactly the same arguments (`mimic = "Mplus"`, `ordered = TRUE`), so
  results are reproduced: on `Data_Personality` the largest loading
  difference against the old engine is 1.6e-05, which is smaller than
  the 1.9e-05 that lavaan itself produces between two identical runs.
  The internal version drops the `purrr` and `magrittr` usage of the
  original and returns a plain `data.frame` instead of a tibble.

### New functions

- **[`item_stability()`](https://jventural.github.io/OptimalFactor/reference/item_stability.md)**:
  runs the whole
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  pipeline over bootstrap or subsample resamples and reports, per item,
  the retention and removal rates plus the stability of its factor
  assignment. Factor labels are aligned to a full-sample reference
  before agreement is computed, since rotation names factors
  arbitrarily. This is the empirical answer to the
  capitalization-on-chance objection (MacCallum, Roznowski & Necowitz,
  1992): an item dropped in 97 of 100 resamples is a defensible
  decision, one dropped in 55 is not. Ships with a `print` method that
  flags decisions taken in 25–75 % of resamples as unstable. Resampling
  defaults to `method = "subsample"`: drawing with replacement
  duplicates cases, which distorts the polychoric correlations ordinal
  estimation relies on, and on a sample of 100 leaves as few as 63
  distinct cases, enough to send a WLSMV fit grinding for minutes.
  Measured on `Data_Personality` with `R = 40` over 8 cores, bootstrap
  took 5m46s with 3 replications hitting the cap against 2m31s with none
  under subsampling.

- **[`simulate_recovery()`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md)**:
  Monte Carlo evidence on when the pipeline recovers a known structure.
  Simulates ordinal data from a population model with good,
  cross-loading and weak items, crosses conditions over sample size,
  loading size and items per factor, and reports recovery rate,
  sensitivity (contaminated items removed) and specificity (good items
  kept). Replications are fitted in parallel through `n_cores`, which is
  what makes a crossed design feasible; the datasets are always drawn
  sequentially from `seed`, so results do not depend on how many cores
  ran them. Ships `print` and `plot` methods.
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws the
  recovery curve against sample size, one line per population loading
  and one panel per number of items per factor, which is the figure a
  validation paper reports.

- **[`simulate_cfa_recovery()`](https://jventural.github.io/OptimalFactor/reference/simulate_cfa_recovery.md)**:
  the confirmatory counterpart, running the same population model
  through
  [`cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md).
  The question is not the same one: the structure is imposed rather than
  discovered, so the assignment cannot be wrong and what is measured is
  the item selection. Reports `exact_rate` (the removed set equals the
  contaminated set item for item), sensitivity, specificity and
  `mean_covs_added`, the residual covariances introduced where the
  population has none, which turns the MacCallum et al. (1992) objection
  into a number.

### `cfa_boosting()` enforces its own loading floor

The confirmatory simulation was built to evaluate the pipeline and
immediately found a defect in it. The optimisation loop tested the fit
targets first and broke as soon as they were met, so the low-loading
check further down was unreachable: an item the user’s own
`thresholds$loading` declares inadmissible survived untouched whenever
global fit happened to be acceptable. And global fit is acceptable far
more often than one would hope. In simulation, a population item loading
.20 sat inside a model with RMSEA = .03, and adding cross-loading items
barely moved the index at all: with six items cross-loading at .60,
RMSEA stayed at .053 while CFI *rose* to .997, because omitted
cross-loadings are absorbed by the interfactor correlation instead of
degrading fit.

- **`thresholds$enforce_loading`** (default `TRUE`): an item below the
  loading floor is now removed regardless of the fit indices, weakest
  first and one per iteration since the loadings are re-estimated after
  each refit. This is an admissibility criterion, not a fit
  optimisation, so the removal is unconditional rather than contingent
  on improving the loss. Items whose removal would breach
  `min_items_per_factor` are protected. `FALSE` restores the previous
  behaviour.
- Measured on the confirmatory simulation (12 replications, 12 good
  items at .60 plus one cross-loading and one weak item): sensitivity
  rose from .167 to .667 at N = 300 and from .208 to .583 at N = 600,
  with specificity unchanged or better (.965 to .986, .965 to .965). The
  floor finds the contaminated items without taking good ones along.
- The trade is real and worth stating. On `Data_Personality` the
  previous behaviour removed one item and bought its fit with a residual
  covariance (RMSEA .083); enforcing the floor removes the weak item as
  well, needs no covariance at all, and ends at RMSEA .091. Slightly
  worse global fit, a defensible retained set, and one less parameter
  that the population may not have.
- **`thresholds$enforce_simple_structure`** (default `TRUE`) and
  **`thresholds$cross_loading`** (default 0.30): the floor catches the
  item that measures nothing, not the one that measures two things. A
  cross-loading item still loads acceptably on its own factor, so the
  floor never sees it. Detection is by the modification index of the
  absent loading, but never by significance alone: the standardized EPC
  must also reach the threshold, since a significant index with a
  trivial EPC is the capitalization on chance the whole exercise is
  meant to avoid. Adding this took the confirmatory simulation from
  sensitivity .667/.583 to **.958/.958** and exact recovery from
  .250/.167 to **.917/.917**, with specificity rising to **1.000** at
  both sample sizes, fewer spurious residual covariances (.17 to .08 and
  .08 to .00) and better final fit (RMSEA .031 to .013 and .035 to
  .004). Nothing was traded away for it.
- **`stop_reason`** is now returned, with values `all_targets_met`,
  `targets_met_loading_protected`, `no_improving_action` and
  `max_iterations`, so the caller no longer has to infer it from the
  log.

### Misspecified dimensionality: `n_factors_fitted`

Every condition in a recovery design hands the pipeline the true number
of factors, and in practice nobody knows it.
[`simulate_recovery()`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md)
gains `n_factors_fitted`, crossed like the other conditions, so the
population keeps its own dimensionality while the algorithm is told
something else.

The answer is asymmetric, and the asymmetry is the practical finding.
Against a three-factor population with 12 good items, one cross-loading
item and one weak one (12 replications):

| fitted |    recovery | sensitivity |     specificity |      retained |
|-------:|------------:|------------:|----------------:|--------------:|
|      2 |        .000 | .750 / .542 | **.729 / .653** |   9.25 / 8.75 |
|      3 |       1.000 |        .875 |           1.000 |         12.25 |
|      4 | .667 / .556 |    **.000** |    .986 / 1.000 | 13.83 / 14.00 |

Extracting too few factors is destructive: specificity collapses to
.65-.73, so between three and four good items are discarded to force the
data into a structure that cannot hold them. Extracting too many makes
the pipeline inert instead: sensitivity falls to zero, no contaminated
item is removed at all, and 18 of 24 replications stop at
`min_items_per_factor_protected`, because with four factors and fourteen
items almost any removal breaches the minimum. The scale survives
untouched, which is the harmless failure.

The practical reading: when the dimensionality is uncertain, erring
towards more factors leaves the algorithm conservative, while erring
towards fewer leaves it removing good items.

### Population model: cross-loading items no longer share a factor pair

[`simulate_recovery()`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md)
and
[`simulate_cfa_recovery()`](https://jventural.github.io/OptimalFactor/reference/simulate_cfa_recovery.md)
used to place every cross-loading item on the same two factors. With
more than one such item that makes them clones: they correlate strongly,
form a coherent cluster, and the rotation reports the cluster as a
factor of its own, each item showing a single clean loading near .80. No
item-level criterion can call that a cross-loading, because in the
estimated solution it no longer is one, and the good items get displaced
to make room for it. They are now spread over different pairs.

The correction matters because it inverts the verdict. Under three
cross-loading items at .55 plus two weak ones, the old generator made
[`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
look defective (sensitivity .617/.700, recovery falling to .75 at N =
600, contaminated items left in the pool). With the population correctly
specified the same conditions give sensitivity .983/1.000, recovery
1.000 and specificity 1.000, retaining 12.08 and 12.00 items against the
12 good ones in the population. The apparent defect was the measuring
instrument.

### Progress, timeouts and parallel robustness

Resampling exposed three things that made these functions unusable in
practice rather than merely slow.

- **Progress is now reported while a cluster works.** The parallel
  branch used to print nothing at all, which is indistinguishable from a
  hung session. Both simulations and
  [`item_stability()`](https://jventural.github.io/OptimalFactor/reference/item_stability.md)
  now print a bar with elapsed time and an estimate of what is left,
  announce the cluster starting up, and say in advance whether the bar
  will advance per round or jump at the end. Status lines go through
  [`flush.console()`](https://rdrr.io/r/utils/flush.console.html),
  without which the R console holds them until the computation ends.
- **`timeout` (default 120 s per replication)** in
  [`item_stability()`](https://jventural.github.io/OptimalFactor/reference/item_stability.md),
  [`simulate_recovery()`](https://jventural.github.io/OptimalFactor/reference/simulate_recovery.md)
  and
  [`simulate_cfa_recovery()`](https://jventural.github.io/OptimalFactor/reference/simulate_cfa_recovery.md).
  A bootstrap draw of N = 100 can leave 63 distinct cases, and a WLSMV
  fit on the near singular matrix that follows runs for minutes:
  measured, one such replication passed 180 s without finishing, while
  the same one capped returns in 69 s. One replication like that stalls
  an entire run. Capped replications are counted and reported rather
  than hidden.
- **Workers never outnumber tasks**, and the master’s
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is pushed to
  them before loading the package, so a session that added a library at
  runtime (RStudio project libraries, `renv`) does not produce workers
  that cannot find OptimalFactor. That failure previously surfaced as an
  opaque `checkForRemoteErrors()` message, because the guard tested for
  an error while
  [`requireNamespace(quietly = TRUE)`](https://rdrr.io/r/base/ns-load.html)
  merely returns `FALSE`.

### Performance

- **`performance$fit_target_only`** (default `TRUE`) in
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md).
  Evaluating a candidate reads only the `n_factors` solution, yet the
  engine fitted the 1..k-1 solutions as well and discarded them.
  Verified identical on `Data_Personality` and `Data_Expectativas`
  (structure, removed items, stop reason, RMSEA, omega and the fit
  table), with 38 % fewer lavaan fits and a 1.43x speedup where the
  greedy loop evaluates candidates. `FALSE` restores the previous
  behaviour.
- Cluster work is dispatched with `parLapplyLB`: replication times are
  heavy tailed, so load balancing matters more than it would with
  uniform tasks.

### Bug fixes

- `fits[[r]] <- NULL` deleted the slot instead of filling it, and a
  failed replication returns exactly `NULL`. Any condition where a
  replication failed aborted with a subscript error; the same idiom in
  [`item_stability()`](https://jventural.github.io/OptimalFactor/reference/item_stability.md)
  kept its progress counter permanently at “0 failed”.
- The fitting workers addressed
  [`OptimalFactor::efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  even when running in process, which fails under `devtools::load_all()`
  because nothing is exported yet. The error was swallowed and every
  replication was recorded as a silent non-convergence, a result that
  looked like evidence.
- The skewed response thresholds were not cumulative proportions, so the
  skew came out reversed with about 70 % of responses in the top
  category.
- Passing `performance` through `...` to the resampling functions raised
  “formal argument matched by multiple actual arguments” instead of
  overriding the defaults. It is now merged.

### Tests

- The package ships `tests/testthat` for the first time: 170 checks over
  the population model, the ordinal simulation, factor alignment,
  recovery scoring for both pipelines, the progress helpers and the
  timeout translation, with regression tests for each of the fixes
  above.

### Reliability floor

- **`thresholds$min_omega`** in
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  and **`min_omega`** in
  [`redundancy_short_form()`](https://jventural.github.io/OptimalFactor/reference/redundancy_short_form.md):
  an item is removed only if every factor keeps McDonald’s omega at or
  above the floor. It is deliberately a constraint and not a term in the
  loss — reliability is not interpretable in the ill-fitting
  intermediate models the loss visits, so omega vetoes individual
  removals instead of trading against the fit indices. A factor already
  below the floor is not frozen: its effective bar becomes its current
  omega, so removals that do not reduce reliability remain available.
  Heywood cases are exempt, because an inadmissible solution must be
  fixed regardless. `NULL` (default) reproduces the previous behaviour
  exactly.
- New stop reason `min_omega_protected`.
- [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  now always returns `omega_final` (omega per factor) and `omega_check`,
  whether or not the floor is active.
- [`redundancy_short_form()`](https://jventural.github.io/OptimalFactor/reference/redundancy_short_form.md)
  gained an `omega` column in its trajectory plus `stop_reason` and
  `omega_blocked` fields.

## OptimalFactor 1.2.2

### CRAN readiness

- `R CMD check --as-cran` now passes with 0 errors and 0 warnings (apart
  from the expected note about `PsyMetricTools` not yet being on CRAN):
  non-ASCII characters in R code were escaped to `\uXXXX`, runtime calls
  to
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  / `devtools::install_github()` were replaced by informative errors,
  [`library()`](https://rdrr.io/r/base/library.html) calls inside
  functions were removed, and the `Remotes` field was dropped from
  `DESCRIPTION`.
- [`imprimir_items_eliminados()`](https://jventural.github.io/OptimalFactor/reference/imprimir_items_eliminados.md)
  is now exported.
- Documentation fixes: usage sections synchronized with the real default
  arguments of
  [`cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md)
  and
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
  examples updated to the current `cfa_boosting(data, model)` signature,
  duplicated alias for `print.specification_search` removed, and the new
  `print` argument of
  [`report_efa_results()`](https://jventural.github.io/OptimalFactor/reference/report_efa_results.md)
  documented.
- `parallel` and `stats` are now declared in `Imports`.

## OptimalFactor 1.2.1

- All AI calls (conceptual analysis in
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
  [`efa_optimizer()`](https://jventural.github.io/OptimalFactor/reference/efa_optimizer.md),
  [`optimal_efa_with_ai()`](https://jventural.github.io/OptimalFactor/reference/optimal_efa_with_ai.md)
  and
  [`optimal_cfa_with_ai()`](https://jventural.github.io/OptimalFactor/reference/optimal_cfa_with_ai.md))
  now honour the `OPENAI_BASE_URL` environment variable, so they can be
  redirected to any OpenAI-compatible provider — e.g. the Hugging Face
  router (`https://router.huggingface.co/v1`) to use models such as
  `Qwen/Qwen2.5-72B-Instruct` with an `hf_` token. Default behaviour
  (variable unset) is unchanged.

## OptimalFactor 1.2.0

### Breaking changes

- **The Shiny apps moved out of the package.** `run_efa_boosting()` and
  `run_efa_boosting_wizard()` (plus `inst/shiny-apps/` and the two app
  vignettes) were removed: the interactive *OptimalFactor Wizard* is now
  distributed separately as a web application on Posit Connect Cloud.
  The package keeps the analysis engine only, which also trims the
  suggested dependencies (shiny, bslib, DT, officer, flextable, readxl,
  readr, commonmark, later, future, promises are no longer suggested).

### New functions

- **specification_search_theory()**: theory-guided specification search.
  Extends the MacCallum (1986) hill-climbing search with a
  theory-congruence term in the loss (penalizes moving items away from
  their theoretical factor and dropping theoretical items). A single
  `theory_weight` parameter grades how much theory counts relative to
  fit (`0` reproduces the fit-only search).
- **cross_validate_cfa()**: split-half cross-validation of a factor
  model.
- **bifactor_indices()**: bifactor statistical indices.
- **redundancy_short_form()**: redundancy-guided short form of a
  unidimensional scale.

### Deprecated

- **specification_search()** is deprecated in favour of
  [`specification_search_theory()`](https://jventural.github.io/OptimalFactor/reference/specification_search_theory.md)
  and now emits a deprecation warning on every call. It is kept for
  backward compatibility.

## OptimalFactor 1.1.0

### New: Guided wizard for EFA-Boosting

- **run_efa_boosting_wizard()**: a second Shiny app that coexists with
  the original `run_efa_boosting()` studio. Provides a guided 5-phase
  flow (Data → Parallel diagnostic → EFA boosting → Reliability →
  External and convergent/discriminant validity), each with a “proposed
  action / what will happen” panel.

#### Wizard features

- **Multi-method consensus for number of factors**: Kaiser, parallel
  analysis (Horn, 1965), MAP (Velicer, 1976) and BIC. The user can
  accept the consensus recommendation or override it with a theoretical
  value.
- **Trace tab**: captures the verbose stdout of
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  plus the
  [`print_conceptual_analysis()`](https://jventural.github.io/OptimalFactor/reference/print_conceptual_analysis.md)
  output so the entire item-purification process is auditable in a
  single pane. Downloadable as `.txt`.
- **AI integration**: optional autopilot mode that uses OpenAI
  (`gpt-4.1` default) to drive the wizard step by step. Includes a chat
  panel with Markdown rendering of replies (via `commonmark`).
- **Reliability**: omega
  ([`semTools::compRelSEM`](https://rdrr.io/pkg/semTools/man/compRelSEM.html),
  `ord.scale = TRUE`) and Cronbach’s alpha per factor, reported
  alongside CFI/TLI/RMSEA/SRMR of the EFA-derived CFA model.
- **Convergent / discriminant validity with multidimensional
  comparators**:
  - Automatic detection of comparison instruments from column names via
    hierarchical regex (e.g. `DERS_AC1`, `DERS_OB1` → multidimensional
    instrument with two subscales).
  - Mini parallel analysis on each candidate confirms or challenges the
    detected dimensionality.
  - AI auto-classification of each instrument as convergent or
    discriminant based on its label, using
    `response_format = "json_object"`.
  - Per-pair verdict using Cohen’s (1988) magnitude conventions:
    `convergencia fuerte` / `moderada` / `débil` / `no significativa`
    for convergent expectations; `discrimina` / `dudosa` /
    `NO discrimina` for discriminant expectations.
  - Score construction preserves the comparator’s internal structure
    (sub-totals when multidimensional, optional grand total).
  - Heatmap visualization (ggplot2 `geom_tile`) with asterisks for
    `p < .05`.
- **Downloadable session log (.txt)**: full audit trail of every phase,
  numerical detail, items removed, fit indices, omega/alpha and validity
  correlations.
- **Downloadable manuscript (.docx)**: AI-drafted APA-7 sections “2.4
  Análisis de datos” and “3. Resultados” (subsections 3.1–3.6 when
  convergent instruments are present). Tables are inserted from real
  session data via placeholders (`{{TABLE_LOADINGS}}`, `{{TABLE_FIT}}`,
  `{{TABLE_OMEGA}}`, `{{TABLE_EXTERNAL}}`, `{{TABLE_CONVERGENT}}`). The
  AI never invents numbers — it only writes prose around the embedded
  tables. Requires an OpenAI API key.
- **Autopilot controls**: each step exposes `← Back`,
  `⏸ Pause autopilot` and `▶ Resume AI` in a persistent toolbar. The
  user can rewind to any previous phase (even from the “wizard
  completed” state) without losing previously computed results.

### Improvements to `efa_boosting()`

- Added `performance$max_candidates_eval` (default 12) and
  `performance$smart_pruning = TRUE`: ranks candidate items by their
  maximum factor loading and evaluates only the top-K, giving large
  speed-ups on long instruments (e.g. 32 items: \>30 min → ~2–3 min).
- New return field `stop_reason` with canonical values
  (`all_criteria_met`, `min_items_per_factor_protected`,
  `max_iterations`, `not_enough_items`, `efa_convergence_failed`,
  `fit_target_reached`, `fit_zero_no_structural_problem`, `timeout`).
  Allows downstream code to give precise messages without inferring why
  the loop stopped.

### New helpers

- **report_efa_results()** / **report_cfa_results()**: structured
  text-and-data reports of the optimization process. Each returns an
  invisible list with a `$text` field suitable for embedding in
  manuscripts or printing to console.

## OptimalFactor 1.0.0

### Initial CRAN Release

#### Main Features

- **efa_boosting()**: Advanced iterative EFA optimization algorithm
  - Greedy and global search strategies for optimal item selection
  - Adaptive composite fit indices (RMSEA, SRMR, CFI) with weights based
    on df x N
  - Automatic Heywood case and near-Heywood detection
  - Cross-loading identification and removal
  - Minimum items per factor enforcement
  - Interfactor correlation verification
  - Optional AI-assisted conceptual analysis via GPT models
- **run_efa_boosting()**: Launch EFA-Boosting Studio
  - Interactive Shiny application for EFA optimization
  - Real-time console output with auto-scroll
  - Modern Bootstrap 5 interface
  - CSV/Excel data import
  - Configurable fit targets and thresholds
  - Option to exclude specific items
  - Export results to CSV
- **cfa_boosting()**: Confirmatory Factor Analysis optimization
  - Modification indices-based model improvement
  - Automatic covariance addition
  - Fit index monitoring
- **print_conceptual_analysis()**: Formatted output for AI analyses
  - Bilingual support (English/Spanish)
  - Technical statistics display
  - Narrative formatting
- **export_conceptual_analysis()**: Export analysis to various formats
  - Word document export
  - Plain text export
  - Formatted tables

#### Data

- `Data_Expectativas`: Example dataset for expectation scale
- `Data_Personality`: Example personality assessment dataset

#### Dependencies

- Requires PsyMetricTools for EFA_modern function
- lavaan for structural equation modeling
- psych for factor analysis utilities

#### References

- Kenny, D. A., & McCoach, D. B. (2003). Effect of the number of
  variables on measures of fit in structural equation modeling.
  Structural Equation Modeling, 10(3), 333-351.
- Shi, D., Lee, T., & Maydeu-Olivares, A. (2019). Understanding the
  model size effect on SEM fit indices. Educational and Psychological
  Measurement, 79(2), 310-334.
