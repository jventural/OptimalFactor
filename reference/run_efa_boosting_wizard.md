# Launch the OptimalFactor Wizard (v2)

Opens a guided 5-phase Shiny wizard built on top of
[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
with reliability, external validity and convergent / discriminant
validity steps. Coexists with the original studio app
[`run_efa_boosting`](https://jventural.github.io/OptimalFactor/reference/run_efa_boosting.md)
— neither replaces the other.

The five phases are:

1.  **Data** — load a CSV / Excel / RDS and select the items of the
    scale under analysis.

2.  **Parallel diagnostic** — multi-method consensus (Kaiser, Horn's
    parallel analysis, Velicer's MAP, BIC) suggesting the number of
    factors. The user can accept the recommendation or override it with
    a theoretical value.

3.  **EFA boosting** — runs
    [`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
    with the chosen `k`, captures the verbose trail (visible in the
    *Trace* tab) and, when an OpenAI key is provided, appends a
    conceptual analysis of every dropped item.

4.  **Reliability** — McDonald's omega (categorical, via
    `semTools::compRelSEM(ord.scale = TRUE)`) plus Cronbach's alpha per
    factor, alongside CFI/TLI/RMSEA/SRMR of the EFA-derived CFA model.

5.  **Validity** — Pearson correlations between factor scores and (a)
    standalone criterion variables and (b) comparison instruments, with
    automatic detection of multidimensional comparators (sub-prefixes
    like `DERS_AC1`, `DERS_OB1`) and AI-assisted convergent/discriminant
    classification.

On top of the steps, the wizard offers:

- **Autopilot mode** — the AI walks every step on its own after a
  user-configurable read delay, with explicit *Pause*, *Resume* and
  *Back* buttons.

- **Downloadable session log (.txt)** — full audit trail.

- **Downloadable manuscript (.docx)** — APA-7 "Análisis de datos" +
  "Resultados" sections drafted by the AI; tables are inserted from real
  session data (the AI never invents numbers).

- **IA Chat tab** — free-form chat with full context of the fitted
  model, with Markdown rendering of the replies.

## Usage

``` r
run_efa_boosting_wizard(launch.browser = TRUE)
```

## Arguments

- launch.browser:

  Logical. If `TRUE` (default) the app opens in the system browser;
  otherwise the app object is returned for manual launch.

## Value

Invisibly returns the Shiny app object; primarily called for side
effects.

## Required packages

`shiny`, `bslib`, `DT`. Strongly recommended: `psych`, `lavaan`,
`semTools`, `ggplot2`, `commonmark`. For AI features: `httr`,
`jsonlite`. For Word manuscript export: `officer`, `flextable`. For
autopilot timing: `later`.

## Quick start


      library(OptimalFactor)
      run_efa_boosting_wizard()

## See also

[`run_efa_boosting`](https://jventural.github.io/OptimalFactor/reference/run_efa_boosting.md),
[`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
[`report_efa_results`](https://jventural.github.io/OptimalFactor/reference/report_efa_results.md),
[`report_cfa_results`](https://jventural.github.io/OptimalFactor/reference/report_cfa_results.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive launch (opens the browser).
library(OptimalFactor)
run_efa_boosting_wizard()

# Headless / programmatic launch — returns the app object for
# embedding in a hosting environment.
app <- run_efa_boosting_wizard(launch.browser = FALSE)
} # }
```
