<p align="center"> <img src="man/figures/logo.png" alt="OptimalFactor" width="220"/> </p> <h1 align="center">OptimalFactor</h1> <p align="center">
  A comprehensive package for optimal factor model refinement in both EFA and CFA using machine-learning inspired boosting algorithms. <br /> <a href="https://joseventuraleon.com/"><strong>Visit the author's website</strong></a> <br /><br /> </p> <p align="center"> <a href="https://jventural.github.io/OptimalFactor/"><img src="https://img.shields.io/badge/docs-pkgdown-blue.svg" alt="Documentation"/></a> </p>

## Features

- **EFA-Boosting Algorithm**: Advanced iterative optimization for Exploratory Factor Analysis with smart-pruning of the candidate space (large speed-ups on long instruments) and a canonical `stop_reason` field that explains exactly why the loop ended
- **Adaptive Fit Indices**: Dynamic weights based on df x N following Kenny, Shi & Savalei (2022)
- **Automatic Problem Detection**: Heywood cases, cross-loadings, and low loadings
- **Global Search**: Multi-item removal optimization
- **AI Integration**: Optional GPT-powered conceptual analysis of removed items
- **Specification Search (CFA)**: Heuristic hill-climbing over seed configurations following MacCallum (1986), with move / drop / cov operations and optional bifactor variant

> **Looking for the interactive app?** The *OptimalFactor Wizard* (guided
> 5-phase Shiny interface with reliability, convergent/discriminant validity,
> AI autopilot and APA-7 report downloads) is distributed separately as a
> web application on Posit Connect Cloud. This package contains the analysis
> engine only.

## Installation

You can install the latest version of OptimalFactor from GitHub:

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("jventural/OptimalFactor")
```

## Quick Start

### EFA-Boosting

```r
library(OptimalFactor)

result <- efa_boosting(
  data = your_data,
  name_items = "item",
  n_factors = 3,
  verbose = TRUE
)
```

## Main Functions

| Function | Description |
|----------|-------------|
| `efa_boosting()` | EFA optimization with adaptive composite fit |
| `cfa_boosting()` | CFA optimization with modification indices |
| `specification_search_theory()` | Theory-guided CFA specification search (MacCallum, 1986 + theory-congruence loss) |
| `cross_validate_cfa()` | Split-half cross-validation of a factor model |
| `bifactor_indices()` | Bifactor statistical indices |
| `redundancy_short_form()` | Redundancy-guided short form of a unidimensional scale |
| `item_stability()` | Resampling stability of the EFA-boosting item selection |
| `simulate_recovery()` | Monte Carlo recovery of a known factor structure |
| `report_efa_results()` | Structured + console report of an EFA-boosting run |
| `report_cfa_results()` | Structured + console report of a CFA-boosting run |
| `print_conceptual_analysis()` | Display AI-generated item analyses |

### Is the item selection stable, or is it chance?

Data-driven item purification invites the classic objection of capitalization
on chance (MacCallum, Roznowski & Necowitz, 1992). `item_stability()` answers
it empirically: it re-runs the whole pipeline over resamples and reports how
often each item survives and how stable its factor assignment is.

```r
st <- item_stability(your_data, "item", n_factors = 3, R = 100)
st              # retention rates, factor agreement, unstable decisions
```

An item dropped in 97 of 100 resamples is a defensible removal; one dropped in
55 is a coin flip. The printed summary flags every decision taken in 25–75 % of
resamples as unstable.

### When does the pipeline recover the true structure?

`simulate_recovery()` simulates ordinal data from a population model with good,
cross-loading and weak items, runs the pipeline on each replication and crosses
conditions over sample size, loading size and items per factor.

```r
sim <- simulate_recovery(n = c(200, 500, 1000), loading = c(0.50, 0.70),
                         n_reps = 200)
sim$summary     # recovery rate, sensitivity, specificity per condition
```

Sensitivity (contaminated items removed) and specificity (good items kept) must
be read together: an algorithm that removes everything scores a perfect
sensitivity and a dismal specificity.

### Reliability floor

Removing items to improve fit tends to cost internal consistency. Setting
`min_omega` makes that trade-off explicit: an item is removed only if every
factor keeps McDonald's omega at or above the floor.

```r
efa_boosting(your_data, "item", n_factors = 3,
             thresholds = list(min_omega = 0.80))
```

The floor is a constraint, not a term in the loss — reliability is not
interpretable in the ill-fitting intermediate models the loss visits, so omega
vetoes individual removals instead of trading against the fit indices. Omega per
factor (`omega_final`) is now reported in every run, floor or no floor.

### Theory-Guided Specification Search (CFA)

`specification_search_theory()` runs a heuristic hill-climbing search over CFA
model configurations in the spirit of MacCallum (1986), extended with a
**theory-congruence term** in the loss: candidate models are penalized for
moving items away from their theoretical factor or for dropping theoretical
items, so the search cannot drift toward models that fit well but break the
intended structure. A single `theory_weight` parameter grades how much theory
counts relative to fit (`0` reproduces the classic fit-only search).

For each seed (a candidate number of factors with an initial item-to-factor
assignment — the theoretical structure itself is always included as a seed)
the algorithm iteratively tries three local operations and keeps the change
with the lowest composite loss:

- **move** an item from its current factor to another;
- **drop** an item with low loading or causing strain;
- **cov** add a residual covariance suggested by modification indices.

A bifactor variant (one orthogonal general factor plus the configured group
factors) can be tried automatically for every seed with k >= 2. The procedure
returns every model evaluated, the subset that meets the user-supplied fit
targets (CFI / RMSEA), and the best model under the composite loss.

```r
library(lavaan)
library(OptimalFactor)

# Simulated 3-factor data
sim <- '
  F1 =~ 0.75*x1 + 0.70*x2 + 0.65*x3
  F2 =~ 0.75*x4 + 0.70*x5 + 0.65*x6
  F3 =~ 0.75*x7 + 0.70*x8 + 0.65*x9
  F1 ~~ 0.30*F2 + 0.30*F3
  F2 ~~ 0.30*F3
'
df    <- simulateData(sim, sample.nobs = 500, seed = 2026)
items <- paste0("x", 1:9)

res <- specification_search_theory(
  data          = df,
  items         = items,
  theory        = list(F1 = c("x1","x2","x3"),
                        F2 = c("x4","x5","x6"),
                        F3 = c("x7","x8","x9")),
  theory_weight = 0.5,   # 0 = fit-only (classic search); higher = more conservative
  estimator     = "ML",
  ordered       = FALSE,
  try_bifactor  = TRUE,
  verbose       = TRUE
)

print(res, top = 8)         # ranking by composite loss (+ theory congruence)
res$successful              # configurations meeting CFI / RMSEA targets
summary(res$best$fit)       # fitted lavaan object for the best model
```

> **Deprecated:** the original fit-only `specification_search()` is kept for
> backward compatibility but emits a deprecation warning; use
> `specification_search_theory()` (with `theory_weight = 0` if you really
> want the fit-only behaviour).

**Important caveat — MacCallum (1986).** Specification search capitalizes on
chance. Use it as an **exploratory** device only and:

1. Report the procedure transparently as exploratory.
2. Cross-validate the resulting model with an independent sample or via
   bootstrap.
3. Justify every accepted modification on substantive theoretical grounds.

The function prints this warning at the start of every run (suppress with
`verbose = FALSE`).

## Examples

[Basic tutorial of the R OptimalFactor library](https://rpubs.com/jventural/OptimalFactor)

## References

- MacCallum, R. C. (1986). Specification searches in covariance structure modeling. *Psychological Bulletin, 100*(1), 107–120.
- Saris, W. E., Satorra, A., & van der Veld, W. M. (2009). Testing structural equation models or detection of misspecifications? *Structural Equation Modeling, 16*(4), 561–582.
- Kenny, D. A., Shi, D., & Savalei, V. (2022). Improvements in the goodness of fit assessment for confirmatory factor analysis. *Psychological Methods*.

## License

GPL-3

## Citation

Ventura-Leon, J. (2026). _OptimalFactor: Optimal Factor Analysis with EFA-Boosting Algorithm_ [R package]. GitHub. https://github.com/jventural/OptimalFactor

## Author

Jose Ventura-Leon <jventuraleon@gmail.com>
