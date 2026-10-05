# Discriminant-Boosting: Rescue the Discriminant Validity of a Scale

Reespecifies a multidimensional scale whose interfactor correlations are
so high that the factors cannot be told apart — or so high that the
solution is inadmissible, with a correlation above 1 — using the
smallest possible departure from the theoretical structure.

## Usage

``` r
discriminant_boosting(
  data,
  theory,
  reverse_items = NULL,
  phi_max = 0.9,
  cfi_target = 0.95,
  rmsea_target = 0.08,
  omega_min = 0.7,
  min_loading = 0.35,
  min_items_per_factor = 4,
  exclude_reverse = c("auto", "always", "never"),
  try_method_factor = TRUE,
  try_bifactor = TRUE,
  estimator = "WLSMV",
  ordered = TRUE,
  n_cores = 1,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame with the item responses.

- theory:

  Named list encoding the theoretical structure, of the form
  `list(FactorA = c("it1","it2",...), FactorB = c(...))`.

- reverse_items:

  Character vector of reverse-worded items, used to test whether they
  form a method factor. Default `NULL`.

- phi_max:

  Maximum acceptable interfactor correlation. Default `0.90`.

- cfi_target, rmsea_target:

  Fit targets. Defaults `0.95` and `0.08`.

- omega_min:

  Reliability floor per factor. Default `0.70`.

- min_loading:

  Loading floor. Default `0.35`.

- min_items_per_factor:

  Structural protection rule. Default `4`.

- exclude_reverse:

  One of `"auto"`, `"always"` or `"never"`. With `"auto"` (default) a
  method factor over `reverse_items` is fitted and, if it raises CFI by
  more than .02, those items are dropped before the search. Reverse
  wording introduces shared variance that does not belong to the
  construct (Marsh, 1996; Podsakoff et al., 2003).

- try_method_factor, try_bifactor:

  Whether to include the method-factor and the bifactor specifications
  in the ladder. Default `TRUE`.

- estimator, ordered:

  Passed to [`cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html). Defaults
  `"WLSMV"` and `TRUE`.

- n_cores:

  Cores for the candidate evaluations within each greedy iteration.
  Default `1`. Values above 1 use a PSOCK cluster.

- verbose:

  Logical; print progress. Default `TRUE`.

## Value

An object of class `discriminant_boosting`, a list with `best` (the
retained model and its indices), `best_model`, `reason`, `baseline`,
`ladder` (one row per candidate), `refined`, `logs`, `all`,
`reverse_excluded` and `config`.

## Details

A model can fit well and still be uninterpretable. A four-factor
solution whose factors correlate .97 reproduces the covariance matrix as
well as a one-factor solution does, so CFI and RMSEA say nothing about
whether the factors exist. This routine targets the correlation itself.

Three findings shape the algorithm:

- **Pruning by fit does not lower the interfactor correlation.**
  Removing the item that most improves CFI can leave \\\phi\\ untouched
  or even raise it. The greedy search must be guided by \\\phi\\.

- **Letting the data assign items surfaces method variance.** When
  item-to-factor assignment is taken from an exploratory solution, what
  separates is often the wording polarity of the items, not their
  content, and the algorithm reports a method factor as if it were a
  dimension. The assignment therefore stays *theoretical*; only the
  grouping of whole factors and the set of retained items are searched.

- **Optimising fit and \\\phi\\ at once lets fit dominate.** They are
  optimised in sequence: first \\\phi\\, then CFI.

The search proceeds in four phases.

**Phase 0 — diagnosis.** The theoretical model is fitted and its
interfactor correlations inspected. A correlation above 1 is reported as
an inadmissible solution, not as a large correlation: the latent
covariance matrix is no longer positive definite and the model describes
something that cannot exist.

**Phase 1 — ladder of structures.** Candidates are generated ordered by
distance from the original: the theoretical model, the model plus a
method factor, the bifactor model, and every partition of the factors
into \\k-1, k-2, \ldots, 2\\ blocks. Item-to-factor assignment never
changes; only whole factors are merged.

**Phases 2–4 — pruning.** For each level of the ladder the best
candidate is pruned by \\\phi\\, then by CFI while holding \\\phi\\
below the threshold, and finally cleaned of items below the loading
floor.

The winner is the model that meets every criterion *with the largest
number of factors*, not the one with the best fit: the aim is to depart
as little as possible from the intended structure. If none qualifies,
the admissible model with the lowest \\\phi\\ is returned and `reason`
says so.

Reliability floors are *adaptive*: a model already below the floor is
not frozen, its effective bar becomes its current value, so removals
that do not worsen reliability remain available. A floor set above the
starting value would reject every candidate and stall the search on the
first iteration.

*Warning.* Like any specification search this is an EXPLORATORY device
that capitalises on chance (MacCallum, Roznowski & Necowitz, 1992).
Cross-validate the chosen model on an independent sample; see
[`cross_validate_cfa`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md).

## References

Marsh, H. W. (1996). Positive and negative global self-esteem: A
substantively meaningful distinction or artifactors? *Journal of
Personality and Social Psychology, 70*(4), 810-819.

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.

Podsakoff, P. M., MacKenzie, S. B., Lee, J.-Y., & Podsakoff, N. P.
(2003). Common method biases in behavioral research. *Journal of Applied
Psychology, 88*(5), 879-903.

## See also

[`local_fit_search`](https://jventural.github.io/OptimalFactor/reference/local_fit_search.md),
[`specification_search_theory`](https://jventural.github.io/OptimalFactor/reference/specification_search_theory.md),
[`cross_validate_cfa`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md)

## Examples

``` r
# \donttest{
if (requireNamespace("semTools", quietly = TRUE)) {
  data(Data_Personality)
  theory <- list(F1 = paste0("PPTQ", 1:5),
                 F2 = paste0("PPTQ", 6:10),
                 F3 = paste0("PPTQ", 11:15))
  res <- discriminant_boosting(Data_Personality, theory,
           estimator = "MLR", ordered = FALSE, n_cores = 1)
  res
  res$ladder
}
#> 
#> == Phase 0: theoretical model ==
#>    3 factors, 15 items | max phi = -1.620 | CFI = 0.687 | admissible = FALSE
#> 
#> == Phase 1: ladder of structures ==
#>           model converged n_factors n_items   cfi   tli rmsea  srmr    phi
#>     theoretical      TRUE         3      15 0.687 0.623 0.083 0.098 -1.620
#>        bifactor      TRUE         3      15 0.721 0.609 0.085 0.092     NA
#>  2F: F1_F2 | F3      TRUE         2      15 0.688 0.631 0.082 0.099 -1.502
#>  2F: F1 | F2_F3      TRUE         2      15 0.686 0.629 0.083 0.101  1.189
#>  2F: F2 | F1_F3      TRUE         2      15 0.643 0.578 0.088 0.100  1.200
#>  phi_hi phi_over omega_min min_loading admissible meets
#>  -0.523        3     0.002       0.103      FALSE FALSE
#>      NA       NA     0.323       0.013       TRUE FALSE
#>  -0.701        1     0.003       0.104      FALSE FALSE
#>   1.387        1     0.174       0.119      FALSE FALSE
#>   1.645        1     0.171       0.108      FALSE FALSE
#> 
#> == Phases 2-4: pruning per level ==
#> 
#>    [3 factors] candidate: theoretical
#>       -> n=15 | phi=-1.620 | CFI=0.687 | omega_min=0.002 | min_loading=0.103 | meets=FALSE
#> 
#>    [2 factors] candidate: 2F: F1_F2 | F3
#>       -> n=15 | phi=-1.502 | CFI=0.688 | omega_min=0.003 | min_loading=0.104 | meets=FALSE
#> 
#> == Result: theoretical (no model qualifies: the admissible one with the lowest phi is returned) ==
#>            model converged n_factors n_items   cfi   tli rmsea  srmr    phi
#> 1    theoretical      TRUE         3      15 0.687 0.623 0.083 0.098 -1.620
#> 2       bifactor      TRUE         3      15 0.721 0.609 0.085 0.092     NA
#> 4 2F: F1_F2 | F3      TRUE         2      15 0.688 0.631 0.082 0.099 -1.502
#> 3 2F: F1 | F2_F3      TRUE         2      15 0.686 0.629 0.083 0.101  1.189
#> 5 2F: F2 | F1_F3      TRUE         2      15 0.643 0.578 0.088 0.100  1.200
#>   phi_hi phi_over omega_min min_loading admissible meets
#> 1 -0.523        3     0.002       0.103      FALSE FALSE
#> 2     NA       NA     0.323       0.013       TRUE FALSE
#> 4 -0.701        1     0.003       0.104      FALSE FALSE
#> 3  1.387        1     0.174       0.119      FALSE FALSE
#> 5  1.645        1     0.171       0.108      FALSE FALSE
# }
```
