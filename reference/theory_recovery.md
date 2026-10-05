# How Much of the Theoretical Structure a Solution Recovers

Compares the item-to-factor partition of a solution with the theoretical
key of the instrument and reports how much of the theory survives. It
works on the output of any routine in the package
([`efa_boosting`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
[`discriminant_boosting`](https://jventural.github.io/OptimalFactor/reference/discriminant_boosting.md),
[`specification_search_theory`](https://jventural.github.io/OptimalFactor/reference/specification_search_theory.md)),
on a plain partition, or on a loading matrix, so different algorithms
can be compared on the same scale.

## Usage

``` r
theory_recovery(theory, solution, loadings = NULL)
```

## Arguments

- theory:

  Named list encoding the theoretical key, of the form
  `list(Dimension = c("item1", "item2"), ...)`.

- solution:

  The solution to evaluate. One of: a named list factor -\> items; a
  loading matrix or data frame (items in rows, factors in columns; each
  item is assigned to its largest absolute loading); or the object
  returned by
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md),
  [`cfa_boosting()`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md),
  [`discriminant_boosting()`](https://jventural.github.io/OptimalFactor/reference/discriminant_boosting.md),
  [`specification_search_theory()`](https://jventural.github.io/OptimalFactor/reference/specification_search_theory.md)
  or
  [`local_fit_search()`](https://jventural.github.io/OptimalFactor/reference/local_fit_search.md).

- loadings:

  Optional loading matrix (items in rows) used for the Tucker
  congruence. Taken from `solution` automatically when it is a loading
  matrix or an
  [`efa_boosting()`](https://jventural.github.io/OptimalFactor/reference/efa_boosting.md)
  result.

## Value

A list with `summary` (one-row data frame with `k_theory`, `k_solution`,
`retention`, `accuracy`, `recovery`, `ari` and `tucker_mean`),
`per_dimension` (retention and recovery of each theoretical dimension
and the empirical factor matched to it), `matching`, `tucker` and
`misplaced` (retained items that sit outside their dimension's factor).

## Details

No single number answers the question, because a solution can depart
from the theory in two ways that one index confounds: it can *drop*
items or *move* them to another factor. The metrics are therefore meant
to be read together:

- retention:

  Proportion of the theoretical items that survive.

- accuracy:

  Among the retained items, proportion that sit on the factor matched to
  their own dimension. Factors are matched to dimensions by the
  assignment that maximises the number of hits, searched exhaustively,
  so the result does not depend on the order of the factors.

- recovery:

  `retention * accuracy`: proportion of the theoretical items that end
  up in their place. This is the headline figure.

- ari:

  Adjusted Rand Index (Hubert & Arabie, 1985) between the empirical and
  theoretical partitions of the retained items. It is 0 when the
  agreement is what chance alone would produce and 1 when it is perfect,
  and it does not require the numbers of factors to coincide.

- tucker_mean:

  Only when loadings are available: mean Tucker congruence between each
  factor's loadings and the binary key of its matched dimension. Values
  of .85-.94 indicate fair similarity and .95 or above equality
  (Lorenzo-Seva & ten Berge, 2006). Absolute loadings are used, so a
  reverse-keyed item is not penalised for its sign: polarity is a matter
  of scoring, not of structure.

Accuracy must never be read alone. A confirmatory routine such as
[`cfa_boosting`](https://jventural.github.io/OptimalFactor/reference/cfa_boosting.md)
cannot move items between factors, only drop them, so its accuracy is 1
by construction and its departure from the theory shows up in retention.

## References

Hubert, L., & Arabie, P. (1985). Comparing partitions. *Journal of
Classification, 2*(1), 193-218.
[doi:10.1007/BF01908075](https://doi.org/10.1007/BF01908075)

Lorenzo-Seva, U., & ten Berge, J. M. F. (2006). Tucker's congruence
coefficient as a meaningful index of factor similarity. *Methodology,
2*(2), 57-64.
[doi:10.1027/1614-2241.2.2.57](https://doi.org/10.1027/1614-2241.2.2.57)

## See also

[`algorithm_stability`](https://jventural.github.io/OptimalFactor/reference/algorithm_stability.md),
which applies these metrics to every split of a cross-validation.

## Examples

``` r
theory <- list(A = c("x1", "x2", "x3"), B = c("x4", "x5", "x6"))

# x3 moved to the other factor and x6 dropped
sol <- list(F1 = c("x1", "x2"), F2 = c("x3", "x4", "x5"))
theory_recovery(theory, sol)$summary
#>   k_theory k_solution retention accuracy  recovery       ari tucker_mean
#> 1        2          2 0.8333333      0.8 0.6666667 0.1666667          NA

# A loading matrix works too
L <- matrix(c(.7, .6, .5, .1, .0, .2,
              .1, .0, .2, .8, .7, .6), ncol = 2,
            dimnames = list(paste0("x", 1:6), c("F1", "F2")))
theory_recovery(theory, L)$summary
#>   k_theory k_solution retention accuracy recovery ari tucker_mean
#> 1        2          2         1        1        1   1   0.9730479
```
