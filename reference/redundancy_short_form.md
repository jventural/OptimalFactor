# Redundancy-Guided Short Form of a Unidimensional Scale

Builds a short form of an (essentially) unidimensional scale by
iteratively removing the most locally-dependent item. At each step a
one-factor model is fitted, the pair of items with the largest residual
correlation is located, and the item of that pair with the *lower*
loading is dropped (keeping the stronger indicator). Pruning stops when
the target length `k` is reached or when no residual correlation exceeds
`threshold`. This addresses the common situation where a scale is
unidimensional but the one-factor model misfits because of
near-duplicate items (local dependence).

## Usage

``` r
redundancy_short_form(
  data,
  items,
  k = NULL,
  groups = NULL,
  min_per_group = 3,
  threshold = 0.15,
  estimator = "WLSMV",
  ordered = TRUE
)
```

## Arguments

- data:

  Data frame with the item responses.

- items:

  Character vector with the candidate item names.

- k:

  Target number of items. If `NULL`, prunes until the largest residual
  correlation drops below `threshold`. Default `NULL`.

- groups:

  Optional named list mapping content groups to items (e.g. theoretical
  dimensions), used only to preserve at least `min_per_group` items per
  group during pruning. Default `NULL` (no constraint).

- min_per_group:

  Minimum items kept per group in `groups`. Default 3.

- threshold:

  Residual-correlation stopping threshold when `k = NULL`. Default 0.15.

- estimator:

  Estimator passed to
  [`lavaan::cfa`](https://rdrr.io/pkg/lavaan/man/cfa.html). Default
  `"WLSMV"`.

- ordered:

  Logical; treat items as ordered. Default `TRUE`.

## Value

A list with `items` (retained items), `trajectory` (data frame with
n_items, cfi, tli, rmsea, srmr and the item dropped at each step), `fit`
(final one-factor `lavaan` object), `loadings` (standardized) and
`omega` (McDonald's omega of the final form).

## Details

Removing redundant items is preferred over piling up residual
covariances, which capitalizes on chance (MacCallum, Roznowski &
Necowitz, 1992). The resulting short form should be cross-validated on
an independent sample (see
[`cross_validate_cfa`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md)).

## References

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490–504.

## See also

[`cross_validate_cfa`](https://jventural.github.io/OptimalFactor/reference/cross_validate_cfa.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  sf <- redundancy_short_form(mydata, paste0("IT", 1:16), k = 7,
          groups = list(A = paste0("IT",1:8), B = paste0("IT",9:16)))
  sf$trajectory; sf$items; sf$omega
} # }
```
