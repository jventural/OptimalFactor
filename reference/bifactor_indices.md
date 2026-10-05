# Bifactor Statistical Indices

Computes the auxiliary bifactor statistical indices from a fitted
bifactor `lavaan` model: ECV (explained common variance), PUC (percent
of uncontaminated correlations), omega, omega hierarchical
(\\\omega_H\\), omega hierarchical subscale (\\\omega\_{HS}\\), the H
construct-replicability index, and the item-level ECV (I-ECV). These
indices help decide whether a multidimensional scale can be treated as
essentially unidimensional and scored with a single total score
(Rodriguez, Reise & Haviland, 2016).

## Usage

``` r
bifactor_indices(fit, general = NULL, verbose = TRUE)
```

## Arguments

- fit:

  A fitted bifactor `lavaan` object (general factor + orthogonal
  specific factors).

- general:

  Name of the general factor. If `NULL` (default) it is auto-detected as
  the latent variable that loads on all items.

- verbose:

  Logical. If `TRUE` (default), prints the rounded indices to the
  console.

## Value

A list with: `overall` (a one-row data frame with ECV, PUC, omega,
omega_H and H_general), `by_factor` (per specific factor: ECV, omega_S,
omega_HS, H) and `by_item` (general/specific loadings and I-ECV).
Printed rounded to three decimals.

## Details

The model must be a bifactor structure with one general factor loading
on all items plus orthogonal specific factors. Standardized loadings are
used. With general loadings \\g_i\\, specific loadings \\s_i\\ and
errors \\e_i = 1 - g_i^2 - s_i^2\\:

- `ECV` = \\\sum g_i^2 / \sum (g_i^2 + s_i^2)\\.

- `omega_H` = \\(\sum g_i)^2 / Var\_{total}\\, and `omega` adds the
  grouped specific variance to the numerator.

- `omega_HS` (per specific factor) = \\(\sum s_i)^2 / Var\_{sub}\\.

- `H` = \\1/(1 + 1/\sum (\lambda_i^2/(1-\lambda_i^2)))\\.

- `PUC` = proportion of item correlations that are between items of
  different specific factors.

- `IECV_i` = \\g_i^2 / (g_i^2 + s_i^2)\\ (item purity toward G).

## References

Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating
bifactor models: Calculating and interpreting statistical indices.
*Psychological Methods, 21*(2), 137–150.

## Examples

``` r
# Simulate data from a bifactor population (G + three specific factors)
pop <- '
G  =~ 0.6*x1 + 0.6*x2 + 0.6*x3 + 0.6*x4 + 0.6*x5 + 0.6*x6 +
      0.6*x7 + 0.6*x8 + 0.6*x9
S1 =~ 0.5*x1 + 0.5*x2 + 0.5*x3
S2 =~ 0.5*x4 + 0.5*x5 + 0.5*x6
S3 =~ 0.5*x7 + 0.5*x8 + 0.5*x9
'
set.seed(1)
dat <- lavaan::simulateData(pop, sample.nobs = 500, orthogonal = TRUE)

mod <- '
G  =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
S1 =~ x1 + x2 + x3
S2 =~ x4 + x5 + x6
S3 =~ x7 + x8 + x9
'
fit <- lavaan::cfa(mod, data = dat, orthogonal = TRUE, std.lv = TRUE)
bi <- bifactor_indices(fit)
#> Bifactor statistical indices (general factor: G )
#> Overall:
#>    ECV  PUC omega omega_H H_general
#>  0.577 0.75 0.793   0.637     0.717
#> 
#> By specific factor:
#>  Factor   ECV omega_S omega_HS     H
#>      S1 0.124   0.631    0.243 0.329
#>      S2 0.194   0.651    0.372 0.457
#>      S3 0.105   0.644    0.195 0.294
bi$by_item
#>   Item Factor   General  Specific     I_ECV
#> 1   x1     S1 0.4440820 0.3937189 0.5598970
#> 2   x2     S1 0.4915281 0.3610763 0.6495039
#> 3   x3     S1 0.4801611 0.3674213 0.6307009
#> 4   x4     S2 0.4163543 0.4813720 0.4279531
#> 5   x5     S2 0.3463718 0.4806573 0.3417999
#> 6   x6     S2 0.4511023 0.4407101 0.5116513
#> 7   x7     S3 0.5179359 0.2764372 0.7782912
#> 8   x8     S3 0.5476438 0.2910210 0.7797927
#> 9   x9     S3 0.4651366 0.4406203 0.5270474
```
