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
bifactor_indices(fit, general = NULL)
```

## Arguments

- fit:

  A fitted bifactor `lavaan` object (general factor + orthogonal
  specific factors).

- general:

  Name of the general factor. If `NULL` (default) it is auto-detected as
  the latent variable that loads on all items.

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
if (FALSE) { # \dontrun{
  library(lavaan)
  mod <- 'G  =~ x1+x2+x3+x4+x5+x6
          S1 =~ x1+x2+x3
          S2 =~ x4+x5+x6
          G ~~ 0*S1 + 0*S2
          S1 ~~ 0*S2'
  fit <- cfa(mod, data = mydata, ordered = TRUE, estimator = "WLSMV", std.lv = TRUE)
  bifactor_indices(fit)
} # }
```
