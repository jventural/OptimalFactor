# Print Results from AI‐Assisted EFA Refinement

Prints a structured summary of the key outputs from
`optimal_efa_with_ai`, including the removed items, final RMSEA,
iteration count, final factor structure, step‐by‐step log, and final fit
indices.

## Usage

``` r
print_efa_results(res)
```

## Arguments

- res:

  A `list` returned by `optimal_efa_with_ai`, containing at least the
  components `removed_items`, `final_rmsea`, `iterations`,
  `final_structure`, `steps_log`, and `bondades_original`.

## Details

The function prints to the console in the following order:

1.  A “Summary” block with:

    - `removed_items`: the items removed during refinement.

    - `final_rmsea`: the scaled RMSEA after the last iteration.

    - `iterations`: total number of iterations performed.

2.  “Factor Structure” showing the final loadings/data frame.

3.  “Step Log” displaying the data frame of each removal step.

4.  “Final Fit Indices” printing the `bondades_original` object as
    returned by
    [`PsyMetricTools::EFA_modern`](https://rdrr.io/pkg/PsyMetricTools/man/EFA_modern.html).

## Value

invisibly returns `NULL`. Used for its printing side effect.

## Author

Dr. José Ventura‐León

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume you have run:
# res_efa <- optimal_efa_with_ai(...)

# Then print the results:
print_efa_results(res_efa)
} # }
```
