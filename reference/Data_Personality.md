# Personality Dataset

A dataset containing responses to a personality assessment scale. This
dataset is provided as an example for demonstrating the EFA optimization
functions in the OptimalFactor package.

## Usage

``` r
data(Data_Personality)
```

## Format

A data frame with observations and item responses for personality
traits.

## Source

Simulated data for demonstration purposes.

## Examples

``` r
data(Data_Personality)
head(Data_Personality)
#> # A tibble: 6 × 15
#>   PPTQ1 PPTQ2 PPTQ3 PPTQ4 PPTQ5 PPTQ6 PPTQ7 PPTQ8 PPTQ9 PPTQ10 PPTQ11 PPTQ12
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#> 1     5     1     5     5     5     3     3     5     5      5      5      1
#> 2     1     5     1     5     5     1     3     5     5      5      5      1
#> 3     1     1     5     5     5     5     1     1     5      5      5      1
#> 4     5     1     4     3     5     4     3     5     5      5      3      1
#> 5     1     3     3     4     3     3     1     5     3      4      1      5
#> 6     5     1     5     5     5     5     5     5     5      5      5      1
#> # ℹ 3 more variables: PPTQ13 <dbl>, PPTQ14 <dbl>, PPTQ15 <dbl>
```
