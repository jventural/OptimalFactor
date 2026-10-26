# Print method for specification_search

Print method for specification_search

## Usage

``` r
# S3 method for class 'specification_search'
print(x, top = 10, ...)
```

## Arguments

- x:

  An object returned by
  [`specification_search`](https://jventural.github.io/OptimalFactor/reference/specification_search.md).

- top:

  Number of rows to print. Default 10.

- ...:

  Ignored.

## Value

The input `x`, an object of class `specification_search`, returned
invisibly and unchanged. The method is called for its side effect: it
prints the number of configurations evaluated and how many meet the
CFI/RMSEA targets, the `top` rows of the results table ordered by
composite loss, and the fit indices (CFI, RMSEA, SRMR, loss) and factor
assignment of the best model.
