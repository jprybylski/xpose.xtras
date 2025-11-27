# Get shrinkage estimates from model summary

This function parses shrinkages as they are currently presented in
[`get_summary`](https://uupharmacometrics.github.io/xpose/reference/get_summary.html),
so it is dependent on the current implementation of that function.

## Usage

``` r
get_shk(xpdb, wh = "eta", .problem = NULL, .subprob = NULL, .method = NULL)
```

## Arguments

- xpdb:

  An `xpose_data` object.

- wh:

  The shrinkage to extract (`"eta"` or `"eps"`)

- .problem:

  Problem number to use. Uses the xpose default if not provided.

- .subprob:

  \<`numeric`\> Subproblem number to use. Uses the xpose default if not
  provided.

- .method:

  \<`character`\> Method to use. Uses the xpose default if not provided.

## Value

A numeric vector of shrinkage estimates.

## Examples

``` r
data("xpdb_ex_pk", package = "xpose")

# eta Shrinkage
get_shk(xpdb_ex_pk)
#> [1]  9.3 28.7 23.7

# epsilon Shrinkage
get_shk(xpdb_ex_pk, wh = "eps")
#> [1] 14.9

```
