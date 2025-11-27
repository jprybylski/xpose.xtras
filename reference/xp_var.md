# `xp_var` Method

To add a small amount of functionality to
\<[`xp_var`](https://uupharmacometrics.github.io/xpose/reference/xp_var.html)\>,
this method was created.

## Usage

``` r
xp_var(xpdb, .problem, col = NULL, type = NULL, silent = FALSE)

# Default S3 method
xp_var(xpdb, .problem, col = NULL, type = NULL, silent = FALSE)

# S3 method for class 'xp_xtras'
xp_var(xpdb, .problem, col = NULL, type = NULL, silent = FALSE)
```

## Arguments

- xpdb:

  An xpose database object.

- .problem:

  The \$problem number to be used.

- col:

  The column name to be searched in the index. Alternative to arg
  \`type\`.

- type:

  The type of column to searched in the index. Alternative to \`col\`.

- silent:

  Should the function be silent or return errors.

## Value

A tibble of identified variables.
