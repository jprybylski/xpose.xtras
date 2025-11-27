# Translate values to levels

This is intended to be used as a convenience function in plotting where
levels are set for some variable.

## Usage

``` r
val2lvl(vals, lvl_tbl = NULL)
```

## Arguments

- vals:

  vector of values associated with levels in `lvl_tbl`

- lvl_tbl:

  tibble of levels

## Value

A vector of levels corresponding to the input vector.
