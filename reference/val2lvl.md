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

A vector of levels corresponding to the input vector. If `lvl_tbl`
carries an `ordered` attribute set to `TRUE` (see
[`set_var_levels()`](https://jprybylski.github.io/xpose.xtras/reference/set_var_levels.md)'s
`.ordered` argument and
[`lvl_inord()`](https://jprybylski.github.io/xpose.xtras/reference/levelers.md)),
the result is an ordered factor.
