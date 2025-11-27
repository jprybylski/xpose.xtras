# Convenience wrapper for tidyselect

This is intended for use as an internal function to select a subset of
xpdb objects from an xpose_set.

It is a lower level version of
\<[`select.xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/select.xpose_set.md)\>

## Usage

``` r
select_subset(xpdb_s, ...)
```

## Arguments

- xpdb_s:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more tidyselect selectors

## Value

\<[`numeric`](https://rdrr.io/r/base/numeric.html)\> vector of indices
for selected xpdb objects

## Examples

``` r

select_subset(mod2, xpdb_s=xpdb_set)
#> mod2 
#>    2 

select_subset(dplyr::starts_with("fix"), xpdb_s=xpdb_set)
#> fix1 fix2 
#>    3    4 

```
