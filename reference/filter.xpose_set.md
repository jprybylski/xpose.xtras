# Filtration method for xpose_set

Filtration method for xpose_set

## Usage

``` r
# S3 method for class 'xpose_set'
filter(.data, ..., .rowwise = FALSE)
```

## Arguments

- .data:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  (passed through to
  \<[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)\>)

- .rowwise:

  \<[`logical`](https://rdrr.io/r/base/logical.html)\> Should the
  mutation be applied rowwise? (default: `FALSE`)

## Value

A filtered `xpose_set`

## Examples

``` r
xpdb_set %>%
  filter(label=="mod1")
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 1
#> • Model labels: mod1
#> • Number of relationships: 0
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none

xpdb_set %>%
  filter(length(parent)>1, .rowwise=TRUE)
#> ! No xpdb objects in the set.
```
