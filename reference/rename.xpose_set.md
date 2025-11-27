# Renaming method for xpose_set

Renaming method for xpose_set

## Usage

``` r
# S3 method for class 'xpose_set'
rename(.data, ...)
```

## Arguments

- .data:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  (passed indirectly to
  \<[`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html)\>)

## Value

Re-labeled set

## Examples

``` r
xpdb_set %>%
  rename(Mod = mod1)
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 4
#> • Model labels: Mod, mod2, fix1, and fix2
#> • Number of relationships: 3
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none
#> ! Parent(s) not in {xpose_set} for mod2: mod1
```
