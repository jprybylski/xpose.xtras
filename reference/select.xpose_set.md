# Selection method for xpose_set

Selection method for xpose_set

## Usage

``` r
# S3 method for class 'xpose_set'
select(.data, ...)
```

## Arguments

- .data:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  (passed through to
  \<[`select_subset`](https://jprybylski.github.io/xpose.xtras/reference/select_subset.md)\>)

## Value

Subset of `xpose` set

## Examples

``` r
xpdb_set %>%
  select(starts_with("fix"))
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 2
#> • Model labels: fix1 and fix2
#> • Number of relationships: 2
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none
#> ! Parent(s) not in {xpose_set} for fix1: mod2

xpdb_set %>%
  select(mod1, fix1)
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 2
#> • Model labels: mod1 and fix1
#> • Number of relationships: 1
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none
#> ! Parent(s) not in {xpose_set} for fix1: mod2
```
