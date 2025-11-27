# Add one or more `xpdb` objects to an `xpose_set`

Add one or more `xpdb` objects to an `xpose_set`

## Usage

``` r
add_xpdb(xpdb_s, ..., .relationships = NULL)
```

## Arguments

- xpdb_s:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more `xpdb` objects to add to the set

- .relationships:

  \<[`list`](https://rdrr.io/r/base/list.html)\> A list of relationships
  between the `xpdb` objects.

## Value

An `xpose_set` object with the new `xpdb` objects added

## Examples

``` r
data("xpdb_ex_pk", package = "xpose")

add_xpdb(xpdb_set, ttt=xpdb_ex_pk)
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 5
#> • Model labels: mod1, mod2, fix1, fix2, and ttt
#> • Number of relationships: 3
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none
```
