# Convert xpose_set to a nested list.

This amounts to a convenience function for tidy manipulations.

## Usage

``` r
reshape_set(x)

unreshape_set(y)
```

## Arguments

- x:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- y:

  \<[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)\> A
  nested table from an xpose_set

## Value

\<[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)\>
Nested list, or
\<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>

## Examples

``` r
rset <- reshape_set(xpdb_set)
# Properties (exposed and top-level) can be seen. xpdb objects are nested in the xpdb column.
rset %>% dplyr::select(-xpdb) %>% dplyr::glimpse()
#> Rows: 4
#> Columns: 4
#> $ label  <chr> "mod1", "mod2", "fix1", "fix2"
#> $ parent <named list> NA, "mod1", "mod2", "fix1"
#> $ base   <lgl> FALSE, FALSE, FALSE, FALSE
#> $ focus  <lgl> FALSE, FALSE, FALSE, FALSE

unreshape_set(rset)
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 4
#> • Model labels: mod1, mod2, fix1, and fix2
#> • Number of relationships: 3
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none

# The reversibility of reshaping can be confirmed:
identical(xpdb_set,reshape_set(xpdb_set) %>% unreshape_set())
#> [1] TRUE
```
