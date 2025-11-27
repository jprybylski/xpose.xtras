# Add relationship(s) to an xpose_set

Add relationship(s) to an xpose_set

## Usage

``` r
add_relationship(xpdb_s, ..., .warn = TRUE, .remove = FALSE)

remove_relationship(xpdb_s, ...)
```

## Arguments

- xpdb_s:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more formulas that define relationships between models. One
  list of formulas can also be used, but a warning is generated.

- .warn:

  \<[`logical`](https://rdrr.io/r/base/logical.html)\> Should warnings
  be generated for non-formula inputs? (default: `TRUE`)

- .remove:

  \<[`logical`](https://rdrr.io/r/base/logical.html)\> Should listed
  relationships be removed? (default: `FALSE`)

## Value

An `xpose_set` object with relationships added

## Examples

``` r
xpdb_set %>%
  add_relationship(mod1~fix2) # ouroboros
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 4
#> • Model labels: mod1, mod2, fix1, and fix2
#> • Number of relationships: 4
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none

xpdb_set %>%
  remove_relationship(fix1~mod2) # split down the middle
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 4
#> • Model labels: mod1, mod2, fix1, and fix2
#> • Number of relationships: 2
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none
```
