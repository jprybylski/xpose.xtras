# Base model for `xpose_set`

Base model for `xpose_set`

## Usage

``` r
set_base_model(xpdb_s, ...)

get_base_model(xpdb_s)

unset_base_model(xpdb_s)
```

## Arguments

- xpdb_s:

  \<`xpose_set`\> object

- ...:

  \<\<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  name of base model

## Value

\<`xpose_set`\> object with a base model

## Examples

``` r
w_base <- xpdb_set %>%
  set_base_model(mod2)
w_base # base model listed in output
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 4
#> • Model labels: mod1, mod2, fix1, and fix2
#> • Number of relationships: 3
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: mod2

get_base_model(w_base) # base model name
#> [1] "mod2"

unset_base_model(w_base) # base model no longer in output
#> 
#> ── xpose_set object ────────────────────────────────────────────────────────────
#> • Number of models: 4
#> • Model labels: mod1, mod2, fix1, and fix2
#> • Number of relationships: 3
#> • Focused xpdb objects: none
#> • Exposed properties: none
#> • Base model: none

```
