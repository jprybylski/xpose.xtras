# Mutation method for xpose_set

Mutation method for xpose_set

## Usage

``` r
# S3 method for class 'xpose_set'
mutate(.data, ..., .force = FALSE, .retest = !.force, .rowwise = FALSE)
```

## Arguments

- .data:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Mutations to apply to the xpose_set (passed through to
  \<[`dplyr::mutate`](https://dplyr.tidyverse.org/reference/mutate.html)\>)

- .force:

  \<[`logical`](https://rdrr.io/r/base/logical.html)\> Should top-level
  elements be allowed to be manipulated? (default: `FALSE`)

- .retest:

  \<[`logical`](https://rdrr.io/r/base/logical.html)\> Should the
  xpose_set be retested after mutation? (default: `!force`)

- .rowwise:

  \<[`logical`](https://rdrr.io/r/base/logical.html)\> Should the
  mutation be applied rowwise? (default: `FALSE`)

## Value

A set with updated top-level data (unless focused)

## Examples

``` r
xpdb_set %>%
  # Adds foo = bar for all objects in the set
  mutate(foo = "bar") %>%
  # Reshape to visualize
  reshape_set()
#> # A tibble: 4 × 6
#>   xpdb         label parent       base  focus foo  
#>   <named list> <chr> <named list> <lgl> <lgl> <chr>
#> 1 <xp_xtras>   mod1  <chr [1]>    FALSE FALSE bar  
#> 2 <xp_xtras>   mod2  <chr [1]>    FALSE FALSE bar  
#> 3 <xp_xtras>   fix1  <chr [1]>    FALSE FALSE bar  
#> 4 <xp_xtras>   fix2  <chr [1]>    FALSE FALSE bar  
```
