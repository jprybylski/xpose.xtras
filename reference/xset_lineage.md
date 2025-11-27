# Determine lineage within a set

Determine lineage within a set

## Usage

``` r
xset_lineage(xpdb_s, ..., .spinner = NULL)
```

## Arguments

- xpdb_s:

  \<`xpose_set`\> object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  labels for models in the set from which to create lineages (will
  result in a list if multiple labels are used). If empty, lineage from
  base model will be output; if no base, first listed model will be
  used. Always used the most senior model in this list.

- .spinner:

  Set to `FALSE` to not show a loading spinner in interactive mode.

## Value

\<`character`\> vector of
`c('base', 'base child', 'base grandchild', ...)` or list thereof,
depending on dots arguments.

## Details

This function uses a not-especially-optimized tree-searching algorithm
to determine the longest lineage starting from whatever is treated as
the base model. It is based loosely on
\<[`pluck_depth`](https://purrr.tidyverse.org/reference/pluck_depth.html)\>,
but the values at each depth are maintained. As such, for larger sets
this function and, more importantly, functions that use it may take some
time.

## Examples

``` r
# \donttest{

xset_lineage(xpdb_set)
#> [1] "mod1" "mod2" "fix1" "fix2"

set_base_model(xpdb_set, fix1) %>%
  xset_lineage()
#> [1] "fix1" "fix2"

xset_lineage(xpdb_set, fix1)
#> [1] "fix1" "fix2"

# }
```
