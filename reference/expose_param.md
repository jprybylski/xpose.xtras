# Expose a model parameter of xpdb objects in an xpose_set

Expose a model parameter of xpdb objects in an xpose_set

## Usage

``` r
expose_param(xpdb_s, ..., .problem = NULL, .subprob = NULL, .method = NULL)
```

## Arguments

- xpdb_s:

  \<[`xpose_set`](https://jprybylski.github.io/xpose.xtras/reference/xpose_set.md)\>
  An xpose_set object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  One or more parameter to expose, using selection rules from
  [`add_prm_association`](https://jprybylski.github.io/xpose.xtras/reference/add_prm_association.md).

- .problem:

  \<`numeric`\> Problem number to apply this relationship.

- .subprob:

  \<`numeric`\> Problem number to apply this relationship.

- .method:

  \<`numeric`\> Problem number to apply this relationship.

## Value

An `xpose_set` object with the parameter exposed

## Details

The parameter returned will be top-level, and to avoid conflicting names
will be prepended by `..` (e.g., `..ome1`). The selector used to fetch
the parameter will be used in this `..` name. If a better name is
preferred, there are convenient renaming functions from `dplyr` where
needed.

When using parameter selectors, quotations should be used for more
complex names, like `"OMEGA(1,1)"`, since these may be read incorrectly
otherwise.

The untransformed parameter is used for this exposure. The `get_prm`
call uses `transform=FALSE`.

## See also

[`expose_property()`](https://jprybylski.github.io/xpose.xtras/reference/expose_property.md)

## Examples

``` r
pheno_set %>%
  expose_param(the1) %>%
  reshape_set()
#> # A tibble: 14 × 6
#>    xpdb         label parent       base  focus  ..the1
#>    <named list> <chr> <named list> <lgl> <lgl>   <dbl>
#>  1 <xp_xtras>   run3  <chr [1]>    FALSE FALSE 0.00406
#>  2 <xp_xtras>   run4  <chr [1]>    FALSE FALSE 0.00688
#>  3 <xp_xtras>   run5  <chr [1]>    FALSE FALSE 0.00589
#>  4 <xp_xtras>   run6  <chr [1]>    FALSE FALSE 0.0068 
#>  5 <xp_xtras>   run10 <chr [1]>    FALSE FALSE 0.00682
#>  6 <xp_xtras>   run12 <chr [1]>    FALSE FALSE 0.00671
#>  7 <xp_xtras>   run11 <chr [1]>    FALSE FALSE 0.00681
#>  8 <xp_xtras>   run13 <chr [1]>    FALSE FALSE 0.00681
#>  9 <xp_xtras>   run7  <chr [1]>    FALSE FALSE 0.00678
#> 10 <xp_xtras>   run8  <chr [1]>    FALSE FALSE 0.00711
#> 11 <xp_xtras>   run9  <chr [1]>    FALSE FALSE 0.00613
#> 12 <xp_xtras>   run14 <chr [1]>    FALSE FALSE 0.0073 
#> 13 <xp_xtras>   run15 <chr [1]>    FALSE FALSE 0.00716
#> 14 <xp_xtras>   run16 <chr [1]>    FALSE FALSE 0.00481


pheno_set %>%
  expose_param(RUVADD, "OMEGA(1,1)") %>%
  reshape_set()
#> # A tibble: 14 × 7
#>    xpdb         label parent       base  focus ..RUVADD `..OMEGA(1,1)`
#>    <named list> <chr> <named list> <lgl> <lgl>    <dbl>          <dbl>
#>  1 <xp_xtras>   run3  <chr [1]>    FALSE FALSE     8.35         1.69  
#>  2 <xp_xtras>   run4  <chr [1]>    FALSE FALSE     3.07         0.608 
#>  3 <xp_xtras>   run5  <chr [1]>    FALSE FALSE     2.8          0.198 
#>  4 <xp_xtras>   run6  <chr [1]>    FALSE FALSE     2.86         0.239 
#>  5 <xp_xtras>   run10 <chr [1]>    FALSE FALSE     2.87         0.243 
#>  6 <xp_xtras>   run12 <chr [1]>    FALSE FALSE     2.87         0.232 
#>  7 <xp_xtras>   run11 <chr [1]>    FALSE FALSE     2.86         0.237 
#>  8 <xp_xtras>   run13 <chr [1]>    FALSE FALSE     2.87         0.228 
#>  9 <xp_xtras>   run7  <chr [1]>    FALSE FALSE     2.28         0.254 
#> 10 <xp_xtras>   run8  <chr [1]>    FALSE FALSE     2.78         0.0343
#> 11 <xp_xtras>   run9  <chr [1]>    FALSE FALSE     2.9          0.176 
#> 12 <xp_xtras>   run14 <chr [1]>    FALSE FALSE     2.81         0.0387
#> 13 <xp_xtras>   run15 <chr [1]>    FALSE FALSE     2.86         0.0347
#> 14 <xp_xtras>   run16 <chr [1]>    FALSE FALSE     2.78         0.0404

# This function is useful for generating a model-building table
pheno_set %>%
  # Determine longest lineage
  select(all_of(xset_lineage(.))) %>%
  # Select key variability parameters
  expose_param(RUVADD, "OMEGA(1,1)") %>%
  # Make sure all models have descriptions
  focus_qapply(desc_from_comments) %>%
  # Extract description
  expose_property(descr) %>%
  # Transform to tibble
  reshape_set() # %>% pipe into other processing
#> # A tibble: 6 × 8
#>   xpdb         label parent       base  focus ..RUVADD `..OMEGA(1,1)` ..descr   
#>   <named list> <chr> <named list> <lgl> <lgl>    <dbl>          <dbl> <chr>     
#> 1 <xp_xtras>   run3  <chr [1]>    FALSE FALSE     8.35         1.69   Simplest …
#> 2 <xp_xtras>   run5  <chr [1]>    FALSE FALSE     2.8          0.198  Updated v…
#> 3 <xp_xtras>   run6  <chr [1]>    FALSE FALSE     2.86         0.239  Final str…
#> 4 <xp_xtras>   run9  <chr [1]>    FALSE FALSE     2.9          0.176  Test WT o…
#> 5 <xp_xtras>   run14 <chr [1]>    FALSE FALSE     2.81         0.0387 Final cov…
#> 6 <xp_xtras>   run15 <chr [1]>    FALSE FALSE     2.86         0.0347 Covariate…
```
