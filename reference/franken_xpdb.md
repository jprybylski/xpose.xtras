# Combine several `xpose_data` objects into one

This is an internal function designed to meet the needs of specific
plotting functions

## Usage

``` r
franken_xpdb(
  ...,
  .cols = NULL,
  .types = NULL,
  prop_transforms = NULL,
  problem,
  quiet = TRUE
)
```

## Arguments

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  `xpose_data` or `xp_xtra` objects

- .cols:

  \<`tidyselect`\> of data columns

- .types:

  \<`character`\> of data types in addition to columns

- prop_transforms:

  \<`function`\> Extra processing using
  \<[`franken_prop`](https://jprybylski.github.io/xpose.xtras/reference/franken_prop.md)\>

- problem:

  \<`numeric`\> Problems to look for `cols` and `types` (defaults all)

- quiet:

  Prevents extra output.

## Value

The first `xpose_data` object with new data columns

## Examples

``` r

franken_xpdb(pheno_base, pheno_final, .types="catcov") %>%
  xpose::get_data() %>%
  select(starts_with("APGR"))
#> Returning data from $prob no.1
#> # A tibble: 744 × 3
#>    APGR  APGR_1 APGR_2
#>    <fct> <fct>  <fct> 
#>  1 7     7      7     
#>  2 7     7      7     
#>  3 7     7      7     
#>  4 7     7      7     
#>  5 7     7      7     
#>  6 7     7      7     
#>  7 7     7      7     
#>  8 7     7      7     
#>  9 7     7      7     
#> 10 7     7      7     
#> # ℹ 734 more rows
```
