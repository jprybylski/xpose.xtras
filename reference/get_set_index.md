# Get full index for xpose_data data

Get full index for xpose_data data

## Usage

``` r
get_index(xpdb, .problem = NULL, ...)

set_index(xpdb, index, ...)
```

## Arguments

- xpdb:

  \<`xpose_data`[xpose::xpose_data](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  object

- .problem:

  \<`numeric`\> Problem number to use. Uses the all problems if `NULL`

- ...:

  Ignored. Here for future expansion

- index:

  \<`tibble`\> Index to set

## Value

Tibble of index

## Examples

``` r
get_index(xpose::xpdb_ex_pk)
#> # A tibble: 46 × 6
#>    table        col   type   label units problem
#>    <chr>        <chr> <chr>  <chr> <chr>   <int>
#>  1 catab001.csv ID    id     NA    NA          1
#>  2 catab001.csv SEX   catcov NA    NA          1
#>  3 catab001.csv MED1  catcov NA    NA          1
#>  4 catab001.csv MED2  catcov NA    NA          1
#>  5 sdtab001     ID    id     NA    NA          1
#>  6 sdtab001     DOSE  na     NA    NA          1
#>  7 sdtab001     AMT   amt    NA    NA          1
#>  8 sdtab001     SS    na     NA    NA          1
#>  9 sdtab001     II    na     NA    NA          1
#> 10 sdtab001     TIME  idv    NA    NA          1
#> # ℹ 36 more rows
```
