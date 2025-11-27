# Set a summary property

Set a summary property

## Usage

``` r
set_prop(xpdb, ..., .problem = NULL, .subprob = NULL)
```

## Arguments

- xpdb:

  \<`xpose_data`[xpose::xpose_data](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  object

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  defining which properties to transform. Argument should be valid
  label.

- .problem:

  \<`numeric`\> Problem number to use. Uses all problem if not provided.

- .subprob:

  \<`numeric`\> Subproblem number to use. Uses the xpose default if not
  provided.

## Value

`xp_xtras` object

## Details

Although one might be tempted to set custom properties using this
function, with the intention to maintain cross-functionality with
`xpose`, users cannot set a non-existent property with this function.
When used internally, workarounds to this semi-limitation are used.

## Examples

``` r
set_prop(xpose::xpdb_ex_pk, descr = "New model description") %>%
  xpose::get_summary()
#> # A tibble: 48 × 5
#>    problem subprob descr               label       value                        
#>      <dbl>   <dbl> <chr>               <chr>       <chr>                        
#>  1       0       0 Run description     descr       New model description        
#>  2       0       0 Run directory       dir         data                         
#>  3       0       0 Run errors          errors      na                           
#>  4       0       0 ESAMPLE seed number esampleseed na                           
#>  5       0       0 Run file            file        run001.lst                   
#>  6       0       0 Number of ESAMPLE   nesample    na                           
#>  7       0       0 Reference model     ref         000                          
#>  8       0       0 Run number          run         run001                       
#>  9       0       0 Software            software    nonmem                       
#> 10       0       0 Run start time      timestart   Mon Oct 16 13:34:28 CEST 2017
#> # ℹ 38 more rows
```
