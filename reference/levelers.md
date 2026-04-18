# Level-defining helper functions

Level-defining helper functions

## Usage

``` r
as_leveler(x, .start_index = 1)

is_leveler(x)

lvl_bin(x = c("No", "Yes"), .start_index = 0)

lvl_sex()

lvl_inord(x, .start_index = 1)
```

## Arguments

- x:

  \<`character`\> vector of levels

- .start_index:

  \<`numeric`\> starting index for levels

## Value

Special character vector suitable to be used as leveler

## Examples

``` r
set_var_levels(xpdb_x,
  SEX = lvl_sex(),
  MED1 = lvl_bin(),
  MED2 = lvl_inord(c("n","y"), .start_index = 0)
  )
#> 
#> ── ~ xp_xtras object 
#> Model description: NONMEM PK example for xpose
#> run001.lst overview: 
#>  - Software: nonmem 7.3.0 
#>  - Attached files (memory usage 1.6 Mb): 
#>    + obs tabs: $prob no.1: catab001.csv, cotab001, patab001, sdtab001 
#>    + sim tabs: $prob no.2: simtab001.zip 
#>    + output files: run001.cor, run001.cov, run001.ext, run001.grd, run001.phi, run001.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = data, quiet = FALSE, manual_import = NULL, cvtype = exact
```
