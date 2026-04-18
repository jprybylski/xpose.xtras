# Set variable levels

For variable types such as `catcov`, it can be convenient to define
levels. This function provides a straightforward means to do so,
consistent with `tidy` functions like
\<[`case_when`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)\>.

Several convenience functions are provided for common levels in
\<[`levelers`](https://jprybylski.github.io/xpose.xtras/reference/levelers.md)\>.

## Usage

``` r
set_var_levels(
  xpdb,
  .problem = NULL,
  ...,
  .missing = "Other",
  .handle_missing = c("quiet", "warn", "error")
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> object

- .problem:

  \<`numeric`\> Problem number to use. Uses the all problems if `NULL`

- ...:

  \<`list`\> of formulas or leveler functions, where the relevant
  variable is provided as the argument,

- .missing:

  \<`character`\> Value to use for missing levels

- .handle_missing:

  \<`character`\> How to handle missing levels: "quiet", "warn", or
  "error"

## Value

\<`xp_xtras`\> object with updated levels

## Examples

``` r
set_var_levels(xpdb_x,
  SEX = lvl_sex(),
  MED1 = lvl_bin(),
  MED2 = c(
    0 ~ "n",
    1 ~ "y"
  )
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
