# Print an xpose_data object

**\[experimental\]**

Bugfix for `xpose:::print.xpose_data`. That function's `Options:`
summary line builds `names(x$options)` and `unlist(x$options)`
separately and assumes they come out the same length – true only as long
as every entry of `xpdb$options` is itself a single value. Any entry
that's a multi-element list (eg `default_labs`/ `default_watermark` once
more than one key is set) unlists into more elements than there are
names, and
[`stringr::str_c()`](https://stringr.tidyverse.org/reference/str_c.html)'s
vectorized recycling then errors – so printing (or auto-printing) the
`xpdb` fails outright rather than just rendering oddly. This collapses
each option's value to a single string first, so the name/value pairing
always stays 1:1.

Everything else is an unmodified duplicate of the upstream function.

## Usage

``` r
# S3 method for class 'xpose_data'
print(x, ...)
```

## Arguments

- x:

  An `xpose_data` object.

- ...:

  Passed on to further methods (currently unused upstream too).

## Examples

``` r
xpdb_x %>%
  set_default_labs(title = "t", caption = "c") %>%
  print()
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
#>  - Options: dir = data, quiet = FALSE, manual_import = NULL, cvtype = exact, default_labs = t, c
```
