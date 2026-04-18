# Backfill utility for descriptions

A slightly more generic approach to getting model descriptions.

## Usage

``` r
desc_from_comments(
  xpdb,
  start_check = ".*description",
  maxlines = 5,
  remove = paste0(start_check, ":\\s*"),
  extra_proc = c,
  collapse = " "
)
```

## Arguments

- xpdb:

  \<`xpose_data`\> or \<`xp_xtras`\> object

- start_check:

  Regular expression used to mark start of description. This is tested
  case-insensitively.

- maxlines:

  If the number of lines after description to the first code block is
  more than 1, this allows a limit.

- remove:

  By default, the start check and a colon, with optional whitespace. A
  regex.

- extra_proc:

  Any extra processing that might be desired prior to collapsing the
  description lines. This should be a vectorized function.

- collapse:

  Character to use when collapsing multiple lines.

## Value

The description-updated \<`xpose_data`) object

## See also

[`set_prop()`](https://jprybylski.github.io/xpose.xtras/reference/set_prop.md)

## Examples

``` r
# This has a description, but it's not visible by default
pheno_base
#> 
#> ── ~ xp_xtras object 
#> Model description: na
#> run6.lst overview: 
#>  - Software: nonmem 7.5.0 
#>  - Attached files (memory usage 496.2 Kb): 
#>    + obs tabs: $prob no.1: run6tab 
#>    + sim tabs: <none> 
#>    + output files: run6.cor, run6.cov, run6.ext, run6.grd, run6.phi, run6.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = ~/Step 3/Step 5/Step 6, quiet = FALSE, manual_import = NULL, cvtype = exact

# It can be added with the following
pheno_base %>%
  desc_from_comments()
#> 
#> ── ~ xp_xtras object 
#> Model description: Final structural version of the model: 1 CMT Block IIV on
#> CL+V Add Error
#> run6.lst overview: 
#>  - Software: nonmem 7.5.0 
#>  - Attached files (memory usage 496.4 Kb): 
#>    + obs tabs: $prob no.1: run6tab 
#>    + sim tabs: <none> 
#>    + output files: run6.cor, run6.cov, run6.ext, run6.grd, run6.phi, run6.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = ~/Step 3/Step 5/Step 6, quiet = FALSE, manual_import = NULL, cvtype = exact

# Extra processing for preference can also implemented
pheno_base %>%
  desc_from_comments(extra_proc = tolower)
#> 
#> ── ~ xp_xtras object 
#> Model description: final structural version of the model: 1 cmt block iiv on
#> cl+v add error
#> run6.lst overview: 
#>  - Software: nonmem 7.5.0 
#>  - Attached files (memory usage 496.4 Kb): 
#>    + obs tabs: $prob no.1: run6tab 
#>    + sim tabs: <none> 
#>    + output files: run6.cor, run6.cov, run6.ext, run6.grd, run6.phi, run6.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = ~/Step 3/Step 5/Step 6, quiet = FALSE, manual_import = NULL, cvtype = exact

# If a run label ($PROB) would make a good description, use the
# following instead:
pkpd_m3 %>%
  set_prop(descr=get_prop(pkpd_m3,"label"))
#> 
#> ── ~ xp_xtras object 
#> Model description: Simulated PKPD
#> run.lst overview: 
#>  - Software: nonmem 7.5.0 
#>  - Attached files (memory usage 1.6 Mb): 
#>    + obs tabs: $prob no.1: output.fit, pars.fit 
#>    + sim tabs: <none> 
#>    + output files: run.cor, run.cov, run.ext, run.grd, run.phi, run.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = ~/pkpd_m3, quiet = FALSE, manual_import = NULL, cvtype = exact

```
