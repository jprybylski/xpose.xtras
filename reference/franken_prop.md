# Combine a property from all components of a `franken_xpdb`

Combine a property from all components of a `franken_xpdb`

## Usage

``` r
franken_prop(
  xpdb_f,
  xpdb_list,
  prop,
  problem = NULL,
  glue_cmd = function(x) glue::glue_collapse(x, ", ", last = " and "),
  indices = seq_along(xpdb_list)
)
```

## Arguments

- xpdb_f:

  A product of
  [`franken_xpdb`](https://jprybylski.github.io/xpose.xtras/reference/franken_xpdb.md)

- xpdb_list:

  List of the source `xpose_data` objects.

- prop:

  \<`character`\> of the property to combine

- problem:

  \<`numeric`\> If necessary to specify

- glue_cmd:

  Any special transformation to the properties, including how to
  collapse.

- indices:

  \<`numeric`\> Index values `1:length(xpdb_list)` to include in the
  property collapse.

## Value

Same as `xpdb_f` with new properties.

## Details

This function is meant to be called within
[`franken_xpdb`](https://jprybylski.github.io/xpose.xtras/reference/franken_xpdb.md).
It is expected to be ready to handle cases where, for example, multiple
props are being set in a pipe, or a problem-associated property is being
set while a problem=0 property is also being set.

This is a *low-level* function, so its use outside of internal functions
is not intended.

## Examples

``` r
# \donttest{
# This is designed to be called in a function environment which
# would provide something like the following:

xpdb_f <- franken_xpdb(pheno_base, pheno_final, .types="catcov")

xpdb_list <- list(pheno_base, pheno_final)

# The following would be inside the function
xpdb_f %>%
  franken_prop(xpdb_list, "run",
    glue_cmd = function(x) paste(x, collapse="+"))
#> 
#> ── ~ xp_xtras object 
#> Model description: na
#> run6.lst overview: 
#>  - Software: nonmem 7.5.0 
#>  - Attached files (memory usage 476.2 Kb): 
#>    + obs tabs: $prob no.1 (modified): na, run6tab 
#>    + sim tabs: <none> 
#>    + output files: run6.cor, run6.cov, run6.ext, run6.grd, run6.phi, run6.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = ~/Step 3/Step 5/Step 6, quiet = FALSE, manual_import = NULL, use_labelunit = TRUE, square_bracket = FALSE, cvtype = exact

# xpdb_f may have to be written to a few times if
# and problem-specific combinations are needed:

updated <- xpdb_f %>%
  franken_prop(xpdb_list, "run",
    glue_cmd = function(x) paste(x, collapse="+"))

# problem will also be available. Assume there's
# no reason to loop here, but that may be needed
problem <- 1
updated <- updated %>%
  franken_prop(xpdb_list, "ofv",  problem=problem,
    glue_cmd = function(x) paste(x, collapse="&"))
# }
```
