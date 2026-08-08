# Level-defining helper functions

`as_leveler()` is the generic constructor behind every leveler: it tags
a character vector `x` so that
[`set_var_levels()`](https://jprybylski.github.io/xpose.xtras/reference/set_var_levels.md)
recognizes it as a set of levels rather than a plain formula list.
`x[1]` is mapped to the raw data value `.start_index`, `x[2]` to
`.start_index + 1`, and so on. `lvl_bin()`, `lvl_sex()`, and
`lvl_inord()` are thin wrappers around `as_leveler()` for common cases
(binary Yes/No, Male/Female sex, and pre-ordered factors, respectively);
call `as_leveler()` directly when defining your own levels, eg more than
two categories, a custom starting index, or an unordered custom label
set that the built-in wrappers don't cover.

## Usage

``` r
as_leveler(x, .start_index = 1, .ordered = FALSE)

is_leveler(x)

lvl_bin(x = c("No", "Yes"), .start_index = 0)

lvl_sex()

lvl_inord(x, .start_index = 1, .ordered = TRUE)
```

## Arguments

- x:

  \<`character`\> vector of levels

- .start_index:

  \<`numeric`\> starting index for levels

- .ordered:

  \<`logical`\> should these levels be treated as an ordered factor (see
  [`base::factor`](https://rdrr.io/r/base/factor.html)) wherever they're
  consumed (eg
  [`val2lvl()`](https://jprybylski.github.io/xpose.xtras/reference/val2lvl.md))?

## Value

Special character vector suitable to be used as leveler

## Examples

``` r

# Roll your own leveler for a case the convenience wrappers don't cover,
# eg an unordered, 3-category custom covariate coded 0/1/2 in the data
arm_levels <- as_leveler(c("Placebo", "Low dose", "High dose"), .start_index = 0)
arm_levels
#> [1] "Placebo"   "Low dose"  "High dose"
#> attr(,"class")
#> [1] "xp_levels" "character"
#> attr(,"start")
#> [1] 0
#> attr(,"ordered")
#> [1] FALSE
is_leveler(arm_levels)
#> [1] TRUE

# The convenience wrappers below are just as_leveler() under the hood, eg
# lvl_bin() is equivalent to as_leveler(c("No", "Yes"), .start_index = 0)
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
