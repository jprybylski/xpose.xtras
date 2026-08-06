# Read model outputs directly into an `xp_xtras` object

Convenience wrapper equivalent to
`xpose::xpose_data(...) %>% as_xp_xtras()`.
[`xpose::xpose_data()`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)'s
`quiet` argument already controls its own informative messages, but not
warnings raised while parsing NONMEM tables – e.g. `readr` surfacing
every oddly formatted or `NaN`/`Inf` value it had to coerce, one warning
per table, which can drown out a warning that actually matters. `dplyr`
(\>= 1.1.2, already required by this package) batches any warnings
raised inside a single
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
call – which is how `xpose` reads each table – into one `rlang_warning`
per call rather than letting each one through individually.

By default, `xtras_data()` silences those (and only those) warnings;
pass `warn = TRUE` to see them as
[`xpose::xpose_data()`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)
would raise them. Warnings
[`xpose::xpose_data()`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)
raises directly – e.g. a table or output file it couldn't find at all –
are unaffected either way, since those indicate an actual problem rather
than value-level noise.

## Usage

``` r
xtras_data(..., warn = FALSE)
```

## Arguments

- ...:

  Passed to
  [`xpose::xpose_data()`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)

- warn:

  \<`logical`\> If `FALSE` (default), `rlang_warning`-class warnings
  raised while reading are silenced. If `TRUE`, all warnings are passed
  through unmodified.

## Value

An \<`xp_xtras`\> object

## See also

[`xpose::xpose_data()`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html),
[`as_xpdb_x()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtras.md)

## Examples

``` r
xtras_data(file = file.path(
  system.file("pheno_saemimp", package = "xpose.xtras"), "run18.lst"
))
#> 
#> ── ~ xp_xtras object 
#> Model description: na
#> run18.lst overview: 
#>  - Software: nonmem 7.5.0 
#>  - Attached files (memory usage 511.1 Kb): 
#>    + obs tabs: $prob no.1: run16tab 
#>    + sim tabs: <none> 
#>    + output files: run18.cor, run18.cov, run18.ext, run18.phi, run18.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = /home/runner/work/_temp/Library/xpose.xtras/pheno_saemimp, quiet = TRUE, manual_import = NULL, cvtype = exact
```
