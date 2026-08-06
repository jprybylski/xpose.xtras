# Convenience function for ingesting a bbr model into xpose and xpose.xtras

[bbr](https://metrumresearchgroup.github.io/bbr/) is a Metrum Research
Group package for managing NONMEM modeling workflows via `bbi`. It is
not distributed on CRAN (see `Additional_repositories` in this package's
`DESCRIPTION` for where to obtain it).

Reading a `bbr`-managed model into `xpose`/`xpose.xtras` normally
requires manually reconstructing the output file path from the model
object, e.g.


    xtras_data(
      file = file.path(bbr::get_output_dir(mod), paste0(bbr::get_model_id(mod), ".lst"))
    )

`xp_from_bbr()` wraps that pipeline.

## Usage

``` r
xp_from_bbr(.mod, ..., .use_bbr_descr = TRUE)
```

## Arguments

- .mod:

  \<`bbi_nonmem_model`\> A `bbr` NONMEM model object, e.g. as returned
  by
  [`bbr::read_model()`](https://metrumresearchgroup.github.io/bbr/reference/read_model.html).

- ...:

  Passed to
  [`xtras_data()`](https://jprybylski.github.io/xpose.xtras/reference/xtras_data.md)
  (and, in turn,
  [`xpose::xpose_data()`](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)).

- .use_bbr_descr:

  \<`logical`\> If `TRUE` (default) and `.mod` carries a `bbr`
  description, use it to set the `descr` property of the result (see
  [`set_prop()`](https://jprybylski.github.io/xpose.xtras/reference/set_prop.md)),
  taking precedence over any description parsed from the NONMEM output
  itself.

## Value

An \<`xp_xtras`\> object

## See also

[`bbr::read_model()`](https://metrumresearchgroup.github.io/bbr/reference/read_model.html),
[`xtras_data()`](https://jprybylski.github.io/xpose.xtras/reference/xtras_data.md)

## Examples

``` r
if (requireNamespace("bbr", quietly = TRUE)) {
  # Build a bbr-style model directory from the bundled pheno_saemimp example
  src_dir <- system.file("pheno_saemimp", package = "xpose.xtras")
  mod_dir <- tempfile("xp_from_bbr_ex")
  dir.create(mod_dir)
  file.copy(file.path(src_dir, "run18.mod"), file.path(mod_dir, "18.mod"))
  out_dir <- file.path(mod_dir, "18")
  dir.create(out_dir)
  out_files <- setdiff(list.files(src_dir, pattern = "^run18\\."), "run18.mod")
  for (f in out_files) {
    file.copy(
      file.path(src_dir, f),
      file.path(out_dir, paste0("18.", sub("^run18\\.", "", f)))
    )
  }
  # bbr considers a run finished once bbi has written this file
  writeLines("{}", file.path(out_dir, "bbi_config.json"))

  mod <- bbr::new_model(file.path(mod_dir, "18"), .description = "Phenobarbital SAEM model")
  print(xp_from_bbr(mod))
  unlink(mod_dir, recursive = TRUE)
}
#> Warning: No table files could be found.
#> 
#> ── ~ xp_xtras object 
#> Model description: Phenobarbital SAEM model
#> 18.lst overview: 
#>  - Software: nonmem 7.5.0 
#>  - Attached files (memory usage 348.5 Kb): 
#>    + obs tabs: <none> 
#>    + sim tabs: <none> 
#>    + output files: 18.cor, 18.cov, 18.ext, 18.phi, 18.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = /tmp/Rtmp1d6t3i/xp_from_bbr_ex1a7469200a74/18, quiet = TRUE, manual_import = NULL, cvtype = exact
```
