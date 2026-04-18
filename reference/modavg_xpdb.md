# Create a model-averaged xpose data object

**\[experimental\]**

This function is a helper for plotting functions where models in an
`xpose_set` can be averaged together. The implementation attempts to
match and extend from the cited prior work.

## Usage

``` r
modavg_xpdb(
  xpdb_s,
  ...,
  .lineage = FALSE,
  avg_cols = NULL,
  avg_by_type = NULL,
  algorithm = c("maa", "msa"),
  weight_type = c("individual", "population"),
  auto_backfill = FALSE,
  weight_basis = c("ofv", "aic", "res"),
  res_col = "RES",
  quiet
)
```

## Arguments

- xpdb_s:

  \<`xpose_set`\> object

- ...:

  \<`tidyselect`\> of models in set. If empty, all models are used in
  order of their position in the set. May also use a formula, which will
  just be processed with
  [`all.vars()`](https://rdrr.io/r/base/allnames.html).

- .lineage:

  \<`logical`\> where if `TRUE`, `...` is processed

- avg_cols:

  \<`tidyselect`\> columns in data to average

- avg_by_type:

  \<`character`\> Mainly for use in wrapper functions. Column type to
  average, but resulting column names must be valid for `avg_cols` (ie,
  same across all objects in the set). `avg_cols` will be overwritten.

- algorithm:

  \<`character`\> Model selection or model averaging

- weight_type:

  \<`character`\> Individual-level averaging or by full dataset.

- auto_backfill:

  \<`logical`\> If true,
  \<[`backfill_iofv`](https://jprybylski.github.io/xpose.xtras/reference/backfill_iofv.md)\>
  is automatically applied.

- weight_basis:

  \<`character`\> Weigh by OFV (default), AIC or residual.

- res_col:

  \<`character`\> Column to weight by if `"res"` weight basis.

- quiet:

  \<`logical`\> Minimize extra output.

## Value

Weight-averaged \<`xpose_data`\> object.

## References

Uster, D.W., Stocker, S.L., Carland, J.E., Brett, J., Marriott, D.J.E.,
Day, R.O. and Wicha, S.G. (2021), A Model Averaging/Selection Approach
Improves the Predictive Performance of Model-Informed Precision Dosing:
Vancomycin as a Case Study. Clin. Pharmacol. Ther., 109: 175-183.
[doi:10.1002/cpt.2065](https://doi.org/10.1002/cpt.2065)

## Examples

``` r
pheno_set %>%
  modavg_xpdb(
    avg_cols = IPRED,
    auto_backfill = TRUE,
    algorithm = "maa",
    weight_basis = "aic"
  )
#> 
#> ── ~ xp_xtras object 
#> Model description: na
#> run3.lst overview: 
#>  - Software: nonmem 7.5.0 
#>  - Attached files (memory usage 844.1 Kb): 
#>    + obs tabs: $prob no.1 (modified): na, run3tab 
#>    + sim tabs: <none> 
#>    + output files: run3.cor, run3.cov, run3.ext, run3.grd, run3.phi, run3.shk 
#>    + special: <none> 
#>  - gg_theme: theme_readable 
#>  - xp_theme: xp_xtra_theme new_x$xp_theme 
#>  - Options: dir = ~/Step 3, quiet = FALSE, manual_import = NULL, cvtype = exact
```
