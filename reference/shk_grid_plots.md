# Shrinkage contribution grid plots

These mirror
[`eta_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)/[`eta_vs_cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md),
but for the per-individual shrinkage contribution diagnostic (`shk` type
columns, see
[`derive_shk()`](https://jprybylski.github.io/xpose.xtras/reference/derive_shk.md)/[`backfill_shk()`](https://jprybylski.github.io/xpose.xtras/reference/derive_shk.md))
instead of the etas themselves.

## Usage

``` r
shk_grid(
  xpdb,
  mapping = NULL,
  shkvar = NULL,
  drop_fixed = TRUE,
  title = "Shrinkage contribution correlations | @run",
  subtitle = "Based on @nind individuals",
  caption = "@dir",
  tag = NULL,
  pairs_opts,
  .problem,
  quiet,
  ...
)

shk_vs_cov_grid(
  xpdb,
  mapping = NULL,
  shkvar = NULL,
  cols = NULL,
  covvar = NULL,
  covtypes = c("cont", "cat"),
  show_n = TRUE,
  drop_fixed = TRUE,
  title = "Shrinkage contribution covariate correlations | @run",
  subtitle = "Based on @nind individuals",
  caption = "@dir",
  tag = NULL,
  shkcov = TRUE,
  pairs_opts,
  .problem,
  quiet,
  ...
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> or \<`xpose_data`\> object

- mapping:

  `ggplot2` style mapping

- shkvar:

  `tidyselect` for `shk` variables

- drop_fixed:

  As in `xpose`

- title:

  Plot title

- subtitle:

  Plot subtitle

- caption:

  Plot caption

- tag:

  Plot tag

- pairs_opts:

  List of arguments to pass to `_opts`. See
  \<[`xplot_pairs`](https://jprybylski.github.io/xpose.xtras/reference/xplot_pairs.md)\>

- .problem:

  Problem number

- quiet:

  Silence extra debugging output

- ...:

  Passed to `xplot_pairs`

- cols:

  `tidyselect` for covariates variables

- covvar:

  For `shk_vs_cov_grid` only: an alias for `cols` (matching the `covvar`
  argument of
  [`shk_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/shk_vs_contcov.md)/[`shk_vs_catcov()`](https://jprybylski.github.io/xpose.xtras/reference/shk_vs_catcov.md)).
  If supplied (non-`NULL`), takes precedence over `cols`.

- covtypes:

  Subset to specific covariate type?

- show_n:

  Count the number of `ID`s in each category

- shkcov:

  For `shk_vs_cov_grid`, `shk` are sorted after covariates to give an
  `x` orientation to covariate relationships.

## Value

`xp_tras_plot` object

## Examples

``` r
# \donttest{

xpdb_shk <- backfill_shk(xpdb_x)
shk_grid(xpdb_shk)
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID

shk_vs_cov_grid(xpdb_shk)
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID
#> Using data from $prob no.1
#> Removing duplicated rows based on: ID

# }
```
