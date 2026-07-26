# Covariate effect forest plot

Visualizes the effect of covariates on structural parameters, as
declared with
[`add_cov_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_cov_association.md),
as a forest plot: one row per (parameter, covariate, evaluation point),
the point estimate and interval as a ratio to the parameter's typical
value, with a reference line at `1`.

This is the covariate-specific wrapper: it calls
[`prm_cov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md)
to compute the effect-size table and
[`xplot_forest()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_forest.md)
(a generic, forest-plot-agnostic renderer, see its own documentation) to
draw it.

## Usage

``` r
cov_forest(
  xpdb,
  ...,
  type = "pilr",
  region = NULL,
  show_ref = TRUE,
  log = TRUE,
  forest_opts = list(),
  title = "Covariate effects on model parameters | @run",
  subtitle = "Ratio to typical parameter value; reference line at 1",
  caption = "@dir",
  tag = NULL,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  quiet
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> object with covariate associations declared via
  [`add_cov_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_cov_association.md)

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Forwarded to
  [`prm_cov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md)
  – eg `param ~ covariate` selectors, `ci_method`, `probs`, `level`,
  `nsim`.

- type:

  Passed to
  [`xplot_forest()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_forest.md);
  defaults to `'pilr'` (point + interval + reference line + shaded
  reference region –
  [`xplot_forest()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_forest.md)'s
  own defaults omit the line and region, since those are `cov_forest()`-
  specific opinions, not generic ones). Including `"v"` adds a
  violin/density layer of the raw simulation draws behind each interval;
  this forces `prm_cov(keep_draws = TRUE)`, which in turn requires
  `ci_method = "simulation"` (the default) – pass `ci_method = "delta"`
  in `...` together with `type` containing `"v"` and it will error,
  since no draws exist for the delta method.

- region:

  \<`numeric(2)`\> `c(low, high)` bounds for the shaded reference region
  (`type` includes `"r"`, the default); `NULL` (default) falls back to
  `c(0.8, 1.25)`, a common bioequivalence-style "no relevant effect"
  band.

- show_ref:

  \<`logical`\> Include the reference row(s) (`effect`/
  `ci_low`/`ci_high` always `1`, by construction, for every reference
  covariate value/level)? Defaults to `TRUE`; set `FALSE` to drop them
  from the plot – they carry no information beyond what the reference
  line already shows, and cutting them can reduce clutter when there are
  many covariates.

- log:

  \<`logical`\> Log-scale the effect-ratio (x) axis? Defaults to `TRUE`.
  Unlike most of the package's `log` arguments (eg
  [`eta_vs_contcov()`](https://jprybylski.github.io/xpose.xtras/reference/eta_vs_contcov.md)'s),
  this is a plain boolean rather than an `"x"`/`"y"`/`NULL`
  axis-selector string – `cov_forest()`'s orientation isn't
  user-configurable, so the axis being logged is never ambiguous.

- forest_opts:

  \<`list`\> Extra named arguments forwarded to
  [`xplot_forest()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_forest.md)
  (eg theme overrides), the same way `pairs_opts` works for
  [`cov_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md)/[`eta_grid()`](https://jprybylski.github.io/xpose.xtras/reference/grid_plots.md).
  Rarely needed since the most common override, `type`, is already its
  own argument.

- title:

  Plot title

- subtitle:

  Plot subtitle

- caption:

  Plot caption

- tag:

  Plot tag

- .problem:

  \<`numeric`\> Problem number

- .subprob:

  \<`numeric`\> Subprob number

- .method:

  \<`numeric`\> Method

- quiet:

  Silence extra output

## Value

The desired plot

## See also

[`add_cov_association()`](https://jprybylski.github.io/xpose.xtras/reference/add_cov_association.md),
[`prm_cov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md),
[`xplot_forest()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_forest.md)

## Examples

``` r
# \donttest{

xpdb_x %>%
  add_cov_association(
    TVCL ~ power(CLCR, THETA7, ref = 64),
    TVCL ~ catshift(SEX, THETA4, ref = 1)
  ) %>%
  cov_forest()
#> Using data from $prob no.1

# }
```
