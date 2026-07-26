# Default xpose forest plot function

Manually generate a forest-style plot (point + interval per category)
from an xpdb object. This is a generic, low-level renderer in the same
spirit as
[`xplot_boxplot()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_boxplot.md)/[`xpose::xplot_scatter()`](https://uupharmacometrics.github.io/xpose/reference/xplot_scatter.html):
it has no built-in knowledge of covariate associations,
[`prm_cov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md),
or any other specific data source – it just draws a point + interval
(and, per `type`, a reference guide line) from whatever
`mapping`/pre-fetched `opt` data it's given. See
[`cov_forest()`](https://jprybylski.github.io/xpose.xtras/reference/cov_forest.md)
for the covariate-association-specific wrapper that prepares that
mapping from
[`prm_cov()`](https://jprybylski.github.io/xpose.xtras/reference/prm_cov.md)
and calls this function to render it.

## Usage

``` r
xplot_forest(
  xpdb,
  mapping = NULL,
  type = "pi",
  region = NULL,
  orientation = "y",
  xscale = "continuous",
  yscale = "discrete",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  plot_name = "forest",
  gg_theme,
  xp_theme,
  opt,
  violin_opt,
  quiet,
  ...
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> or \<`xpose_data`\> object

- mapping:

  `ggplot2` style mapping. Expected aesthetics: `x`/`y` (the point) and
  `xmin`/`xmax` (the interval), or the mirrored roles when
  `orientation = "x"`. For the violin layer (`type` includes `"v"`),
  also needs `violin_x`/`violin_y` (prefixed, since this layer's data –
  from `violin_opt` – has a different shape than the rest and can't
  share the plain `x`/`y` mapping; see
  [`xpose::xp_geoms()`](https://uupharmacometrics.github.io/xpose/reference/xp_geoms.html)'s
  `{name}_{aes}` convention for per-layer aesthetic overrides).

- type:

  See Details.

- region:

  \<`numeric(2)`\> `c(low, high)` bounds for the shaded "no relevant
  effect" region (`type` includes `"r"`), eg `c(0.8, 1.25)` for a
  bioequivalence-style band. `NULL` (default) falls back to
  `c(0.8, 1.25)` whenever `"r"` is requested; has no effect otherwise.

- orientation:

  Defaults to `'y'` (categories on the y-axis, values on the x-axis –
  the conventional forest-plot layout).

- xscale:

  Defaults to `'continuous'`.

- yscale:

  Defaults to `'discrete'`.

- title:

  Plot title

- subtitle:

  Plot subtitle

- caption:

  Plot caption

- tag:

  Plot tag

- plot_name:

  Metadata name of plot

- gg_theme:

  As in `xpose`

- xp_theme:

  As in `xpose`

- opt:

  Processing options for fetched data (one row per category).

- violin_opt:

  Processing options for the violin layer's data (one row per draw),
  only used/required when `type` includes `"v"`. Fetched separately from
  `opt` (a distinct
  [`xpose::fetch_data()`](https://uupharmacometrics.github.io/xpose/reference/fetch_data.html)
  call, noted via
  [`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
  unless `quiet = TRUE`) because the two layers need different data
  shapes.

- quiet:

  Silence extra debugging output

- ...:

  Any additional aesthetics, or overrides for the reference line (eg
  `vline_xintercept = 1` for a ratio-style forest plot; defaults to `0`
  like the rest of the package's guide lines, see
  [`xp_xtra_theme()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtra_theme.md)).

## Value

The desired plot

## Details

For type-based customization of plots:

- `p` point (from `geom_point`) – the effect estimate

- `i` interval (from `geom_linerange`) – the confidence/credible
  interval

- `l` reference line through the theme's `vline_xintercept`/
  `hline_yintercept` (`0` by default; a ratio-style forest plot will
  typically override this to `1`, see
  [`cov_forest()`](https://jprybylski.github.io/xpose.xtras/reference/cov_forest.md))

- `v` violin/density (from `geom_violin`), showing the distribution
  behind an interval (eg simulation draws) – requires `violin_opt` and a
  `violin_x`/`violin_y` mapping, see above

- `r` shaded reference region (from `geom_rect`) spanning `region`
  (default `c(0.8, 1.25)`), eg a bioequivalence-style "no relevant
  effect" band; drawn behind every other layer
