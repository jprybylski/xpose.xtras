# Generic heatmap plotting function

Following the `xpose` design pattern, this is a generic template
(analogous to
[`xplot_boxplot()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_boxplot.md)
or
[`xplot_pairs()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_pairs.md))
for rendering a numeric matrix as a themed tile heatmap with cell value
labels. It is not tied to any particular data source; callers are
expected to build a named implementation on top of it (eg,
[`cormat()`](https://jprybylski.github.io/xpose.xtras/reference/cormat.md)
for parameter correlation/covariance matrices) rather than calling it
directly for everyday use.

## Usage

``` r
xplot_heatmap(
  xpdb,
  mat,
  digits = 3,
  limits = NULL,
  midpoint = 0,
  legend_name = "Value",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  plot_name = "heatmap",
  gg_theme,
  xp_theme,
  quiet,
  ...
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> or \<`xpose_data`\> object. Used only for theming;
  `mat` is plotted as-is.

- mat:

  A numeric matrix with row and column names. `NA` cells are dropped
  (not plotted), which can be used to mask out redundant cells (eg, the
  lower triangle of a symmetric matrix).

- digits:

  Number of significant digits to display in cell labels

- limits:

  Fill scale limits, as `c(low, high)`. Defaults to
  `c(-1, 1) * max(abs(mat), na.rm = TRUE)`

- midpoint:

  Fill scale midpoint

- legend_name:

  Fill scale/legend title

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

- quiet:

  Silence extra debugging output

- ...:

  Additional aesthetics, passed to the `heatmap` tile layer and
  `heatmaptxt` label layer (see
  [`xp_xtra_theme()`](https://jprybylski.github.io/xpose.xtras/reference/xp_xtra_theme.md))

## Value

The desired plot

## Examples

``` r
m <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, dimnames = list(c("A", "B"), c("A", "B")))
xplot_heatmap(xpdb_x, m, quiet = TRUE)
```
