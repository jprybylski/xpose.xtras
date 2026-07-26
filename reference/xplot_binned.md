# Generic binned trend plotting function

Following the `xpose` design pattern, this is a generic template
(analogous to
[`xplot_boxplot()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_boxplot.md)
or
[`xplot_pairs()`](https://jprybylski.github.io/xpose.xtras/reference/xplot_pairs.md))
for rendering already-summarized/binned data as connected points and/or
lines across a (typically discrete, possibly ordered) x variable. It is
not tied to any particular binning or summarization logic – callers are
expected to shape their data (eg via `opt`'s `post_processing`) and
build a named implementation on top of it (eg
[`catdv_vs_occ()`](https://jprybylski.github.io/xpose.xtras/reference/catdv_vs_occ.md))
rather than calling it directly for everyday use.

## Usage

``` r
xplot_binned(
  xpdb,
  mapping = NULL,
  group = "variable",
  type = "pl",
  xscale = "discrete",
  yscale = "continuous",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  plot_name = "binned",
  gg_theme,
  xp_theme,
  opt,
  quiet,
  ...
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> or \<`xpose_data`\> object

- mapping:

  `ggplot2` style mapping. Expected to include at least `x` and `y`.

- group:

  Column name distinguishing multiple summarized series (eg a `variable`
  column). Points/lines/smooths are both connected and coloured by this
  column (see
  [`xpose::xplot_scatter()`](https://uupharmacometrics.github.io/xpose/reference/xplot_scatter.html)
  for the same connecting-line convention).

- type:

  String setting the type of plot to be used: point `p`, line `l`, and
  smooth `s`, or any combination thereof.

- xscale:

  Defaults to `discrete`.

- yscale:

  Defaults to `continuous`.

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

  Processing options for fetched data

- quiet:

  Silence extra debugging output

- ...:

  Additional aesthetics, passed to the `point`, `line` and `smooth`
  layers.

## Value

The desired plot

## Details

Unlike
[`xpose::xplot_scatter()`](https://uupharmacometrics.github.io/xpose/reference/xplot_scatter.html)'s
raw per-subject spaghetti plots, `xplot_binned()` assumes the supplied
data is already one row per x/group combination (eg per occasion, per
series) – it does no binning, aggregation, or tidying of its own.
