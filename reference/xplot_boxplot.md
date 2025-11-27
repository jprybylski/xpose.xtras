# Default xpose boxplot function

Manually generate boxplots from an xpdb object.

## Usage

``` r
xplot_boxplot(
  xpdb,
  mapping = NULL,
  type = "bo",
  xscale = "discrete",
  yscale = "continuous",
  orientation = "x",
  group = "ID",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  plot_name = "boxplot",
  gg_theme,
  xp_theme,
  opt,
  quiet,
  jitter_seed,
  ...
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> or \<`xpose_data`\> object

- mapping:

  `ggplot2` style mapping

- type:

  See Details.

- xscale:

  Defaults to `discrete`.

- yscale:

  Defaults to `continuous`, used as check if `orientation` changed.

- orientation:

  Defaults to `x`

- group:

  Grouping for connecting lines through jitter

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

- jitter_seed:

  A numeric, optional seed to be used in jitters

- ...:

  Any additional aesthetics.

## Value

The desired plot

## Details

For type-based customization of plots:

- `b` box-whisker (using default quantiles)

- `p` points (from `geom_dotplot`)

- `v` violin (from `geom_violin`)

- `o` outliers (show outliers)

- `l` line through 0 (or as indicated in `hline_yintercept` or
  `yline_xintercept`)

- `s` smooth line (from `geom_smooth`)

- `j` jitter points (from `geom_jitter`)

- `c` connecting lines for jitter points (from `geom_path`)
