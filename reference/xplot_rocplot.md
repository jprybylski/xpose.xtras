# Default xpose ROC plot function

Manually generate ROCs from an xpdb object.

## Usage

``` r
xplot_rocplot(
  xpdb,
  mapping = NULL,
  type = "c",
  guide = TRUE,
  xscale = "continuous",
  yscale = "continuous",
  group = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  plot_name = "xplot_rocplot",
  gg_theme,
  xp_theme,
  opt,
  quiet,
  thres_fixed = 0.5,
  like_col = NULL,
  obs_col = NULL,
  obs_target = NULL,
  auc_sprintf = "AUC: %.3f",
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

- guide:

  Should the guide (e.g. unity line) be displayed.

- xscale:

  Defaults to `continuous`.

- yscale:

  Defaults to `continuous`.

- group:

  Grouping for curves or points

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

- thres_fixed:

  Fixed threshold value for space

- like_col:

  Column for likelihood/probability value

- obs_col:

  Column for observed value

- obs_target:

  Target observed value for likelihood

- auc_sprintf:

  Format to apply to AUC label

- ...:

  Any additional aesthetics.

## Value

The desired plot

## Details

For type-based customization of plots:

- `c` ROC curve (using `geom_path`)

- `k` Key points on ROC curve (where on curve the threshold is
  `thres_fixed`) (using `geom_point`)

- `p` ROC space points (using `geom_point`)

- `t` ROC space text (using `geom_text`)

- `a` AUC in bottom right (using `geom_label`)
