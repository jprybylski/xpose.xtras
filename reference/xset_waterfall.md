# Waterfall plot

Generic function primarily used with wrappers targeting types of values
changed between two models.

## Usage

``` r
xset_waterfall(
  xpdb_s,
  ...,
  .inorder = FALSE,
  type = "bh",
  .cols = NULL,
  max_nind = 0.7,
  scale_diff = TRUE,
  show_n = TRUE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  plot_name = "waterfall",
  opt,
  facets = NULL,
  facet_scales = "free_x",
  .problem,
  .subprob,
  .method,
  quiet
)
```

## Arguments

- xpdb_s:

  \<`xpose_set`\> object

- ...:

  See
  \<[`two_set_dots`](https://jprybylski.github.io/xpose.xtras/reference/two_set_dots.md)\>

- .inorder:

  See
  \<[`two_set_dots`](https://jprybylski.github.io/xpose.xtras/reference/two_set_dots.md)\>

- type:

  See Details.

- .cols:

  \<`tidyselect`\> data columns to plot.

- max_nind:

  If less than 1, the percentile of absolute change values above which
  to plot. If above 1, the absolute number of subjects is included. To
  show all, use an extreme positive number like 9999.

- scale_diff:

  \<`logical`\> Scale change to the standard deviation of the model 1
  column values. Respects faceting.

- show_n:

  \<`logical`\> For faceting variables, show N per facet. *Not
  implemented*

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

- opt:

  User-specified data options. Only some of these will be used.

- facets:

  \<`character`\> Faceting variables

- facet_scales:

  \<`character`\> Forwarded to `facet_*(scales = facet_scales)`

- .problem:

  The problem to be used, by default returns the last one.

- .subprob:

  The subproblem to be used, by default returns the last one.

- .method:

  The estimation method to be used, by default returns the last one.

- quiet:

  Silence extra debugging output

## Value

The desired plot

## Details

For type-based customization of plots:

- `b` bar plot (from `geom_bar`)

- `h` hline at 0 (from `geom_hline`)

- `t` text of change value (from `geom_text`)
