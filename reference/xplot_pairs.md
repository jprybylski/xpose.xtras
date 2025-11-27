# Wrapper around ggpairs

Following the `xpose` design pattern to derive
\<[`ggpairs`](https://ggobi.github.io/ggally/reference/ggpairs.html)\>
plots.

Established `xplot_` are used to generate parts of the grid.

## Usage

``` r
xplot_pairs(
  xpdb,
  mapping = NULL,
  cont_opts = list(group = "ID", guide = FALSE, type = "ps"),
  dist_opts = list(guide = FALSE, type = "hr"),
  cat_opts = list(type = "bo", log = NULL),
  contcont_opts = list(other_fun = NULL, stars = FALSE, digits = reportable_digits(xpdb),
    title = "Pearson Corr"),
  catcont_opts = list(other_fun = NULL, stars = FALSE, digits = reportable_digits(xpdb),
    title = "Spearman rho"),
  catcat_opts = list(use_rho = TRUE),
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  tag = NULL,
  plot_name = "pairs",
  gg_theme,
  xp_theme,
  opt,
  quiet,
  progress = rlang::is_interactive() && quiet,
  switch = NULL,
  ...
)
```

## Arguments

- xpdb:

  \<`xp_xtras> or <`xpose_data\`\> object

- mapping:

  `ggplot2` style mapping

- cont_opts:

  List of options to pass to `xplot_scatter`. See Details

- dist_opts:

  List of options to pass to `xplot_distribution`. See Details

- cat_opts:

  List of options to pass to `xplot_boxplot`. See Details

- contcont_opts:

  List of options to pass to `ggally_cors`. See Details

- catcont_opts:

  List of options to pass to `ggally_statistic`. See Details

- catcat_opts:

  A list with `use_rho` `TRUE` or `FALSE`. If `TRUE` (default), then the
  Spearman rho is displayed, else the ggpairs default count is used.

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

  As in `xpose`. This does not work reliably when changed from the
  default.

- xp_theme:

  As in `xpose`

- opt:

  Processing options for fetched data

- quiet:

  Silence extra debugging output

- progress:

  Show a progress bar as the plot is generated?

- switch:

  Passed to `ggpairs`

- ...:

  Ignored

## Value

specified pair plot

## Details

There is only limited control over the underlying `ggpairs()` call given
the need to address abstractions in `GGally` and `xpose`. However, users
can modify key display features. For `scatter`, `distribution` and
`boxplots`, the `type` option is directly forwarded to the user. For
upper elements of the matrix, users can modify features of the text
displayed or supply some other function entirely (`other_fun`).

`_opts` lists are consumed with
\<[`modifyList`](https://rdrr.io/r/utils/modifyList.html)\> from the
default, so there is no need to declare preferences that align with the
default if updating a subset.
