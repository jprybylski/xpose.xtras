#' Wrapper around ggpairs
#'
#' @description
#' Following the `xpose` design pattern to derive <[`ggpairs`][GGally::ggpairs]> plots.
#'
#' Established `xplot_` are used to generate parts of the grid.
#'
#'
#' @return
#' @export
#'
#' @examples
xplot_pairs <- function(
  xpdb,
  mapping   = NULL,
  group     = 'ID',
  type      = 'pls',
  guide     = FALSE,
  title     = NULL,
  subtitle  = NULL,
  caption   = NULL,
  tag       = NULL,
  gg_theme,
  xp_theme,
  opt,
  quiet,
  ...
  ) {

}


xplot_boxplot <- function(
  xpdb
  mapping   = NULL,
  group     = 'ID',
  orientation = "bottom",
  guide     = FALSE,
  title     = NULL,
  subtitle  = NULL,
  caption   = NULL,
  tag       = NULL,
  plot_name = 'box_plot',
  gg_theme,
  xp_theme,
  opt,
  quiet,
  ...) {

}
