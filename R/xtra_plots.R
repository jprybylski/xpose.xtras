#' Wrapper around ggpairs
#'
#' @description
#' Following the `xpose` design pattern to derive <[`ggpairs`][GGally::ggpairs]> plots.
#'
#' Established `xplot_` are used to generate parts of the grid.
#'
#'
#' @return specified pair plot
#' @export
#'
#' @examples
#' c()
#'
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


#' @export
xp_xtra_theme <- function(base_on = NULL) {
  if (is.null(base_on)) base_on <- xpose::theme_xp_default()

  # New defaults
  new_defs <- rlang::list2(
    boxplot_fill = "grey45",
    boxplot_linewidth = base_on$density_linewidth,
    boxplot_linetype = base_on$density_linetype,
    boxplot_outlier.colour = base_on$point_color,
    boxplot_outlier.shape = base_on$point_shape,
    violin_fill = base_on$density_fill,
    violin_linewidth = base_on$density_linewidth,
    violin_linetype =  base_on$density_linetype,
    violin_alpha =  base_on$density_alpha,
    dotplot_stackdir = "center",
    dotplot_binpositions = "all",
    dotplot_dotsize = 0.8,
    dotplot_fill = base_on$histogram_fill,
    dotplot_linetype = base_on$area_linetype,
    dotplot_binwidth =NULL,
    hline_color = base_on$line_color,
    hline_linewidth = base_on$line_linewidth,
    hline_linetype = 2,
    hline_yintercept = 0,
    vline_color = base_on$line_color,
    vline_linewidth = base_on$line_linewidth,
    vline_linetype = 2,
    vline_xintercept = 0,
  )

  # May rarely have these xp_theme elements already defined for an xpose
  # object being based_on, so don't want to overwrite.
  already_covered <- names(new_defs) %in% names(base_on)

  modifyList(
    base_on,
    new_defs[!already_covered],
  ) %>%
    xpose::as.xpose.theme()
}
