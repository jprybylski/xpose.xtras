
#' Default xpose boxplot function
#'
#' @param type <`character`> vector of elements to include in the plot. See [Details].
#'
#' @description Manually generate boxplots from an xpdb object.
#'
#' @details
#' For type-based customization of plots:
#' \itemize{
#'   \item `b` box-whisker (using default quantiles)
#'   \item `p` points (from geom_dotplot)
#'   \item `v` violin (from geom_violin)
#'   \item `o` outliers (show outliers)
#' }
#'
#'
#' @export
xplot_boxplot <- function(xpdb,
                          mapping   = NULL,
                          type      = 'bo',
                          xscale    = 'discrete',
                          yscale    = 'continuous',
                          title     = NULL,
                          subtitle  = NULL,
                          caption   = NULL,
                          tag       = NULL,
                          plot_name = 'boxplot',
                          intercept = NULL,
                          gg_theme,
                          xp_theme,
                          opt,
                          quiet,
                          ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = FALSE)
  if (missing(quiet)) quiet <- xpdb$options$quiet

  # Fetch data
  if (missing(opt)) opt <- xpose::data_opt()
  data <- xpose::fetch_data(xpdb, quiet = quiet, .problem = opt$problem, .subprob = opt$subprob,
                     .method = opt$method, .source = opt$source, simtab = opt$simtab,
                     filter = opt$filter, tidy = opt$tidy, index_col = opt$index_col,
                     value_col = opt$value_col, post_processing = opt$post_processing)
  if (is.null(data) || nrow(data) == 0) {
    rlang::abort('No data available for plotting. Please check the variable mapping and filering options.')
  }

  # Check type
  xpose::check_plot_type(type, allowed = c('b', "p","v","o"))

  # Assign xp_theme
  if (!missing(xp_theme)) xpdb <- xpose::update_themes(xpdb = xpdb, xp_theme = xp_theme)

  # Assign gg_theme
  if (missing(gg_theme)) {
    gg_theme <- xpdb$gg_theme
  } else {
    gg_theme <- xpose::update_themes(xpdb = xpdb, gg_theme = gg_theme)$gg_theme
  }
  if (is.function(gg_theme)) {
    gg_theme <- do.call(gg_theme, args = list())
  }

  # Create ggplot base
  xp <- ggplot2::ggplot(data = data, xpose::aes_filter(mapping, keep_only = c('x', 'y'))) + gg_theme

  # Add boxplot
  if (stringr::str_detect(type, stringr::fixed('b', ignore_case = TRUE))) {
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'boxplot', # TODO: add defaults to xp_xtas xp_theme
                               ggfun    = 'geom_boxplot',
                               boxplot_outliers = stringr::str_detect(type, stringr::fixed('o', ignore_case = TRUE)),
                               ...)
  }

  # Add violin
  if (stringr::str_detect(type, stringr::fixed('v', ignore_case = TRUE))) {
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'violin',
                               ggfun    = 'geom_violin',
                               ...)
  }

  # Add dotplot
  if (stringr::str_detect(type, stringr::fixed('p', ignore_case = TRUE))) {
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'dotplot',
                               ggfun    = 'geom_dotplot',
                               dotplot_binaxis ="y", # TODO: add defaults to xp_xtas xp_theme
                               dotplot_stackdir="center",
                               dotplot_binpositions="all",
                               ...)
  }

  # Define scales
  xp <- xp +
    xpose::xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'xscale',
             ggfun    = paste0('scale_x_', xscale),
             ...) +
    xpose::xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = 'yscale',
             ggfun    = paste0('scale_y_', yscale),
             ...)

  # Define panels
  if (!is.null(list(...)[['facets']])) {
    xp <- xp + xpose::xpose_panels(xp_theme = xpdb$xp_theme,
                            extra_args = list(...))
  }

  # Add labels
  xp <- xp + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)

  if (utils::packageVersion('ggplot2') >= '3.0.0') {
    xp <- xp + ggplot2::labs(tag = tag)
  }

  # Add metadata to plots
  xp$xpose <- list(fun      = plot_name,
                   summary  = xpdb$summary,
                   problem  = attr(data, 'problem'),
                   subprob  = attr(data, 'subprob'),
                   method   = attr(data, 'method'),
                   quiet    = quiet,
                   xp_theme = xpdb$xp_theme[stringr::str_c(c('title', 'subtitle',
                                                             'caption', 'tag'), '_suffix')])

  # Ouptut the plot
  xpose::as.xpose.plot(xp)
}
