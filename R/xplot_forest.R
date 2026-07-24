
#' Default xpose forest plot function
#'
#' @description
#' Manually generate a forest-style plot (point + interval per category)
#' from an xpdb object. This is a generic, low-level renderer in the same
#' spirit as [`xplot_boxplot()`]/[`xpose::xplot_scatter()`]: it has no built-in knowledge
#' of covariate associations, [`prm_cov()`], or any other specific data
#' source -- it just draws a point + interval (and, per `type`, a
#' reference guide line) from whatever `mapping`/pre-fetched `opt` data
#' it's given. See [cov_forest()] for the covariate-association-specific
#' wrapper that prepares that mapping from [`prm_cov()`] and calls this
#' function to render it.
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param mapping `ggplot2` style mapping. Expected aesthetics: `x`/`y`
#' (the point) and `xmin`/`xmax` (the interval), or the mirrored roles
#' when `orientation = "x"`.
#' @param type See Details.
#' @param orientation Defaults to `'y'` (categories on the y-axis, values
#' on the x-axis -- the conventional forest-plot layout).
#' @param xscale Defaults to `'continuous'`.
#' @param yscale Defaults to `'discrete'`.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param plot_name Metadata name of plot
#' @param gg_theme As in `xpose`
#' @param xp_theme As in `xpose`
#' @param opt Processing options for fetched data
#' @param quiet Silence extra debugging output
#' @param ... Any additional aesthetics, or overrides for the reference
#' line (eg `vline_xintercept = 1` for a ratio-style forest plot; defaults
#' to `0` like the rest of the package's guide lines, see
#' [`xp_xtra_theme()`]).
#'
#' @details
#' For type-based customization of plots:
#' \itemize{
#'   \item `p` point (from `geom_point`) -- the effect estimate
#'   \item `i` interval (from `geom_linerange`) -- the confidence/credible
#'   interval
#'   \item `l` reference line through the theme's `vline_xintercept`/
#'   `hline_yintercept` (`0` by default; a ratio-style forest plot will
#'   typically override this to `1`, see [`cov_forest()`])
#' }
#'
#' A violin/density layer (eg showing the simulation draws behind an
#' interval) is a natural future addition to this function but is not yet
#' implemented -- the data shape it needs (one row per draw, rather than
#' one row per category) is different enough from the point/interval
#' layers' shape that it needs its own design pass rather than being
#' bolted on.
#'
#' @returns The desired plot
#'
#' @export
xplot_forest <- function(xpdb,
                         mapping   = NULL,
                         type      = 'pi',
                         orientation = 'y',
                         xscale    = 'continuous',
                         yscale    = 'discrete',
                         title     = NULL,
                         subtitle  = NULL,
                         caption   = NULL,
                         tag       = NULL,
                         plot_name = 'forest',
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
  allow_types <- c('p','i','l')
  xpose::check_plot_type(type, allowed = allow_types)
  check_type <- purrr::map(allow_types, ~stringr::str_detect(type, stringr::fixed(.x, ignore_case = TRUE))) %>%
    setNames(allow_types)

  # Check orientation
  orientation <- rlang::arg_match(arg = orientation, values = c('x','y'))

  # Assign xp_theme
  if (!missing(xp_theme)) xpdb <- xpose::update_themes(xpdb = xpdb, xp_theme = xp_xtra_theme(xp_theme))

  # Update theme of non-xp_xtra object
  if (!is_xp_xtras(xpdb)) xpdb <- xpose::update_themes(xpdb = xpdb, xp_theme = xp_xtra_theme(xpdb$xp_theme))

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
  xp <- ggplot2::ggplot(data = data, xpose::aes_filter(mapping, keep_only = c('x', 'y', 'xmin', 'xmax', 'ymin', 'ymax'))) + gg_theme

  # Add reference line
  if (check_type$l) {
    geom_hvline <- ifelse(orientation=='y', 'geom_vline', 'geom_hline')
    hvline_name <- ifelse(orientation=='y', 'vline', 'hline')
    xp <- xp + xpose::xp_geoms(mapping  = NULL,
                               xp_theme = xpdb$xp_theme,
                               name     = hvline_name,
                               ggfun    = geom_hvline,
                               ...)
  }

  # Add interval
  if (check_type$i) {
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'linerange',
                               ggfun    = 'geom_linerange',
                               linerange_orientation = orientation,
                               ...)
  }

  # Add point
  if (check_type$p) {
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'point',
                               ggfun    = 'geom_point',
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

  # Output the plot
  xpose::as.xpose.plot(xp)
}
