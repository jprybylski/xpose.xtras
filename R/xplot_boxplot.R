
#' Default xpose boxplot function
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param mapping `ggplot2` style mapping
#' @param type See Details.
#' @param xscale Defaults to `discrete`.
#' @param yscale Defaults to `continuous`, used as check if `orientation` changed.
#' @param orientation Defaults to `x`
#' @param group Grouping for connecting lines through jitter
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param plot_name Metadata name of plot
#' @param gg_theme As in `xpose`
#' @param xp_theme As in `xpose`
#' @param opt Processing options for fetched data
#' @param quiet Silence extra debugging output
#' @param jitter_seed A numeric, optional seed to be used in jitters
#' @param ... Any additional aesthetics.
#'
#' @description Manually generate boxplots from an xpdb object.
#'
#' @details
#' For type-based customization of plots:
#' \itemize{
#'   \item `b` box-whisker (using default quantiles)
#'   \item `p` points (from `geom_dotplot`)
#'   \item `v` violin (from `geom_violin`)
#'   \item `o` outliers (show outliers)
#'   \item `l` line through 0 (or as indicated in `hline_yintercept` or
#'   `yline_xintercept`)
#'   \item `s` smooth line (from `geom_smooth`)
#'   \item `j` jitter points (from `geom_jitter`)
#'   \item `c` connecting lines for jitter points  (from `geom_path`)
#' }
#'
#' @returns The desired plot
#'
#' @export
xplot_boxplot <- function(xpdb,
                          mapping   = NULL,
                          type      = 'bo',
                          xscale    = 'discrete',
                          yscale    = 'continuous',
                          orientation = "x",
                          group     = 'ID',
                          title     = NULL,
                          subtitle  = NULL,
                          caption   = NULL,
                          tag       = NULL,
                          plot_name = 'boxplot',
                          gg_theme,
                          xp_theme,
                          opt,
                          quiet,
                          jitter_seed,
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
  allow_types <-  c('b', "p","v","o","l","j","c","s")
  xpose::check_plot_type(type, allowed = allow_types)
  check_type <- purrr::map(allow_types, ~stringr::str_detect(type, stringr::fixed(.x, ignore_case = TRUE))) %>%
    setNames(allow_types)

  # Check orientation
  orientation <- rlang::arg_match(arg = orientation, values = c("x","y"))
  if (orientation == "y" && yscale != "discrete") {
    if (!quiet) cli::cli_warn(
      "orientation set to y but yscale is not discrete. This will be flipped"
    )
    xscale1 <- xscale
    xscale <- yscale
    yscale <- xscale1
  }

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
  xp <- ggplot2::ggplot(data = data, xpose::aes_filter(mapping, keep_only = c('x', 'y'))) + gg_theme

  # Add line
  if (check_type$l) {
    geom_hvline <- ifelse(orientation=="x", "geom_hline", "geom_vline")
    hvline_name <- ifelse(orientation=="x", "hline", "vline")
    xp <- xp + xpose::xp_geoms(mapping  = NULL,
                               xp_theme = xpdb$xp_theme,
                               name     = hvline_name,
                               ggfun    = geom_hvline,
                               ...)
  }

  # Add boxplot
  if (check_type$b) {
    if (utils::packageVersion("ggplot2") > "3.5.2") {
      xp <- xp + xpose::xp_geoms(mapping  = mapping,
                                 xp_theme = xpdb$xp_theme,
                                 name     = 'boxplot',
                                 ggfun    = 'geom_boxplot',
                                 boxplot_outliers = check_type$o && !check_type$j,
                                 ...)
    } else {
      xp <- xp + xpose::xp_geoms(mapping  = mapping,
                                 xp_theme = xpdb$xp_theme,
                                 name     = 'boxplot',
                                 ggfun    = 'geom_boxplot',
                                 boxplot_outlier.shape = ifelse(
                                   check_type$o && !check_type$j, xpdb$xp_theme$boxplot_outlier.shape,
                                   NA),
                                 ...)
    }
  }

  # Add violin
  if (check_type$v) {
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'violin',
                               ggfun    = 'geom_violin',
                               ...)
  }

  # Add dotplot
  if (check_type$p) {
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'dotplot',
                               ggfun    = 'geom_dotplot',
                               dotplot_binaxis =ifelse(orientation=="x", "y","x"),
                               ...)
  }

  # Add smooth line
  if (check_type$s) {
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'smooth',
                               ggfun    = 'geom_smooth',
                               ...)
  }

  # Add jitter
  if (missing(jitter_seed) || !is.numeric(jitter_seed)) jitter_seed <- sample(1:1000, 1)
  if (check_type$j) {
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'jitter',
                               ggfun    = 'geom_point',
                               jitter_position = ggplot2::position_jitter(seed = jitter_seed, width = 0.2),
                               ...)
  }

  # Add connecting lines for jitter
  if (check_type$c) {
    this_pos <- if (check_type$j) ggplot2::position_jitter(seed = jitter_seed, width = 0.2) else ggplot2::position_identity()
    xp <- xp + xpose::xp_geoms(mapping  = c(mapping, aes(line_group = .data[[group]])),
                               xp_theme = xpdb$xp_theme,
                               name     = 'line',
                               ggfun    = 'geom_path',
                               line_position = this_pos,
                               ...)
  }
  rm(jitter_seed) # <- don't want an element of randomness in plot_env

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
