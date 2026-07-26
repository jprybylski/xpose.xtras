
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
#' when `orientation = "x"`. For the violin layer (`type` includes `"v"`),
#' also needs `violin_x`/`violin_y` (prefixed, since this layer's data --
#' from `violin_opt` -- has a different shape than the rest and can't
#' share the plain `x`/`y` mapping; see [`xpose::xp_geoms()`]'s
#' `{name}_{aes}` convention for per-layer aesthetic overrides).
#' @param type See Details.
#' @param region <`numeric(2)`> `c(low, high)` bounds for the shaded
#' "no relevant effect" region (`type` includes `"r"`), eg
#' `c(0.8, 1.25)` for a bioequivalence-style band. `NULL` (default) falls
#' back to `c(0.8, 1.25)` whenever `"r"` is requested; has no effect
#' otherwise.
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
#' @param opt Processing options for fetched data (one row per category).
#' @param violin_opt Processing options for the violin layer's data (one
#' row per draw), only used/required when `type` includes `"v"`. Fetched
#' separately from `opt` (a distinct `xpose::fetch_data()` call, noted via
#' `cli::cli_inform()` unless `quiet = TRUE`) because the two layers need
#' different data shapes.
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
#'   \item `v` violin/density (from `geom_violin`), showing the
#'   distribution behind an interval (eg simulation draws) -- requires
#'   `violin_opt` and a `violin_x`/`violin_y` mapping, see above
#'   \item `r` shaded reference region (from `geom_rect`) spanning
#'   `region` (default `c(0.8, 1.25)`), eg a bioequivalence-style
#'   "no relevant effect" band; drawn behind every other layer
#' }
#'
#' @returns The desired plot
#'
#' @export
xplot_forest <- function(xpdb,
                         mapping   = NULL,
                         type      = 'pi',
                         region    = NULL,
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
                         violin_opt,
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
  allow_types <- c('p','i','l','v','r')
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

  # Add shaded "no relevant effect" region (eg a bioequivalence-style 80-125%
  # band); drawn first so it sits behind every other layer. Needs its own
  # single-row synthetic data (a constant band, not data-driven), so -- like
  # the violin layer -- it doesn't fit xp_geoms()'s "extract a `{name}_{aes}`
  # override from the plot's own mapping" convention and is built directly.
  if (check_type$r) {
    if (is.null(region)) region <- c(0.8, 1.25)
    if (length(region)!=2 || region[1]>=region[2])
      cli::cli_abort("`region` must be a length-2 vector `c(low, high)` with `low < high`, not {region}.")
    rect_df <- if (orientation=='y') {
      tibble::tibble(xmin = region[1], xmax = region[2], ymin = -Inf, ymax = Inf)
    } else {
      tibble::tibble(ymin = region[1], ymax = region[2], xmin = -Inf, xmax = Inf)
    }
    xp <- xp + ggplot2::geom_rect(
      data = rect_df,
      mapping = ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]],
                             ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
      inherit.aes = FALSE,
      fill = xpdb$xp_theme$rect_fill,
      alpha = xpdb$xp_theme$rect_alpha
    )
  }

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

  # Add violin (density behind an interval; needs its own, differently-shaped
  # data -- one row per draw, not one row per category -- so it gets its own
  # `violin_opt`/re-fetch rather than reusing `opt`'s data)
  if (check_type$v) {
    if (missing(violin_opt) || is.null(violin_opt)) {
      cli::cli_abort(c(
        "`type` includes {.val v} (violin), which needs `violin_opt`.",
        "i" = "This is a separate {.fn xpose::data_opt}, for the raw per-draw data behind each interval -- a different shape than `opt`'s one-row-per-category data. See {.fn cov_forest} for how it builds one via `prm_cov(keep_draws = TRUE)`."
      ))
    }
    if (!quiet) cli::cli_inform("Re-fetching data for the violin layer (one row per draw, a different shape than the point/interval data).")
    violin_data <- xpose::fetch_data(xpdb, quiet = quiet, .problem = violin_opt$problem, .subprob = violin_opt$subprob,
                       .method = violin_opt$method, .source = violin_opt$source, simtab = violin_opt$simtab,
                       filter = violin_opt$filter, tidy = violin_opt$tidy, index_col = violin_opt$index_col,
                       value_col = violin_opt$value_col, post_processing = violin_opt$post_processing)
    xp <- xp + xpose::xp_geoms(mapping  = mapping,
                               xp_theme = xpdb$xp_theme,
                               name     = 'violin',
                               ggfun    = 'geom_violin',
                               violin_data = violin_data,
                               violin_orientation = orientation,
                               violin_inherit.aes = FALSE, # different data (per-draw, not per-category); must not inherit opt's xmin/xmax etc.
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
