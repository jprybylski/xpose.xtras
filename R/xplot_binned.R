#' Generic binned trend plotting function
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param mapping `ggplot2` style mapping. Expected to include at least `x`
#' and `y`.
#' @param group Column name distinguishing multiple summarized series (eg
#' a `variable` column). Points/lines/smooths are both connected and
#' coloured by this column (see [`xpose::xplot_scatter()`] for the same
#' connecting-line convention).
#' @param type String setting the type of plot to be used: point `p`, line
#' `l`, and smooth `s`, or any combination thereof.
#' @param xscale Defaults to `discrete`.
#' @param yscale Defaults to `continuous`.
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param plot_name Metadata name of plot
#' @param gg_theme As in `xpose`
#' @param xp_theme As in `xpose`
#' @param opt Processing options for fetched data
#' @param quiet Silence extra debugging output
#' @param ... Additional aesthetics, passed to the `point`, `line` and
#' `smooth` layers.
#'
#' @description
#' Following the `xpose` design pattern, this is a generic template
#' (analogous to [`xplot_boxplot()`] or [`xplot_pairs()`]) for rendering
#' already-summarized/binned data as connected points and/or lines across
#' a (typically discrete, possibly ordered) x variable. It is not tied to
#' any particular binning or summarization logic -- callers are expected
#' to shape their data (eg via `opt`'s `post_processing`) and build a named
#' implementation on top of it (eg [`catdv_vs_occ()`]) rather than calling
#' it directly for everyday use.
#'
#' @details
#' Unlike [`xpose::xplot_scatter()`]'s raw per-subject spaghetti plots,
#' `xplot_binned()` assumes the supplied data is already one row per
#' x/group combination (eg per occasion, per series) -- it does no binning,
#' aggregation, or tidying of its own.
#'
#' @returns The desired plot
#' @export
xplot_binned <- function(
    xpdb,
    mapping = NULL,
    group = "variable",
    type = "pl",
    xscale = "discrete",
    yscale = "continuous",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    tag = NULL,
    plot_name = "binned",
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
    rlang::abort("No data available for plotting. Please check the variable mapping and filering options.")
  }

  # Check type
  allow_types <- c("p", "l", "s")
  xpose::check_plot_type(type, allowed = allow_types)
  check_type <- purrr::map(allow_types, ~stringr::str_detect(type, stringr::fixed(.x, ignore_case = TRUE))) %>%
    setNames(allow_types)

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
  xp <- ggplot2::ggplot(data = data, mapping) + gg_theme

  # Colour each series by `group` (eg the "variable" column distinguishing
  # multiple summarized series). This has to be threaded through as a
  # `<name>_colour`-prefixed aes for each geom (rather than relying on the
  # colour inherited from the base plot mapping) because xp_geoms()/
  # xp_map() gives a fixed xp_theme default (eg `point_color`/`line_color`)
  # priority over an *inherited* aes -- only a same-named mapping entry
  # passed directly to that xp_geoms() call suppresses the fixed default.
  if (check_type$l) {
    xp <- xp + xpose::xp_geoms(mapping  = ggplot2::aes(line_colour = .data[[group]],
                                                        line_group  = .data[[group]]),
                               xp_theme = xpdb$xp_theme,
                               name     = "line",
                               ggfun    = "geom_line",
                               ...)
  }

  # Add smooth
  if (check_type$s) {
    xp <- xp + xpose::xp_geoms(mapping  = ggplot2::aes(smooth_colour = .data[[group]],
                                                        smooth_group  = .data[[group]]),
                               xp_theme = xpdb$xp_theme,
                               name     = "smooth",
                               ggfun    = "geom_smooth",
                               ...)
  }

  # Add points
  if (check_type$p) {
    xp <- xp + xpose::xp_geoms(mapping  = ggplot2::aes(point_colour = .data[[group]]),
                               xp_theme = xpdb$xp_theme,
                               name     = "point",
                               ggfun    = "geom_point",
                               ...)
  }

  # Define scales
  xp <- xp +
    xpose::xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = "xscale",
             ggfun    = paste0("scale_x_", xscale),
             ...) +
    xpose::xp_geoms(mapping  = mapping,
             xp_theme = xpdb$xp_theme,
             name     = "yscale",
             ggfun    = paste0("scale_y_", yscale),
             ...)

  # Define panels
  if (!is.null(list(...)[["facets"]])) {
    xp <- xp + xpose::xpose_panels(xp_theme = xpdb$xp_theme,
                            extra_args = list(...))
  }

  # Add labels
  xp <- xp + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)

  if (utils::packageVersion("ggplot2") >= "3.0.0") {
    xp <- xp + ggplot2::labs(tag = tag)
  }

  # Add metadata to plots
  xp$xpose <- list(fun      = plot_name,
                   summary  = xpdb$summary,
                   problem  = attr(data, "problem"),
                   subprob  = attr(data, "subprob"),
                   method   = attr(data, "method"),
                   quiet    = quiet,
                   xp_theme = xpdb$xp_theme[stringr::str_c(c("title", "subtitle",
                                                             "caption", "tag"), "_suffix")])

  # Output the plot
  xpose::as.xpose.plot(xp)
}
