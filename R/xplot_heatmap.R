#' Generic heatmap plotting function
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object. Used only for theming;
#' `mat` is plotted as-is.
#' @param mat A numeric matrix with row and column names. `NA` cells are
#' dropped (not plotted), which can be used to mask out redundant cells
#' (eg, the lower triangle of a symmetric matrix).
#' @param digits Number of significant digits to display in cell labels
#' @param limits Fill scale limits, as `c(low, high)`. Defaults to
#' `c(-1, 1) * max(abs(mat), na.rm = TRUE)`
#' @param midpoint Fill scale midpoint
#' @param legend_name Fill scale/legend title
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param plot_name Metadata name of plot
#' @param gg_theme As in `xpose`
#' @param xp_theme As in `xpose`
#' @param quiet Silence extra debugging output
#' @param ... Additional aesthetics, passed to the `heatmap` tile layer and
#' `heatmaptxt` label layer (see [`xp_xtra_theme()`])
#'
#' @description
#' Following the `xpose` design pattern, this is a generic template
#' (analogous to [`xplot_boxplot()`] or [`xplot_pairs()`]) for rendering a
#' numeric matrix as a themed tile heatmap with cell value labels. It is not
#' tied to any particular data source; callers are expected to build a named
#' implementation on top of it (eg, [`cormat()`] for parameter
#' correlation/covariance matrices) rather than calling it directly for
#' everyday use.
#'
#' @return The desired plot
#' @export
#'
#' @examples
#' m <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, dimnames = list(c("A", "B"), c("A", "B")))
#' xplot_heatmap(xpdb_x, m, quiet = TRUE)
xplot_heatmap <- function(
    xpdb,
    mat,
    digits = 3,
    limits = NULL,
    midpoint = 0,
    legend_name = "Value",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    tag = NULL,
    plot_name = "heatmap",
    gg_theme,
    xp_theme,
    quiet,
    ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = FALSE)
  checkmate::assert_matrix(mat, mode = "numeric")
  if (is.null(rownames(mat)) || is.null(colnames(mat))) {
    rlang::abort("`mat` must have row and column names.")
  }
  if (missing(quiet)) quiet <- xpdb$options$quiet

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

  data <- as.data.frame(as.table(mat), stringsAsFactors = FALSE) %>%
    purrr::set_names(c("PAR1", "PAR2", "value")) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      PAR1 = factor(PAR1, levels = rownames(mat)),
      PAR2 = factor(PAR2, levels = colnames(mat))
    )

  if (nrow(data) == 0) {
    rlang::abort("No data available for plotting. Please check the variable mapping and filering options.")
  }

  if (is.null(limits)) limits <- max(abs(data$value), na.rm = TRUE) * c(-1, 1)

  # Fall back to sensible defaults if xp_theme predates these keys (eg, a
  # bundled example object saved before this plot was added).
  theme_default <- function(x, default) if (is.null(x)) default else x

  xp <- ggplot2::ggplot(data, ggplot2::aes(x = PAR1, y = PAR2, fill = value)) +
    xpose::xp_geoms(
      mapping = NULL, xp_theme = xpdb$xp_theme, name = "heatmap",
      ggfun = "geom_tile", ...
    ) +
    ggplot2::scale_fill_gradient2(
      low = theme_default(xpdb$xp_theme$heatmapfill_low, "steelblue"),
      mid = theme_default(xpdb$xp_theme$heatmapfill_mid, "white"),
      high = theme_default(xpdb$xp_theme$heatmapfill_high, "firebrick"),
      midpoint = midpoint,
      limits = limits,
      name = legend_name
    ) +
    xpose::xp_geoms(
      mapping = ggplot2::aes(heatmaptxt_label = signif(value, digits)),
      xp_theme = xpdb$xp_theme, name = "heatmaptxt", ggfun = "geom_text", ...
    ) +
    ggplot2::scale_x_discrete(drop = FALSE, guide = ggplot2::guide_axis(n.dodge = 2)) +
    ggplot2::scale_y_discrete(limits = rev, drop = FALSE) +
    ggplot2::coord_fixed() +
    gg_theme

  # Add labels
  xp <- xp + ggplot2::labs(title = title, subtitle = subtitle, caption = caption, x = NULL, y = NULL)

  if (utils::packageVersion("ggplot2") >= "3.0.0") {
    xp <- xp + ggplot2::labs(tag = tag)
  }

  # Add metadata to plots
  xp$xpose <- list(
    fun = plot_name,
    summary = xpdb$summary,
    problem = NULL,
    subprob = NULL,
    method = NULL,
    quiet = quiet,
    xp_theme = xpdb$xp_theme[stringr::str_c(c(
      "title", "subtitle",
      "caption", "tag"
    ), "_suffix")]
  )

  # Output the plot
  xpose::as.xpose.plot(xp)
}
