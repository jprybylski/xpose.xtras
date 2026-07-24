# Standalone watermark layer, addressing issue #38. Implemented as an
# ordinary ggplot2 layer (ggplot2::annotation_custom()) rather than a
# post-hoc grid overlay, so the returned object stays a normal
# `ggplot`/`xpose_plot` -- composable with further `+` layers,
# apply_default_labs(), and ggsave_xp(). Because the layer's grob carries
# no facet variables, ggplot2 draws it once per panel, so faceted/paginated
# xpose plots get the watermark on every panel "for free".
#
# Settings resolve with the same precedence as apply_default_labs(): the
# `xpose.xtras.default_watermark` R option, then xpdb-level defaults (set
# via set_default_watermark(), read from an explicitly-supplied `xpdb` --
# see the comment in xtra_labs.R on why this can't be auto-discovered from
# `plot` alone), then arguments passed directly to add_watermark().

default_watermark_args <- c("label", "colour", "alpha", "size", "angle", "fontface")

#' Set default watermark options on an `xp_xtras` object
#'
#' @description
#' Stores default [add_watermark()] arguments on `xpdb` that
#' `add_watermark()` will use for any of `label`/`colour`/`alpha`/`size`/
#' `angle`/`fontface` not otherwise supplied, when `xpdb` is passed to it.
#'
#' @param xpdb <[`xpose_data`][xpose::xpose_data]> or <`xp_xtras`> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more of
#' `label`, `colour`, `alpha`, `size`, `angle`, `fontface`
#'
#' @return `xp_xtras` object
#' @seealso [set_xtras_options()] for the session-option equivalent
#' (`xpose.xtras.default_watermark`), and [get_xtras_option()] to check
#' which one is currently dominant.
#' @export
#'
#' @examples
#' xpdb_x <- set_default_watermark(xpdb_x, label = "PRELIMINARY", colour = "red")
#' p <- xpose::dv_vs_ipred(xpdb_x)
#' add_watermark(p, xpdb = xpdb_x)
set_default_watermark <- function(xpdb, ...) {
  xpose::check_xpdb(xpdb, check = FALSE)

  new_wm <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "error")
  checkmate::assert_list(new_wm, names = "unique")
  checkmate::assert_subset(names(new_wm), default_watermark_args)

  # set_option() merges list-valued options recursively -- see set_option()
  set_option(xpdb, default_watermark = new_wm)
}

#' Add a watermark to a plot
#'
#' @description
#' Overlays large, semi-transparent, rotated text across a `ggplot` or
#' `xpose_plot` object (e.g. `"DRAFT"`, `"PRELIMINARY"`, `"CONFIDENTIAL"`).
#' Purely opt-in: call it on a plot when you want the watermark, there is
#' no automatic/global trigger.
#'
#' `label`/`colour`/`alpha`/`size`/`angle`/`fontface` resolve with the same
#' precedence as [apply_default_labs()]: (in increasing precedence) the
#' `xpose.xtras.default_watermark` R option, `xpdb`-level defaults set via
#' [set_default_watermark()] (when `xpdb` is supplied), then the argument
#' itself when explicitly passed. Built-in fallbacks (`"DRAFT"`,
#' `"grey50"`, `0.3`, `24`, `30`, `"bold"`) apply if a setting isn't
#' resolved from any of those.
#'
#' @param plot <`ggplot`> or <`xpose_plot`> object
#' @param label <`character`> Watermark text
#' @param colour <`character`> Text colour, passed to [grDevices::adjustcolor()]
#' @param alpha <`numeric`> Transparency of the watermark text, between 0
#' (invisible) and 1 (opaque)
#' @param size <`numeric`> Font size in points
#' @param angle <`numeric`> Rotation angle in degrees (counter-clockwise)
#' @param fontface <`character`> Font face, passed to [grid::gpar()]
#' @param xpdb <[`xpose_data`][xpose::xpose_data]> or <`xp_xtras`> object
#' `plot` was built from, used to resolve `xpdb`-level defaults (see
#' [set_default_watermark()])
#'
#' @return `plot`, with the watermark layer added
#' @seealso [set_xtras_options()] for the full list of `xpose.xtras.*`
#' session options, and [get_xtras_option()] to check which tier
#' (option/`xpdb`) is currently dominant for a given `xpdb`.
#' @export
#'
#' @examples
#' p <- xpose::dv_vs_ipred(xpose::xpdb_ex_pk)
#' add_watermark(p, label = "PRELIMINARY")
#'
#' options(xpose.xtras.default_watermark = list(label = "CONFIDENTIAL", colour = "red"))
#' add_watermark(p)
#' options(xpose.xtras.default_watermark = NULL)
add_watermark <- function(plot, label, colour, alpha, size, angle, fontface, xpdb = NULL) {
  checkmate::assert_class(plot, "ggplot")
  if (!is.null(xpdb)) checkmate::assert_multi_class(xpdb, c("xpose_data", "xp_xtras"))

  resolved <- list(
    label = "DRAFT", colour = "grey50", alpha = 0.3,
    size = 24, angle = 30, fontface = "bold"
  )

  opt_wm <- getOption("xpose.xtras.default_watermark", default = list())
  checkmate::assert_list(opt_wm, names = "unique")
  checkmate::assert_subset(names(opt_wm), default_watermark_args)
  resolved <- utils::modifyList(resolved, opt_wm)

  xpdb_wm <- xpdb$options$default_watermark
  if (!is.null(xpdb_wm)) resolved <- utils::modifyList(resolved, xpdb_wm)

  if (!missing(label))    resolved$label    <- label
  if (!missing(colour))   resolved$colour   <- colour
  if (!missing(alpha))    resolved$alpha    <- alpha
  if (!missing(size))     resolved$size     <- size
  if (!missing(angle))    resolved$angle    <- angle
  if (!missing(fontface)) resolved$fontface <- fontface

  checkmate::assert_string(resolved$label)
  checkmate::assert_string(resolved$colour)
  checkmate::assert_number(resolved$alpha, lower = 0, upper = 1)
  checkmate::assert_number(resolved$size, lower = 0)
  checkmate::assert_number(resolved$angle)
  checkmate::assert_string(resolved$fontface)

  watermark_grob <- grid::textGrob(
    label = resolved$label,
    rot = resolved$angle,
    gp = grid::gpar(
      col = grDevices::adjustcolor(resolved$colour, alpha.f = resolved$alpha),
      fontsize = resolved$size,
      fontface = resolved$fontface
    )
  )

  plot + ggplot2::annotation_custom(
    watermark_grob,
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )
}
