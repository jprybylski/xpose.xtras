# Central registry of xpose.xtras.* session options (issue #37). Kept as a
# named vector so set_xtras_options()/get_xtras_option() can validate names
# against it; the roxygen page below is the actual (human-facing)
# reference and has to be kept in sync by hand when an option is added or
# changed.
#
# default_labs/default_watermark are two-tier: resolved per-use (by
# apply_default_labs()/add_watermark()) from either this option or an
# xpdb-level default of the *same name* under xpdb$options -- set via
# set_option(), which merges list-valued options recursively (see
# set_option()), so set_default_labs()/set_default_watermark() and direct
# set_option() calls behave identically. The rest (save_dir/width/height,
# gg_theme, xp_theme) are option-only: there's no separate xpdb-level
# tier to compare against, either because there's nothing on xpdb to
# compare with (ggsave_xp()'s dir/width/height) or because the option is
# applied once, at as_xpdb_x() conversion time, becoming indistinguishable
# from the xpdb's own gg_theme/xp_theme rather than a resolved-per-use
# default.

xtras_option_registry <- c(
  default_labs      = "default title/subtitle/caption/tag templates for apply_default_labs()",
  default_watermark = "default add_watermark() arguments",
  save_dir          = "default `dir` for ggsave_xp()",
  save_width        = "default `width` for ggsave_xp()",
  save_height       = "default `height` for ggsave_xp()",
  gg_theme          = "default ggplot2 theme applied by as_xpdb_x()",
  xp_theme          = "default xpose xp_theme applied by as_xpdb_x()"
)

xtras_two_tier_options <- c("default_labs", "default_watermark")

#' Set `xpose.xtras` session options
#'
#' @description
#' Convenience wrapper around base [options()] for the `xpose.xtras.*`
#' family of options recognized by this package. Prefixes are added
#' automatically and names are validated against the list below, so e.g.
#' `set_xtras_options(save_dir = "figures")` is equivalent to (but safer
#' against typos than) `options(xpose.xtras.save_dir = "figures")`.
#' Current values can be read back with regular [getOption()] (e.g.
#' `getOption("xpose.xtras.save_dir")`), or with [get_xtras_option()] for
#' `default_labs`/`default_watermark`, which also considers any
#' `xpdb`-level default.
#'
#' @details
#' Recognized options (all unset, i.e. `NULL`, by default):
#'
#' \describe{
#'   \item{`default_labs`}{Named list of default `title`/`subtitle`/
#'   `caption`/`tag` templates (may contain `@keyword` placeholders, see
#'   [xpose::parse_title()]). Used by [apply_default_labs()] (and by
#'   extension [ggsave_xp()]) as the lowest-precedence source for any
#'   label a plot doesn't already have -- xpdb-level defaults (see
#'   [set_default_labs()]) and arguments passed directly to
#'   `apply_default_labs()` both take precedence over this option.}
#'   \item{`default_watermark`}{Named list of default [add_watermark()]
#'   arguments (any of `label`/`colour`/`alpha`/`size`/`angle`/
#'   `fontface`). Used by `add_watermark()` as the lowest-precedence
#'   source -- xpdb-level defaults (see [set_default_watermark()]) and
#'   arguments passed directly to `add_watermark()` both take precedence
#'   over this option.}
#'   \item{`save_dir`, `save_width`, `save_height`}{Defaults for the
#'   `dir`/`width`/`height` arguments of [ggsave_xp()], used whenever
#'   those arguments aren't supplied explicitly.}
#'   \item{`gg_theme`, `xp_theme`}{Default `ggplot2` theme / `xpose`
#'   `xp_theme` (see [xpose::update_themes()]) applied to an `xpose_data`
#'   object the first time it's converted via [as_xpdb_x()],
#'   so a project-wide look can be set once per session instead of
#'   calling [xpose::update_themes()] on every `xpdb` individually. Has
#'   no effect on `xpdb`s that are already `xp_xtras` objects.}
#' }
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more of
#' `default_labs`, `default_watermark`, `save_dir`, `save_width`,
#' `save_height`, `gg_theme`, `xp_theme`, given as `name = value`
#'
#' @return the previous values of the options that were set, invisibly
#' (see [options()])
#' @export
#'
#' @examples
#' set_xtras_options(save_dir = "figures", default_watermark = list(label = "DRAFT"))
#' getOption("xpose.xtras.save_dir")
#' set_xtras_options(save_dir = NULL, default_watermark = NULL)
set_xtras_options <- function(...) {
  new_opts <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "error")
  checkmate::assert_list(new_opts, names = "unique")
  checkmate::assert_subset(names(new_opts), names(xtras_option_registry))

  names(new_opts) <- paste0("xpose.xtras.", names(new_opts))
  do.call(options, new_opts)
}

#' Inspect which `xpose.xtras` option value is dominant
#'
#' @description
#' For the two "two-tier" options -- `default_labs` and
#' `default_watermark`, which can be set both as a session-wide R option
#' (via [set_xtras_options()]) and per-`xpdb` (via [set_default_labs()] /
#' [set_default_watermark()]) -- reports the value at each tier and which
#' one is dominant, i.e. would currently be used by [apply_default_labs()]
#' / [add_watermark()] absent any argument passed directly to those
#' functions (which always takes precedence over both tiers).
#'
#' The other options recognized by [set_xtras_options()] (`save_dir`,
#' `save_width`, `save_height`, `gg_theme`, `xp_theme`) have no `xpdb`-level
#' tier to compare against (see the Details in [set_xtras_options()]), so
#' for those `xpdb` is always `NULL` and `dominant` is `"option"` or
#' `"neither"`.
#'
#' @param name <`character`> One of the option names recognized by
#' [set_xtras_options()]
#' @param xpdb <[`xpose_data`][xpose::xpose_data]> or <`xp_xtras`> object
#' to check for an `xpdb`-level value (optional; only consulted for
#' `default_labs`/`default_watermark`)
#'
#' @return a list with elements `option` (the [getOption()] value),
#' `xpdb` (the `xpdb`-level value, or `NULL`), and `dominant` (one of
#' `"option"`, `"xpdb"`, or `"neither"`)
#' @export
#'
#' @examples
#' options(xpose.xtras.default_labs = list(caption = "session default"))
#' xpdb_x2 <- set_default_labs(xpdb_x, caption = "model-specific")
#' get_xtras_option("default_labs", xpdb_x2)
#' options(xpose.xtras.default_labs = NULL)
get_xtras_option <- function(name, xpdb = NULL) {
  checkmate::assert_choice(name, names(xtras_option_registry))
  if (!is.null(xpdb)) checkmate::assert_multi_class(xpdb, c("xpose_data", "xp_xtras"))

  opt_val <- getOption(paste0("xpose.xtras.", name))
  xpdb_val <- if (!is.null(xpdb) && name %in% xtras_two_tier_options) xpdb$options[[name]] else NULL

  dominant <- if (!is.null(xpdb_val)) "xpdb" else if (!is.null(opt_val)) "option" else "neither"

  list(option = opt_val, xpdb = xpdb_val, dominant = dominant)
}
