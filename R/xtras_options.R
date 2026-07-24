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
# set_option() calls behave identically. The rest (save_dir/width/height/
# fun, gg_theme, xp_theme) are option-only: there's no separate xpdb-level
# tier to compare against, either because there's nothing on xpdb to
# compare with (ggsave_xp()'s path/width/height/save_fun) or because the
# option is applied once, at as_xpdb_x() conversion time, becoming
# indistinguishable from the xpdb's own gg_theme/xp_theme rather than a
# resolved-per-use default.

xtras_option_registry <- c(
  auto_apply        = "whether print.xpose_plot()/ggsave_xp() auto-apply default_labs/default_watermark",
  default_labs      = "default title/subtitle/caption/tag templates for apply_default_labs()",
  default_watermark = "default add_watermark() arguments",
  save_dir          = "default `path` for ggsave_xp()",
  save_width        = "default `width` for ggsave_xp()",
  save_height       = "default `height` for ggsave_xp()",
  save_fun          = "default save function for ggsave_xp(), e.g. ggplot2::ggsave()",
  gg_theme          = "default ggplot2 theme applied by as_xpdb_x()",
  xp_theme          = "default xpose xp_theme applied by as_xpdb_x()"
)

xtras_two_tier_options <- c("default_labs", "default_watermark")

# Shared by print.xpose_plot() (R/fixes.R) and ggsave_xp() (R/xtra_labs.R).
#
# has_default_watermark() gates auto-watermarking on there being an actual
# configured default -- add_watermark() itself always has something to draw
# (built-in fallbacks like "DRAFT"), so unconditionally auto-calling it
# would stamp every single plot the first time this package is loaded,
# which is not what "auto-apply *your configured* defaults" should mean.
# apply_default_labs() needs no equivalent guard: it already no-ops when
# there's nothing configured to fill in.
has_default_watermark <- function(xpdb = NULL) {
  !is.null(getOption("xpose.xtras.default_watermark")) || !is.null(xpdb$options$default_watermark)
}

# auto_apply_defaults() is the print.xpose_plot() hook: gated entirely by
# the auto_apply option (print() has no argument to override it per-call),
# and -- since print() never has the plot's source xpdb -- only ever sees
# the option-level tier of default_labs/default_watermark, never an
# xpdb-level one (see the file-level comment in xtra_labs.R). ggsave_xp()
# has its own apply_labs/apply_watermark arguments instead, since it does
# receive an optional xpdb and per-call overrides make sense there.
auto_apply_defaults <- function(plot, xpdb = NULL) {
  if (!isTRUE(getOption("xpose.xtras.auto_apply", TRUE))) return(plot)

  plot <- apply_default_labs(plot, xpdb = xpdb)
  if (has_default_watermark(xpdb)) plot <- add_watermark(plot, xpdb = xpdb)

  plot
}

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
#' Recognized options (all unset, i.e. `NULL`, by default, except
#' `auto_apply` which defaults to `TRUE`):
#'
#' \describe{
#'   \item{`auto_apply`}{Whether [print.xpose_plot()][print.xpose_plot] and
#'   [ggsave_xp()] automatically apply configured `default_labs`/
#'   `default_watermark` (labels always; a watermark only if
#'   `default_watermark` is actually set at some tier -- there's no
#'   unprompted default watermark). Defaults to `TRUE`, but is a no-op
#'   until `default_labs`/`default_watermark` are themselves configured, so
#'   leaving it at its default has no visible effect on its own; set it to
#'   `FALSE` to opt out of the auto-apply behavior everywhere at once (or
#'   pass `apply_labs`/`apply_watermark` to a specific [ggsave_xp()] call
#'   to opt out just there). `print.xpose_plot()` only ever sees the
#'   session-wide option tier (not an `xpdb`-level one) for the same reason
#'   noted under `default_labs` below.}
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
#'   `path`/`width`/`height` arguments of [ggsave_xp()], used whenever
#'   those arguments aren't supplied explicitly.}
#'   \item{`save_fun`}{Default save function for [ggsave_xp()] (itself
#'   defaulting to [ggplot2::ggsave()] when this is unset), for swapping
#'   in a drop-in alternative such as `reportifyr::ggsave_with_metadata()`
#'   project-wide instead of passing `save_fun` to every [ggsave_xp()] call.}
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
#' `save_height`, `save_fun`, `gg_theme`, `xp_theme`, given as `name = value`
#'
#' @return the previous values of the options that were set, invisibly
#' (see [options()])
#' @seealso [get_xtras_option()] to inspect which tier is currently
#' dominant for a two-tier option; [apply_default_labs()],
#' [add_watermark()], [ggsave_xp()], and [as_xpdb_x()], which consume
#' these options.
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
