# Default label overrides (issue #37). apply_default_labs() is a
# standalone function usable on its own, but is also invoked automatically
# by print.xpose_plot() (see auto_apply_defaults() in xtras_options.R) --
# note that hook is option-level only (see below), and print.xpose_plot()
# is itself slated for eventual removal (issue #36), at which point that
# particular auto-apply hook will need a new home.
#
# apply_default_labs() takes an explicit `xpdb` argument for the xpdb-level
# tier rather than trying to auto-discover it from `plot`. xpose plotting
# functions (xpose::dv_vs_ipred() and friends, including this package's own
# xplot_*() builders) attach only a *reduced* context object to the
# resulting xpose_plot (`plot$xpose`: fun/summary/problem/subprob/method/
# quiet/xp_theme, with xp_theme itself filtered down to just the
# `*_suffix` keys used by xpose::append_suffix()) -- the original xpdb,
# and any custom `options` on it (like the `default_labs` set by
# set_default_labs()), are not reachable from `plot` alone.

default_lab_types <- c("title", "subtitle", "caption", "tag")

# Shared by apply_default_labs() and ggsave_xp(): resolves the xpdb-like
# context xpose::parse_title() needs to expand @keyword placeholders,
# preferring an explicitly-supplied xpdb but falling back to the reduced
# context xpose attaches to xpose_plot objects (see the file-level comment
# above for why that reduced object -- not a full xpdb -- is all a
# rendered plot carries). `@plotfun` isn't a real xpdb$summary column --
# xpose's own functions (print.xpose_plot(), xpose::xpose_save()) inject it
# as an extra_key/extra_value pair from the reduced object's $fun field, so
# it's only resolvable there, never from a bare xpdb (which has no notion
# of "the function that built this plot").
resolve_keyword_ctx <- function(plot, xpdb) {
  if (!is.null(xpdb)) {
    list(xpdb = xpdb, problem = utils::tail(xpdb$summary$problem, 1), quiet = xpdb$options$quiet, fun = NULL)
  } else if (xpose::is.xpose.plot(plot)) {
    list(xpdb = plot$xpose, problem = plot$xpose$problem, quiet = plot$xpose$quiet, fun = plot$xpose$fun)
  } else NULL
}

# Expands @keyword placeholders in a single string, if `ctx` is available
# and the string actually contains any
resolve_keywords <- function(string, ctx) {
  if (is.null(ctx) || is.null(string) || !grepl("@", string)) return(string)
  args <- list(
    string = string,
    xpdb = ctx$xpdb, problem = ctx$problem, quiet = ctx$quiet,
    ignore_key = c("page", "lastpage")
  )
  if (!is.null(ctx$fun)) {
    args$extra_key <- "plotfun"
    args$extra_value <- ctx$fun
  }
  do.call(xpose::parse_title, args)
}

#' Set default plot label overrides on an `xp_xtras` object
#'
#' @description
#' Stores `title`/`subtitle`/`caption`/`tag` templates on `xpdb` that
#' [apply_default_labs()] will use to fill in (or overwrite) labels on any
#' plot built from this `xpdb`, when `xpdb` is passed to it. Values may
#' contain the same `@keyword` placeholders understood by
#' [xpose::parse_title()] (e.g. `"@nind"`, `"@nobs"`, `"@run"`), since
#' they are resolved the same way.
#'
#' @param xpdb <[`xpose_data`][xpose::xpose_data]> or <`xp_xtras`> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more of
#' `title`, `subtitle`, `caption`, `tag`, given as character strings
#'
#' @return `xp_xtras` object
#' @seealso [set_xtras_options()] for the session-option equivalent
#' (`xpose.xtras.default_labs`), and [get_xtras_option()] to check which
#' one is currently dominant.
#' @export
#'
#' @examples
#' xpdb_x <- set_default_labs(xpdb_x, caption = "@nobs observations in @nind individuals")
#' p <- xpose::dv_vs_ipred(xpdb_x)
#' apply_default_labs(p, xpdb = xpdb_x)
set_default_labs <- function(xpdb, ...) {
  xpose::check_xpdb(xpdb, check = FALSE)

  new_labs <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "error")
  checkmate::assert_list(new_labs, types = "character", names = "unique")
  checkmate::assert_subset(names(new_labs), default_lab_types)

  # set_option() merges list-valued options recursively, so this only
  # touches the label(s) named in `...`, leaving any others already set
  # on xpdb$options$default_labs untouched
  set_option(xpdb, default_labs = new_labs)
}

#' Apply default label overrides to a plot
#'
#' @description
#' Resolves `title`/`subtitle`/`caption`/`tag` labels for `plot` from (in
#' increasing precedence):
#' 1. the `xpose.xtras.default_labs` R option (a named list, see examples),
#' 2. defaults set on `xpdb` via [set_default_labs()], when `xpdb` is
#'    supplied (a rendered plot does not retain enough of its source
#'    `xpdb` to look this up automatically -- pass it explicitly),
#' 3. values passed directly via `...`.
#'
#' By default only labels not already set on the plot are filled in; set
#' `overwrite = TRUE` to replace existing labels too.
#' [print.xpose_plot()][print.xpose_plot] calls this automatically
#' whenever `xpose.xtras.auto_apply` is enabled (the default; see
#' [set_xtras_options()]), but `apply_default_labs()` itself is a
#' standalone function that doesn't depend on that print method existing,
#' so it works the same regardless of whether that method survives issue
#' #36's eventual removal.
#'
#' @param plot <`ggplot`> or <`xpose_plot`> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Direct overrides for
#' `title`/`subtitle`/`caption`/`tag`, taking precedence over both the
#' option- and `xpdb`-level defaults
#' @param xpdb <[`xpose_data`][xpose::xpose_data]> or <`xp_xtras`> object
#' `plot` was built from, used to resolve `xpdb`-level defaults (see
#' [set_default_labs()]) and to resolve `@keyword` placeholders in any of
#' the labels. If omitted and `plot` is an `xpose_plot`, `@keyword`
#' placeholders are still resolved (using the reduced context xpose
#' attaches to the plot) but `xpdb`-level defaults are not available
#' @param overwrite <`logical`> Replace labels already present on `plot`
#' (default `FALSE`, meaning only missing labels are filled in)
#'
#' @return `plot`, with resolved labels applied
#' @seealso [set_xtras_options()] for the full list of `xpose.xtras.*`
#' session options, and [get_xtras_option()] to check which tier
#' (option/`xpdb`) is currently dominant for a given `xpdb`.
#' @export
#'
#' @examples
#' options(xpose.xtras.default_labs = list(caption = "Draft -- do not distribute"))
#' p <- xpose::dv_vs_ipred(xpose::xpdb_ex_pk)
#' apply_default_labs(p)
#' options(xpose.xtras.default_labs = NULL)
apply_default_labs <- function(plot, ..., xpdb = NULL, overwrite = FALSE) {
  checkmate::assert_class(plot, "ggplot")
  checkmate::assert_flag(overwrite)
  if (!is.null(xpdb)) checkmate::assert_multi_class(xpdb, c("xpose_data", "xp_xtras"))

  direct <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "error")
  checkmate::assert_list(direct, types = "character", names = "unique")
  checkmate::assert_subset(names(direct), default_lab_types)

  opt_labs <- getOption("xpose.xtras.default_labs", default = list())
  checkmate::assert_list(opt_labs, types = "character")

  xpdb_labs <- xpdb$options$default_labs
  if (is.null(xpdb_labs)) xpdb_labs <- list()

  resolved <- utils::modifyList(utils::modifyList(opt_labs, xpdb_labs), direct)
  resolved <- resolved[names(resolved) %in% default_lab_types]
  if (length(resolved) == 0) return(plot)

  if (!overwrite) {
    current <- suppressMessages(ggplot2::get_labs(plot = plot))
    resolved <- resolved[vapply(names(resolved), function(nm) is.null(current[[nm]]), logical(1))]
  }
  if (length(resolved) == 0) return(plot)

  keyword_ctx <- resolve_keyword_ctx(plot, xpdb)
  resolved <- lapply(resolved, resolve_keywords, ctx = keyword_ctx)

  plot + do.call(ggplot2::labs, resolved)
}

#' Save a plot with `xpose.xtras` default output resolution
#'
#' @description
#' A `ggplot2::ggsave()`-compatible wrapper (defaulting to
#' [ggplot2::ggsave()] itself, but swappable via `save_fun` -- e.g. for
#' `reportifyr::ggsave_with_metadata()` or any other function sharing
#' `ggsave()`'s `plot`/`filename`/`path`/`width`/`height` signature).
#' Before saving: resolved labels are applied via [apply_default_labs()]
#' and a configured watermark (if any) via [add_watermark()] -- see
#' `apply_labs`/`apply_watermark`, both of which default to the
#' `xpose.xtras.auto_apply` option (`TRUE` unless changed), the same
#' switch [print.xpose_plot()][print.xpose_plot] uses, so a plot looks the
#' same whether it's viewed interactively or saved with `ggsave_xp()`.
#' `filename`/`path` have any `@keyword` placeholders expanded via
#' [xpose::parse_title()], the same way [xpose::xpose_save()] does
#' (independent of which `save_fun` is used, since most save functions
#' don't do this themselves); and `path`/`width`/`height`/`save_fun` fall
#' back to the `xpose.xtras.save_dir`, `xpose.xtras.save_width`,
#' `xpose.xtras.save_height`, and `xpose.xtras.save_fun` R options when not
#' supplied explicitly, so a project can set output defaults once (see
#' [set_xtras_options()]).
#'
#' @param plot <`ggplot`> or <`xpose_plot`> object
#' @param filename <`character`> File name, optionally with `@keyword`
#' placeholders (e.g. `"@run_@plotfun.pdf"`, see [xpose::parse_title()]).
#' `@plotfun` (the name of the function that built `plot`, e.g.
#' `"dv_vs_ipred"`) only resolves when `plot` is an `xpose_plot`, since
#' it isn't something a bare `xpdb` knows about
#' @param path <`character`> Directory to save in; falls back to the
#' `xpose.xtras.save_dir` R option
#' @param width,height <`numeric`> Plot size (in inches by default,
#' see `save_fun`'s own `units` argument if it has one); fall back to the
#' `xpose.xtras.save_width`/`xpose.xtras.save_height` R options
#' @param xpdb <[`xpose_data`][xpose::xpose_data]> or <`xp_xtras`> object
#' `plot` was built from, forwarded to [apply_default_labs()]/
#' [add_watermark()] (see their `xpdb` argument) and used to resolve
#' `@keyword` placeholders
#' @param apply_labs <`logical`> Apply [apply_default_labs()] to `plot`
#' before saving; defaults to the `xpose.xtras.auto_apply` option (`TRUE`
#' unless changed)
#' @param apply_watermark <`logical`> Apply [add_watermark()] to `plot`
#' before saving, if a `default_watermark` is configured at some tier (see
#' [add_watermark()]) -- otherwise a no-op regardless; defaults to the
#' `xpose.xtras.auto_apply` option (`TRUE` unless changed)
#' @param save_fun <`function`> The actual save function to call, e.g.
#' [ggplot2::ggsave()] (the default) or a drop-in alternative such as
#' `reportifyr::ggsave_with_metadata()`; falls back to the
#' `xpose.xtras.save_fun` R option, then [ggplot2::ggsave()]
#' @param ... Passed on to `save_fun` (e.g. `device`, `dpi`, `units`, `bg`)
#'
#' @return the result of `save_fun` (for the default [ggplot2::ggsave()],
#' the saved file path, invisibly)
#' @seealso [set_xtras_options()] for the full list of `xpose.xtras.*`
#' session options.
#' @export
#'
#' @examples
#' \dontrun{
#' options(xpose.xtras.save_dir = "figures", xpose.xtras.save_width = 8)
#' p <- xpose::dv_vs_ipred(xpose::xpdb_ex_pk)
#' ggsave_xp(p, filename = "dv_vs_ipred.png")
#' }
ggsave_xp <- function(plot = ggplot2::last_plot(),
                       filename,
                       path = getOption("xpose.xtras.save_dir"),
                       width = getOption("xpose.xtras.save_width", 7),
                       height = getOption("xpose.xtras.save_height", 6),
                       xpdb = NULL,
                       apply_labs = getOption("xpose.xtras.auto_apply", TRUE),
                       apply_watermark = getOption("xpose.xtras.auto_apply", TRUE),
                       save_fun = getOption("xpose.xtras.save_fun", ggplot2::ggsave),
                       ...) {
  checkmate::assert_class(plot, "ggplot")
  checkmate::assert_string(filename)
  checkmate::assert_flag(apply_labs)
  checkmate::assert_flag(apply_watermark)
  checkmate::assert_function(save_fun)

  if (apply_labs) plot <- apply_default_labs(plot, xpdb = xpdb)
  if (apply_watermark && has_default_watermark(xpdb)) plot <- add_watermark(plot, xpdb = xpdb)

  keyword_ctx <- resolve_keyword_ctx(plot, xpdb)
  filename <- resolve_keywords(filename, keyword_ctx)
  path <- resolve_keywords(path, keyword_ctx)

  save_fun(plot = plot, filename = filename, path = path, width = width, height = height, ...)
}
