# Bulk plot generation (issue #66): plot.xpose_data() runs a whole batch of
# diagnostic plots from one xpdb in a single call. The spec that drives it
# resolves the same layered way as default_labs/default_watermark (see
# xtras_options.R) -- a session option, then an xpdb-level default (see
# set_default_plots()), then whatever is passed directly -- except each
# tier replaces the spec wholesale rather than merging key by key (see
# resolve_plot_spec() below for why).

# Built-in default, used when `plots` isn't supplied and no default_plots
# is configured at either tier. Each entry is a one-sided formula in the
# `~ fn(.x)` idiom this package already uses for user-supplied functions
# (see focus_function()/purrr::as_mapper()) -- baking any non-default
# arguments straight into the formula is how the same underlying plot
# function can appear more than once with different options (see
# plot.xpose_data()'s examples).
default_plot_spec <- list(
  dv_vs_ipred      = ~ xpose::dv_vs_ipred(.x),
  dv_vs_pred       = ~ xpose::dv_vs_pred(.x),
  res_vs_idv       = ~ xpose::res_vs_idv(.x),
  res_vs_pred      = ~ xpose::res_vs_pred(.x),
  eta_distrib      = ~ xpose::eta_distrib(.x),
  eta_grid         = ~ eta_grid(.x),
  eta_vs_cov_grid  = ~ eta_vs_cov_grid(.x),
  ind_plots_sample = ~ ind_plots_sample(.x)
)

# Resolves the plot spec to use, in increasing precedence: package default,
# xpose.xtras.default_plots session option, xpdb-level default (see
# set_default_plots()), `plots` argument. Entries in a plot spec aren't
# addressable by a stable key -- the same function can legitimately appear
# more than once (e.g. res_vs_idv() for both CWRES and IWRES) -- so unlike
# default_labs/default_watermark there's no sensible key-by-key merge
# across tiers; whichever tier wins replaces the spec in full.
resolve_plot_spec <- function(xpdb, plots) {
  if (!missing(plots) && !is.null(plots)) return(plots)
  if (!is.null(xpdb$options$default_plots)) return(xpdb$options$default_plots)
  opt <- getOption("xpose.xtras.default_plots")
  if (!is.null(opt)) return(opt)
  default_plot_spec
}

# Derives a display name for each spec entry -- the list's own name if
# given, otherwise the head symbol of a formula's call (e.g. "eta_grid"
# from `~ eta_grid(.x)`), falling back to a positional placeholder -- then
# makes the whole set unique (vctrs::vec_as_names()), since two entries can
# legitimately share a function name (see above).
label_plot_spec <- function(spec) {
  nms <- rlang::names2(spec)
  # purrr::imap()'s index is names(x) (not position) whenever `spec` is
  # named, so this walks seq_along() directly instead to always get a
  # stable positional index into `nms`/`spec`.
  derived <- vapply(seq_along(spec), function(i) {
    if (nzchar(nms[[i]])) return(nms[[i]])
    entry <- spec[[i]]
    if (rlang::is_formula(entry) && rlang::is_call(rlang::f_rhs(entry))) {
      return(rlang::as_label(rlang::f_rhs(entry)[[1]]))
    }
    paste0("plot_", i)
  }, character(1))
  vctrs::vec_as_names(derived, repair = "unique", quiet = TRUE)
}

# Splices a single spec entry's result into the flat output: a single plot
# object becomes one named element, while a *plain list* of them (e.g.
# eta_vs_catcov()'s one-plot-per-eta output) is flattened in, one element
# per item -- so a spec mixing single-plot and list-returning functions
# still produces one flat list, never a list of lists (see #66). Plot
# objects (ggplot2's S7-based `ggplot`, GGally's S7-based `ggmatrix`, this
# package's xp_xtra_plot, ...) are never plain lists themselves --
# `is.list()` is FALSE for all of them -- so that alone distinguishes a
# single plot from a list of several without needing to enumerate every
# concrete plot class this package (or `xpose`/`GGally`) might return.
flatten_plot_result <- function(value, nm) {
  if (is.null(value)) {
    cli::cli_abort("Plot spec entry {.val {nm}} returned {.code NULL} instead of a plot.")
  }
  if (is.data.frame(value) || (is.atomic(value) && !is.list(value))) {
    cli::cli_abort(
      "Plot spec entry {.val {nm}} did not return a plot object (got class {.cls {class(value)}})."
    )
  }
  if (is.list(value)) {
    if (length(value) == 0) {
      cli::cli_abort("Plot spec entry {.val {nm}} returned an empty list.")
    }
    sub_nms <- rlang::names2(value)
    sub_nms <- ifelse(nzchar(sub_nms), paste(nm, sub_nms, sep = "_"), paste0(nm, "_", seq_along(value)))
    return(stats::setNames(value, sub_nms))
  }
  stats::setNames(list(value), nm)
}

#' Set a default plot spec on an `xp_xtras` object
#'
#' @description
#' Stores a plot spec on `xpdb` that [plot.xpose_data()][plot.xpose_data] uses instead of
#' the package's built-in default whenever `plots` isn't supplied directly
#' to that call. Unlike [set_default_labs()]/[set_default_watermark()],
#' this replaces the `xpdb`-level default wholesale rather than merging
#' with it: entries in a plot spec aren't addressable by a stable key (the
#' same plot function can legitimately appear more than once), so there is
#' no sensible key-by-key merge to perform.
#'
#' @param xpdb <[`xpose_data`][xpose::xpose_data]> or <`xp_xtras`> object
#' @param plots A list of plot specs -- see [plot.xpose_data()][plot.xpose_data] for the
#' accepted forms
#'
#' @return `xp_xtras` object
#' @seealso [set_xtras_options()] for the session-option equivalent
#' (`xpose.xtras.default_plots`), and [get_xtras_option()] to check which
#' one is currently dominant.
#' @export
#'
#' @examples
#' xpdb2 <- set_default_plots(xpdb_x, list(~ xpose::dv_vs_ipred(.x), ~ xpose::eta_distrib(.x)))
#' names(plot(xpdb2, quiet = TRUE))
set_default_plots <- function(xpdb, plots) {
  xpose::check_xpdb(xpdb, check = FALSE)
  checkmate::assert_list(plots, min.len = 1)

  # Bypasses set_option()'s utils::modifyList() merge -- that recurses into
  # list-valued options and merges *by name*, which would corrupt a plot
  # spec (often unnamed, sometimes duplicate-named) instead of replacing it
  xpdb$options$default_plots <- plots
  as_xpdb_x(xpdb)
}

#' Generate a batch of diagnostic plots from an `xpdb`
#'
#' @description
#' Runs a list of plot-generating calls against `x` in one go, returning a
#' flat, named list of the results. Each element of `plots` (or the
#' resolved default, see Details) is either:
#' - a bare function taking a single `xpdb` argument, or
#' - a one-sided formula in the `~ fn(.x)` idiom used elsewhere in this
#'   package (see [focus_function()]), letting extra arguments be baked
#'   straight into the call -- e.g. `~ xpose::res_vs_idv(.x, res = "CWRES")`.
#'
#' Both forms are converted to callables with [purrr::as_mapper()]. Because
#' entries are just calls, the same underlying plot function can appear
#' more than once with different options (e.g. `res_vs_idv` for both CWRES
#' and IWRES) -- see the examples.
#'
#' If a single entry itself returns a list of plots (as e.g. `eta_grid()`
#' can, given more than one eta), that list is flattened into the overall
#' output rather than kept as a nested list -- so the return value is
#' always a flat list of `ggplot`/`xpose_plot` objects.
#'
#' @details
#' `plots` is resolved, in increasing precedence, from: this package's
#' built-in default (`dv_vs_ipred`, `dv_vs_pred`, `res_vs_idv`,
#' `res_vs_pred`, `eta_distrib`, `eta_grid`, `eta_vs_cov_grid`,
#' `ind_plots_sample`), the `xpose.xtras.default_plots` session option, an
#' `xpdb`-level default set via [set_default_plots()], and finally the
#' `plots` argument itself, if supplied.
#'
#' A loading spinner is shown (interactive sessions only, unless
#' `quiet = TRUE`) while plots are generated. If a plot fails to generate,
#' the default (`force = FALSE`) is to immediately raise an error (showing
#' the original error as its parent) without returning any plots at all.
#' With `force = TRUE`, a failure is instead emitted as a warning and that
#' entry is skipped, so the rest of `plots` still gets a chance to run.
#'
#' @param x <[`xpose_data`][xpose::xpose_data]> or <`xp_xtras`> object
#' @param y unused; present only for consistency with the
#' [graphics::plot()] generic
#' @param plots A list of plot specs (see Description); defaults to the
#' resolved value described in Details
#' @param ... unused
#' @param force <`logical`> If `FALSE` (the default), a failing plot
#' immediately aborts the whole call. If `TRUE`, a failing plot is instead
#' skipped (with a warning), and the rest of `plots` is still attempted.
#' @param quiet <`logical`> Silence the loading spinner and the summary
#' warning issued when `force = TRUE` and at least one plot failed;
#' defaults to `x$options$quiet`
#'
#' @return A flat, named list of `ggplot`/`xpose_plot` objects
#' @method plot xpose_data
#' @export
#'
#' @examples
#' \donttest{
#' # the package's built-in default battery of diagnostic plots
#' default_plots <- plot(xpdb_x, quiet = TRUE)
#' names(default_plots)
#'
#' # a custom spec: bare functions, formulas, and the same function twice
#' custom_plots <- plot(
#'   xpdb_x,
#'   plots = list(
#'     xpose::dv_vs_ipred,
#'     ~ xpose::res_vs_idv(.x, res = "CWRES"),
#'     ~ xpose::res_vs_idv(.x, res = "IWRES")
#'   ),
#'   quiet = TRUE
#' )
#' names(custom_plots)
#' }
plot.xpose_data <- function(x, y, plots, ..., force = FALSE, quiet) {
  rlang::check_dots_empty()
  xpose::check_xpdb(x, check = FALSE)
  checkmate::assert_flag(force)
  if (missing(quiet)) quiet <- isTRUE(x$options$quiet)
  checkmate::assert_flag(quiet)

  spec <- resolve_plot_spec(x, plots)
  checkmate::assert_list(spec, min.len = 1)
  nms <- label_plot_spec(spec)
  n <- length(spec)

  for (i in seq_len(n)) {
    entry <- spec[[i]]
    if (!rlang::is_function(entry) && !rlang::is_formula(entry)) {
      cli::cli_abort(
        "Plot spec entry {i} ({.val {nms[[i]]}}) must be a function or a one-sided formula (e.g. `~ eta_grid(.x)`), not {.cls {class(entry)}}."
      )
    }
  }

  spinner_on <- !quiet && rlang::is_interactive()
  sp <- if (spinner_on) cli::make_spinner(default_spinner) else NULL

  out <- vector("list", n)
  ok <- rep(TRUE, n)
  for (i in seq_len(n)) {
    if (spinner_on) sp$spin()
    fn <- purrr::as_mapper(spec[[i]])
    out[[i]] <- rlang::try_fetch(
      fn(x),
      error = function(e) {
        if (!force) {
          if (spinner_on) sp$finish()
          cli::cli_abort(
            c("Failed to generate the {.val {nms[[i]]}} plot ({i} of {n}).",
              i = "Set `force = TRUE` to skip failing plots and continue with the rest."),
            parent = e
          )
        }
        cli::cli_warn(
          "Failed to generate the {.val {nms[[i]]}} plot ({i} of {n}); skipping.",
          parent = e
        )
        ok[i] <<- FALSE
        NULL
      }
    )
  }
  if (spinner_on) sp$finish()

  if (!any(ok)) cli::cli_abort("All {n} plot(s) failed to generate; see warnings above for details.")

  flattened <- purrr::flatten(purrr::map2(out[ok], nms[ok], flatten_plot_result))
  names(flattened) <- vctrs::vec_as_names(names(flattened), repair = "unique", quiet = TRUE)

  if (!quiet && any(!ok)) {
    cli::cli_alert_warning(
      "{sum(!ok)} of {n} plot(s) failed and {if (sum(!ok) == 1) 'was' else 'were'} skipped: {.val {nms[!ok]}}"
    )
  }

  flattened
}
