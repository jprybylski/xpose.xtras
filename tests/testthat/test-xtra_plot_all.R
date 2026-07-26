test_that("plot.xpose_data runs the built-in default spec and flattens/labels it", {
  res <- plot(xpdb_x, quiet = TRUE)

  expect_type(res, "list")
  expect_named(res, c(
    "dv_vs_ipred", "dv_vs_pred", "res_vs_idv", "res_vs_pred",
    "eta_distrib", "eta_grid", "eta_vs_cov_grid", "ind_plots_sample"
  ))
  expect_true(all(vapply(res, Negate(is.null), logical(1))))
})

test_that("plot() dispatches to plot.xpose_data() through the base plot() generic", {
  expect_identical(class(xpdb_x)[1], "xp_xtras")
  res <- plot(xpdb_x, quiet = TRUE)
  expect_identical(names(res), names(default_plot_spec))
})

test_that("plots accepts bare functions and formulas, including the same function twice", {
  res <- plot(
    xpdb_x,
    plots = list(
      xpose::dv_vs_ipred,
      ~ xpose::res_vs_idv(.x, res = "CWRES"),
      ~ xpose::res_vs_idv(.x, res = "IWRES")
    ),
    quiet = TRUE
  )

  expect_length(res, 3)
  # a bare function has no deparse-able name of its own -> positional fallback
  expect_identical(names(res)[1], "plot_1")
  # formulas derive a name from the call, deduped since both share it
  expect_true(all(grepl("^xpose::res_vs_idv", names(res)[2:3])))
  expect_false(identical(names(res)[2], names(res)[3]))
})

test_that("a spec entry's own list result is flattened into the overall output", {
  p1 <- xpose::dv_vs_ipred(xpdb_x, quiet = TRUE)
  p2 <- xpose::eta_distrib(xpdb_x, quiet = TRUE)

  res_named <- plot(xpdb_x, plots = list(mock = ~ list(a = p1, b = p2)), quiet = TRUE)
  expect_named(res_named, c("mock_a", "mock_b"))
  expect_identical(res_named$mock_a, p1)
  expect_identical(res_named$mock_b, p2)

  res_unnamed <- plot(xpdb_x, plots = list(mock = ~ list(p1, p2)), quiet = TRUE)
  expect_named(res_unnamed, c("mock_1", "mock_2"))
})

test_that("plot spec entries must be a function or a one-sided formula", {
  expect_error(
    plot(xpdb_x, plots = list("dv_vs_ipred"), quiet = TRUE),
    regexp = "function or a one-sided formula"
  )
  expect_error(
    plot(xpdb_x, plots = list(42), quiet = TRUE),
    regexp = "function or a one-sided formula"
  )
})

test_that("a spec entry that doesn't return a plot object errors clearly", {
  expect_error(plot(xpdb_x, plots = list(~1), quiet = TRUE), regexp = "did not return a plot object")
  expect_error(
    plot(xpdb_x, plots = list(~ data.frame(x = 1)), quiet = TRUE),
    regexp = "did not return a plot object"
  )
  expect_error(plot(xpdb_x, plots = list(~NULL), quiet = TRUE), regexp = "NULL.*instead of a plot")
  expect_error(plot(xpdb_x, plots = list(~ list()), quiet = TRUE), regexp = "returned an empty list")
})

test_that("force = FALSE (the default) aborts immediately on the first failing plot", {
  expect_error(
    plot(xpdb_x, plots = list(xpose::dv_vs_ipred, ~ stop("boom")), quiet = TRUE),
    regexp = "Failed to generate.*stop"
  )
  expect_error(
    plot(xpdb_x, plots = list(~ stop("boom")), quiet = TRUE),
    regexp = "boom",
    class = "rlang_error"
  )
})

test_that("force = TRUE skips failing plots (with a warning) and still returns the rest", {
  expect_warning(
    res <- plot(
      xpdb_x,
      plots = list(xpose::dv_vs_ipred, ~ stop("boom"), xpose::eta_distrib),
      force = TRUE, quiet = FALSE
    ),
    regexp = "Failed to generate"
  )
  expect_length(res, 2)
})

test_that("quiet = TRUE suppresses the failure summary alert but still returns partial results", {
  res <- suppressWarnings(
    plot(xpdb_x, plots = list(xpose::dv_vs_ipred, ~ stop("boom")), force = TRUE, quiet = TRUE)
  )
  expect_length(res, 1)

  # quiet = TRUE only silences *this function's own* summary alert -- any
  # message emitted by an individual plot function (e.g. dv_vs_ipred()'s own
  # "Using data from..." note) isn't ours to suppress unless the caller bakes
  # `quiet = TRUE` into that entry's own formula/call.
  msgs <- testthat::capture_messages(
    suppressWarnings(
      plot(xpdb_x, plots = list(xpose::dv_vs_ipred, ~ stop("boom")), force = TRUE, quiet = TRUE)
    )
  )
  expect_false(any(grepl("plot\\(s\\) failed", msgs)))
})

test_that("force = TRUE still aborts if every plot in the spec fails", {
  expect_error(
    suppressWarnings(
      plot(xpdb_x, plots = list(~ stop("a"), ~ stop("b")), force = TRUE, quiet = TRUE)
    ),
    regexp = "All 2 plot"
  )
})

test_that("a loading spinner runs (and doesn't error) in a mocked interactive session", {
  old_opts <- options(rlang_interactive = TRUE)
  on.exit(options(old_opts), add = TRUE)

  expect_no_error(
    utils::capture.output(
      res <- plot(xpdb_x, plots = list(xpose::dv_vs_ipred, xpose::eta_distrib), quiet = FALSE)
    )
  )
  expect_length(res, 2)
})

test_that("resolve_plot_spec() resolves plots > xpdb-level default_plots > option > built-in default", {
  old_opts <- options(xpose.xtras.default_plots = NULL)
  on.exit(options(old_opts), add = TRUE)

  # nothing set anywhere: package default
  expect_identical(resolve_plot_spec(xpdb_x), default_plot_spec)

  # option-level default
  option_spec <- list(~ xpose::dv_vs_ipred(.x))
  options(xpose.xtras.default_plots = option_spec)
  expect_identical(resolve_plot_spec(xpdb_x), option_spec)

  # xpdb-level default wins over the option
  xpdb_spec <- list(~ xpose::eta_distrib(.x))
  xpdb2 <- set_default_plots(xpdb_x, xpdb_spec)
  expect_identical(resolve_plot_spec(xpdb2), xpdb_spec)
  # the option is still visible for a plain xpdb without its own default
  expect_identical(resolve_plot_spec(xpdb_x), option_spec)

  # a `plots` argument wins over both
  direct_spec <- list(~ xpose::dv_vs_pred(.x))
  expect_identical(resolve_plot_spec(xpdb2, direct_spec), direct_spec)
})

test_that("plot() picks up an xpdb-level default_plots set via set_default_plots()", {
  xpdb2 <- set_default_plots(xpdb_x, list(~ xpose::dv_vs_ipred(.x), ~ xpose::eta_distrib(.x)))
  res <- plot(xpdb2, quiet = TRUE)
  expect_named(res, c("xpose::dv_vs_ipred", "xpose::eta_distrib"))
})

test_that("plot() picks up the xpose.xtras.default_plots session option when no xpdb-level default is set", {
  old_opts <- options(xpose.xtras.default_plots = list(~ xpose::dv_vs_pred(.x)))
  on.exit(options(old_opts), add = TRUE)

  res <- plot(xpdb_x, quiet = TRUE)
  expect_named(res, "xpose::dv_vs_pred")
})

test_that("set_default_plots() replaces the xpdb-level spec wholesale rather than merging", {
  xpdb1 <- set_default_plots(xpdb_x, list(~ xpose::dv_vs_ipred(.x)))
  expect_length(xpdb1$options$default_plots, 1)

  xpdb2 <- set_default_plots(xpdb1, list(~ xpose::dv_vs_pred(.x), ~ xpose::eta_distrib(.x)))
  # replaced wholesale: the first spec is gone entirely, not merged in
  expect_length(xpdb2$options$default_plots, 2)
  fn_names <- vapply(
    xpdb2$options$default_plots,
    function(f) rlang::as_label(rlang::f_rhs(f)[[1]]),
    character(1)
  )
  expect_identical(fn_names, c("xpose::dv_vs_pred", "xpose::eta_distrib"))
  expect_s3_class(xpdb2, "xp_xtras")
})

test_that("set_default_plots() validates its inputs", {
  expect_error(set_default_plots(xpdb_x, plots = list()), regexp = "length")
  expect_error(set_default_plots(xpdb_x, plots = "not a list"), regexp = "list")
  expect_error(
    set_default_plots("not an xpdb", plots = list(~ xpose::dv_vs_ipred(.x))),
    regexp = "xpdb"
  )
})

test_that("default_plots is a registered two-tier xtras option", {
  old_opts <- options(xpose.xtras.default_plots = NULL)
  on.exit(options(old_opts), add = TRUE)

  res0 <- get_xtras_option("default_plots", xpdb_x)
  expect_null(res0$option)
  expect_null(res0$xpdb)
  expect_identical(res0$dominant, "neither")

  set_xtras_options(default_plots = list(~ xpose::dv_vs_ipred(.x)))
  res1 <- get_xtras_option("default_plots", xpdb_x)
  expect_identical(res1$dominant, "option")

  xpdb2 <- set_default_plots(xpdb_x, list(~ xpose::eta_distrib(.x)))
  res2 <- get_xtras_option("default_plots", xpdb2)
  expect_identical(res2$dominant, "xpdb")

  set_xtras_options(default_plots = NULL)
})

test_that("plot.xpose_data() rejects unrecognized extra arguments", {
  expect_error(plot(xpdb_x, quiet = TRUE, bogus = "x"), regexp = "\\.\\.\\.")
})
