# Regression tests for .onAttach()'s conflicted:: preference registration (#39).
#
# conflicted is an optional (Suggests) dependency: .onAttach() only talks to
# it if the user has it loaded (#72). When it *is* loaded, conflicted only
# resolves a name conflict among *currently attached* packages, and
# (re-)attaching conflicted resets what it has already learned. Since
# users can attach conflicted/xpose/xpose.xtras in any order, a single
# one-shot conflict_prefer_all() call in .onAttach() is not enough -- it must
# be redone whenever xpose or conflicted (re)attach. Each of these has to run
# in a fresh subprocess since attaching packages is session-global and would
# otherwise leak between test cases (and across the whole test run).

attach_order_is_safe <- function(order) {
  pkg_dir <- find.package("xpose.xtras")
  callr::r(
    function(pkg_dir, order) {
      attach_xtras <- function() {
        ok <- tryCatch({
          library(xpose.xtras)
          TRUE
        }, error = function(e) FALSE)
        if (!ok) pkgload::load_all(pkg_dir, quiet = TRUE, export_all = FALSE)
      }
      for (pkg in order) {
        if (identical(pkg, "xpose.xtras")) attach_xtras() else library(pkg, character.only = TRUE)
      }
      # Bare (unqualified) call, so conflicted actually has to resolve it --
      # `dplyr::filter()` would bypass the conflict entirely.
      filter(data.frame(x = 1:2), x == 1)
      invisible(TRUE)
    },
    args = list(pkg_dir = pkg_dir, order = order),
    libpath = .libPaths()
  )
}

orders <- list(
  "conflicted, dplyr, xpose, xpose.xtras" = c("conflicted", "dplyr", "xpose", "xpose.xtras"),
  "dplyr, xpose, xpose.xtras, conflicted" = c("dplyr", "xpose", "xpose.xtras", "conflicted"),
  "conflicted, dplyr, xpose.xtras, xpose" = c("conflicted", "dplyr", "xpose.xtras", "xpose"),
  "dplyr, xpose.xtras, xpose, conflicted" = c("dplyr", "xpose.xtras", "xpose", "conflicted")
)

for (order_name in names(orders)) {
  local({
    order <- orders[[order_name]]
    test_that(paste0("filter() resolves without error when attached as: ", order_name), {
      skip_if_not_installed("callr")
      skip_if_not_installed("pkgload")
      skip_if_not_installed("conflicted")
      skip_on_cran()

      expect_no_error(attach_order_is_safe(order))
    })
  })
}

# .onAttach() only runs automatically on package load/attach, which happens
# in a separate process for the subprocess-based tests above (out of covr's
# view). Call it directly in-session to cover its internal
# set_conflict_prefs() helper.
test_that(".onAttach() registers conflict preferences directly", {
  skip_if_not_installed("conflicted")
  testthat::local_mocked_bindings(has_conflicted = function() TRUE)
  expect_no_error(xpose.xtras:::.onAttach())
})

# conflicted is Suggests-only (#72): .onAttach() must not hard-require it,
# so it needs to behave when conflicted is unavailable/not loaded, without
# needing to actually uninstall conflicted to exercise that branch.
test_that(".onAttach() is a no-op towards conflicted when it isn't loaded", {
  testthat::local_mocked_bindings(has_conflicted = function() FALSE)
  expect_no_error(xpose.xtras:::.onAttach())
})

test_that("has_conflicted() reflects whether the conflicted namespace is loaded", {
  expect_equal(xpose.xtras:::has_conflicted(), isNamespaceLoaded("conflicted"))
})

# .onAttach()'s "xpose isn't attached" startup message (#72): the bugfix
# bullet only appears when there's a real, currently-active bugfix at stake.
# irep() isn't version-gated (xpose fixed it upstream around 0.5.0, then
# reverted that fix, so there's no known-safe version to defer to
# xpose::irep() for), so it's always listed regardless of xpose's version.
test_that("active_bugfixes() always lists irep(), regardless of the installed xpose version", {
  testthat::local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "xpose")) package_version("0.4.0") else utils::packageVersion(pkg, ...)
    },
    .package = "utils"
  )
  expect_equal(xpose.xtras:::active_bugfixes(), "irep")

  testthat::local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "xpose")) package_version("99.0.0") else utils::packageVersion(pkg, ...)
    },
    .package = "utils"
  )
  expect_equal(xpose.xtras:::active_bugfixes(), "irep")
})

test_that(".onAttach() reminds the user xpose isn't attached whenever it isn't, regardless of bugfix/conflicted state", {
  testthat::local_mocked_bindings(active_bugfixes = function() character(0))

  testthat::local_mocked_bindings(is_attached = function(x) FALSE, has_conflicted = function() FALSE)
  expect_message(xpose.xtras:::.onAttach(), "isn't attached", class = "packageStartupMessage")

  testthat::local_mocked_bindings(is_attached = function(x) FALSE, has_conflicted = function() TRUE)
  expect_message(xpose.xtras:::.onAttach(), "isn't attached", class = "packageStartupMessage")
})

test_that(".onAttach() stays silent once xpose is attached, regardless of bugfix/conflicted state", {
  testthat::local_mocked_bindings(active_bugfixes = function() "irep")
  testthat::local_mocked_bindings(is_attached = function(x) TRUE, has_conflicted = function() FALSE)
  expect_no_message(xpose.xtras:::.onAttach(), class = "packageStartupMessage")
})

test_that(".onAttach() names the affected bugfix only when one is active and conflicted isn't loaded", {
  testthat::local_mocked_bindings(is_attached = function(x) FALSE)

  testthat::local_mocked_bindings(active_bugfixes = function() "irep", has_conflicted = function() FALSE)
  expect_message(xpose.xtras:::.onAttach(), "irep", class = "packageStartupMessage")

  testthat::local_mocked_bindings(active_bugfixes = function() character(0), has_conflicted = function() FALSE)
  expect_false(any(grepl("irep", testthat::capture_messages(xpose.xtras:::.onAttach()))))

  testthat::local_mocked_bindings(active_bugfixes = function() "irep", has_conflicted = function() TRUE)
  expect_false(any(grepl("irep", testthat::capture_messages(xpose.xtras:::.onAttach()))))
})

# print.xpose_plot registration (#72): both xpose and xpose.xtras provide a
# print.xpose_plot, and if both declared it as a NAMESPACE-level S3 method,
# R would print a "Registered S3 method overwritten" startup message
# whenever both load in the same session, regardless of order. It's instead
# registered manually via register_print_xpose_plot() -- confirm that both
# stays true (no NAMESPACE declaration) and keeps working (still wins
# dispatch, even after something else claims the slot first).
test_that("print.xpose_plot is not declared as a NAMESPACE-level S3 method", {
  # registerS3method() (used by register_print_xpose_plot(), see below) also
  # records into asNamespace("xpose.xtras")'s own "S3methods" info table as a
  # side effect, so that table can't distinguish a manual registration from a
  # real NAMESPACE declaration -- check the NAMESPACE file itself instead,
  # which is the thing that actually triggers R's "declared in two
  # namespaces" collision message if it's present here (#72).
  namespace_file <- system.file("NAMESPACE", package = "xpose.xtras")
  skip_if(!nzchar(namespace_file), "xpose.xtras not installed (only loaded via load_all())")
  expect_false(any(grepl("^S3method\\(print,\\s*xpose_plot\\)", readLines(namespace_file))))
})

test_that("register_print_xpose_plot() re-asserts priority over any other print.xpose_plot registration", {
  original <- getS3method("print", "xpose_plot")
  on.exit(registerS3method("print", "xpose_plot", original, envir = asNamespace("xpose.xtras")), add = TRUE)

  other <- function(x, ...) "not ours"
  registerS3method("print", "xpose_plot", other, envir = asNamespace("xpose"))
  expect_identical(getS3method("print", "xpose_plot"), other)

  xpose.xtras:::register_print_xpose_plot()
  expect_identical(getS3method("print", "xpose_plot"), xpose.xtras:::print_xpose_plot_impl)
})
