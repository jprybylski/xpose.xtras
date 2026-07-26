# Regression tests for .onAttach()'s conflicted:: preference registration (#39).
#
# conflicted only resolves a name conflict among *currently attached* packages,
# and (re-)attaching conflicted resets what it has already learned. Since
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
      skip_on_cran()

      expect_no_error(attach_order_is_safe(order))
    })
  })
}

# .onAttach() only runs automatically on package load/attach, which happens
# in a separate process for the subprocess-based tests above (out of covr's
# view). Call it directly in-session to cover its internal
# set_conflict_prefs() helper, including the irep() conflict preference that
# only applies for xpose >= 0.5.0 (mocked here since the installed xpose may
# be older).
test_that(".onAttach() registers conflict preferences directly", {
  expect_no_error(xpose.xtras:::.onAttach())
})

test_that(".onAttach() registers the irep() preference for xpose >= 0.5.0", {
  testthat::local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "xpose")) package_version("0.6.0") else utils::packageVersion(pkg, ...)
    },
    .package = "utils"
  )
  expect_no_error(xpose.xtras:::.onAttach())
})
