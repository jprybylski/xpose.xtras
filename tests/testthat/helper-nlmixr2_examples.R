# Session-level cache so each nlmixr2 example fit runs at most once per
# test session rather than once per test_that block.
.nlmixr2_example_cache <- new.env(parent = emptyenv())

cached_nlmixr_example <- function(name) {
  skip_on_cran()
  if (!exists(name, envir = .nlmixr2_example_cache, inherits = FALSE)) {
    # Same net effect as suppressWarnings(nlmixr_example(name)) in the
    # normal case, but if fitting *errors* (eg a fit that never gets
    # promoted to the nlmixr2FitData class -- see CI failures gated on
    # low-resource runners), any warnings raised along the way are
    # otherwise silently discarded right when they'd be most useful for
    # diagnosing why. Surface them alongside the error instead.
    warnings_seen <- character(0)
    result <- withCallingHandlers(
      tryCatch(
        nlmixr_example(name),
        error = function(e) {
          if (length(warnings_seen) > 0) {
            cli::cli_abort(
              c(
                "Building the {.val {name}} nlmixr2 example failed.",
                "i" = "Warning{?s} raised while fitting:",
                stats::setNames(warnings_seen, rep("*", length(warnings_seen)))
              ),
              parent = e,
              class = "xpose.xtras_nlmixr2_example_error"
            )
          }
          stop(e)
        }
      ),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    assign(name, result, envir = .nlmixr2_example_cache)
  }
  get(name, envir = .nlmixr2_example_cache, inherits = FALSE)
}

# Helper to load xpdb_nlmixr2_old from test data. This object was created
# with rxode2 < 5.0 and cannot be regenerated; it lives outside lazy data to
# avoid the nlmixr2est namespace warning on package load.
get_xpdb_nlmixr2_old <- function() {
  e <- new.env(parent = emptyenv())
  load(test_path("testdata", "xpdb_nlmixr2_old.rda"), envir = e)
  e$xpdb_nlmixr2_old
}
