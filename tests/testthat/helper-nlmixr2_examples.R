# Session-level cache so each nlmixr2 example fit runs at most once per
# test session rather than once per test_that block.
.nlmixr2_example_cache <- new.env(parent = emptyenv())

cached_nlmixr_example <- function(name) {
  if (!exists(name, envir = .nlmixr2_example_cache, inherits = FALSE)) {
    assign(name, nlmixr_example(name), envir = .nlmixr2_example_cache)
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
