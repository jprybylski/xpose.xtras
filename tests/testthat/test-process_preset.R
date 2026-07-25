# .process_preset_env is a package-level singleton, so every test resets it
# on both ends -- leftover presets from one test would leak into (and desync
# auto-naming for) the next.
reset_process_presets <- function() {
  .process_preset_env$presets <- list()
}

test_that("validate_process_preset accepts one-sided formulas and functions, rejects everything else", {
  reset_process_presets()
  on.exit(reset_process_presets())

  expect_identical(validate_process_preset(~ .x %>% as_xpdb_x()), ~ .x %>% as_xpdb_x())
  fn <- function(xpdb) xpdb
  expect_identical(validate_process_preset(fn), fn)

  expect_error(validate_process_preset(y ~ .x), "one-sided formula or a function")
  expect_error(validate_process_preset("not a preset"), "one-sided formula or a function")
  expect_error(validate_process_preset(1), "one-sided formula or a function")
})

test_that("add_process_preset auto-names presets with sequential integers", {
  reset_process_presets()
  on.exit(reset_process_presets())

  expect_message(nm1 <- add_process_preset(~ .x %>% as_xpdb_x()), "process_preset")
  expect_identical(nm1, "1")
  nm2 <- add_process_preset(~ .x %>% as_xpdb_x())
  expect_identical(nm2, "2")

  expect_identical(process_preset_names(), c("1", "2"))
})

test_that("add_process_preset skips already-used integer names when auto-naming", {
  reset_process_presets()
  on.exit(reset_process_presets())

  add_process_preset(~ .x %>% as_xpdb_x(), name = "1")
  nm <- add_process_preset(~ .x %>% as_xpdb_x())
  expect_identical(nm, "2")
})

test_that("add_process_preset stores under an explicit name and returns it invisibly", {
  reset_process_presets()
  on.exit(reset_process_presets())

  expect_invisible(add_process_preset(~ .x %>% as_xpdb_x(), name = "convert"))
  expect_identical(process_preset_names(), "convert")
})

test_that("add_process_preset requires overwrite=TRUE to replace an existing name", {
  reset_process_presets()
  on.exit(reset_process_presets())

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  expect_error(
    add_process_preset(~ .x, name = "convert"),
    "already exists"
  )

  expect_message(
    add_process_preset(~ .x, name = "convert", overwrite = TRUE),
    "convert"
  )
  expect_identical(.process_preset_env$presets[["convert"]], ~.x)
})

test_that("process_preset applies a stored formula preset by name and by index", {
  reset_process_presets()
  on.exit(reset_process_presets())

  data("xpdb_ex_pk", package = "xpose", envir = environment())
  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")

  out_by_name <- process_preset(xpdb_ex_pk, "convert")
  expect_true(is_xp_xtras(out_by_name))

  out_by_index <- process_preset(xpdb_ex_pk, 1)
  expect_true(is_xp_xtras(out_by_index))
})

test_that("process_preset applies a stored function preset and forwards ...", {
  reset_process_presets()
  on.exit(reset_process_presets())

  data("xpdb_ex_pk", package = "xpose", envir = environment())
  add_process_preset(function(xpdb, descr = "default") set_prop(xpdb, descr = descr), name = "describe")

  out <- process_preset(xpdb_ex_pk, "describe", descr = "custom description")
  expect_identical(get_prop(out, "descr"), "custom description")
})

test_that("process_preset validates xpdb and errors clearly when presets are missing", {
  reset_process_presets()
  on.exit(reset_process_presets())

  data("xpdb_ex_pk", package = "xpose", envir = environment())
  expect_error(process_preset("not an xpdb", "convert"), "xpose_data|xp_xtras")

  expect_error(process_preset(xpdb_ex_pk, "convert"), "No process presets have been added")

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  expect_error(process_preset(xpdb_ex_pk, "missing"), "No process preset named.*missing")
  expect_error(process_preset(xpdb_ex_pk, 5), regexp = NULL)
})

test_that("print_process_preset reports 'no presets' and lists names otherwise", {
  reset_process_presets()
  on.exit(reset_process_presets())

  expect_message(res_empty <- print_process_preset(), "No process presets defined")
  expect_identical(res_empty, list())

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  add_process_preset(~ .x %>% as_xpdb_x() %>% set_var_types(na = "ETA5"), name = "drop_eta5")

  expect_message(res_all <- print_process_preset(), "convert")
  expect_named(res_all, c("convert", "drop_eta5"))

  expect_message(res_one <- print_process_preset("drop_eta5"), "drop_eta5")
  expect_named(res_one, "drop_eta5")

  expect_error(print_process_preset("nope"), regexp = NULL)
})

test_that("remove_process_preset removes by name and by index, and errors on unknown presets", {
  reset_process_presets()
  on.exit(reset_process_presets())

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert2")

  expect_message(nm <- remove_process_preset("convert"), "Removed")
  expect_identical(nm, "convert")
  expect_identical(process_preset_names(), "convert2")

  remove_process_preset(1)
  expect_identical(process_preset_names(), character())

  expect_error(remove_process_preset("convert"), "No process presets have been added")

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  expect_error(remove_process_preset("nope"), "No process preset named.*nope")
})

test_that("amend_process_preset replaces an existing preset's definition in place", {
  reset_process_presets()
  on.exit(reset_process_presets())

  data("xpdb_ex_pk", package = "xpose", envir = environment())
  add_process_preset(~ .x %>% as_xpdb_x(), name = "describe")
  amend_process_preset("describe", ~ .x %>% as_xpdb_x() %>% set_prop(descr = "amended"))

  out <- process_preset(xpdb_ex_pk, "describe")
  expect_identical(get_prop(out, "descr"), "amended")
  expect_identical(process_preset_names(), "describe")
})

test_that("amend_process_preset errors when the preset doesn't already exist", {
  reset_process_presets()
  on.exit(reset_process_presets())

  expect_error(
    amend_process_preset("nope", ~ .x %>% as_xpdb_x()),
    "No process preset named.*nope.*to amend"
  )
})

test_that("resolve_process_preset_profile resolves 'project'/'user'/literal paths", {
  expect_identical(resolve_process_preset_profile("project"), file.path(getwd(), ".Rprofile"))

  old_env <- Sys.getenv("R_PROFILE_USER", unset = NA)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("R_PROFILE_USER") else Sys.setenv(R_PROFILE_USER = old_env)
  }, add = TRUE)

  Sys.setenv(R_PROFILE_USER = "/tmp/some_profile.R")
  expect_identical(resolve_process_preset_profile("user"), "/tmp/some_profile.R")

  Sys.unsetenv("R_PROFILE_USER")
  expect_identical(resolve_process_preset_profile("user"), path.expand("~/.Rprofile"))

  expect_identical(resolve_process_preset_profile("/some/literal/path"), "/some/literal/path")
})

test_that("splice_process_preset_block inserts, replaces and removes the marked block", {
  block <- c(process_preset_marker_start, "some_call()", process_preset_marker_end)

  # no existing block: appended after whatever's already there
  expect_identical(
    splice_process_preset_block(c("existing <- 1"), block),
    c("existing <- 1", block)
  )

  # existing block (with content before/after) gets replaced in place
  old <- c("before <- 1", process_preset_marker_start, "old_call()", process_preset_marker_end, "after <- 2")
  expect_identical(
    splice_process_preset_block(old, block),
    c("before <- 1", block, "after <- 2")
  )

  # empty new_block (no presets left) drops the marked block entirely
  expect_identical(
    splice_process_preset_block(old, character()),
    c("before <- 1", "after <- 2")
  )
})

test_that("build_process_preset_block deparses presets into re-runnable add_process_preset() calls", {
  reset_process_presets()
  on.exit(reset_process_presets())

  expect_identical(build_process_preset_block(), character())

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  block <- build_process_preset_block()
  expect_identical(block[1], process_preset_marker_start)
  expect_identical(block[length(block)], process_preset_marker_end)
  expect_true(any(grepl('name = "convert"', block, fixed = TRUE)))
  expect_true(any(grepl("xpose.xtras::add_process_preset", block, fixed = TRUE)))
})

test_that("persist_process_presets refuses to write outside an interactive session", {
  reset_process_presets()
  on.exit(reset_process_presets())

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  expect_error(
    persist_process_presets(profile = tmp),
    "interactive session"
  )
  expect_false(file.exists(tmp))
})

test_that("persist_process_presets asks for confirmation and does nothing if declined", {
  reset_process_presets()
  on.exit(reset_process_presets())

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(askYesNo = function(...) FALSE, .package = "utils")

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  res <- expect_message(persist_process_presets(profile = tmp), "Not persisted")
  expect_false(isTRUE(res))
  expect_false(file.exists(tmp))
})

test_that("persist_process_presets writes the block when confirmed, preserving surrounding content", {
  reset_process_presets()
  on.exit(reset_process_presets())

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("# user content before", "options(foo = 1)"), tmp)

  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(askYesNo = function(...) TRUE, .package = "utils")

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  res <- persist_process_presets(profile = tmp)
  expect_true(isTRUE(res))

  written <- readLines(tmp)
  expect_identical(written[1:2], c("# user content before", "options(foo = 1)"))
  expect_true(any(grepl("xpose.xtras::add_process_preset", written, fixed = TRUE)))

  # re-syncing after removing the preset drops the block again, but keeps
  # the user's own content untouched
  remove_process_preset("convert")
  persist_process_presets(profile = tmp, ask = FALSE)
  written2 <- readLines(tmp)
  expect_identical(written2, c("# user content before", "options(foo = 1)"))
})

test_that("persist_process_presets(ask = FALSE) writes without prompting", {
  reset_process_presets()
  on.exit(reset_process_presets())

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(askYesNo = function(...) stop("should not be called"), .package = "utils")

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert")
  expect_true(isTRUE(persist_process_presets(ask = FALSE, profile = tmp)))
  expect_true(file.exists(tmp))
})

test_that("add_process_preset/remove_process_preset/amend_process_preset forward persist=TRUE", {
  reset_process_presets()
  on.exit(reset_process_presets())

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  local_mocked_bindings(is_interactive = function() TRUE, .package = "rlang")
  local_mocked_bindings(askYesNo = function(...) TRUE, .package = "utils")

  add_process_preset(~ .x %>% as_xpdb_x(), name = "convert", persist = TRUE, profile = tmp)
  expect_true(any(grepl("convert", readLines(tmp))))

  amend_process_preset("convert", ~ .x, persist = TRUE, profile = tmp)
  expect_true(any(grepl("~.x", readLines(tmp), fixed = TRUE)))

  remove_process_preset("convert", persist = TRUE, profile = tmp)
  expect_false(any(grepl("add_process_preset", readLines(tmp))))
})
