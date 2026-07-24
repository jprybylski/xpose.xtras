test_that("set_xtras_options forwards to options() with the xpose.xtras. prefix", {
  old_opts <- options(xpose.xtras.save_dir = NULL, xpose.xtras.default_watermark = NULL)
  on.exit(options(old_opts), add = TRUE)

  set_xtras_options(save_dir = "figures", default_watermark = list(label = "DRAFT"))

  expect_identical(getOption("xpose.xtras.save_dir"), "figures")
  expect_identical(getOption("xpose.xtras.default_watermark"), list(label = "DRAFT"))
})

test_that("set_xtras_options can clear an option by setting it to NULL", {
  old_opts <- options(xpose.xtras.save_dir = "figures")
  on.exit(options(old_opts), add = TRUE)

  set_xtras_options(save_dir = NULL)
  expect_null(getOption("xpose.xtras.save_dir"))
})

test_that("set_xtras_options rejects names outside the recognized registry", {
  expect_error(set_xtras_options(not_a_real_option = TRUE), regexp = "not_a_real_option")
})

test_that("set_xtras_options returns previous values invisibly, like options()", {
  old_opts <- options(xpose.xtras.save_dir = "old")
  on.exit(options(old_opts), add = TRUE)

  expect_invisible(set_xtras_options(save_dir = "new"))

  options(xpose.xtras.save_dir = "old2")
  res <- set_xtras_options(save_dir = "new2")
  expect_identical(res$xpose.xtras.save_dir, "old2")
})

test_that("set_option() merges default_labs/default_watermark the same way as the dedicated setters", {
  # calling set_option() directly (bypassing set_default_labs()) only
  # touches the key named in `...`, leaving other keys already set alone
  xpdb1 <- set_default_labs(xpdb_x, caption = "A", title = "B")
  xpdb2 <- set_option(xpdb1, default_labs = list(caption = "C"))
  expect_identical(xpdb2$options$default_labs, list(caption = "C", title = "B"))

  xpdb3 <- set_default_watermark(xpdb_x, label = "A", colour = "red")
  xpdb4 <- set_option(xpdb3, default_watermark = list(label = "B"))
  expect_identical(xpdb4$options$default_watermark, list(label = "B", colour = "red"))
})

test_that("get_xtras_option reports the dominant tier for two-tier options", {
  old_opts <- options(xpose.xtras.default_labs = NULL)
  on.exit(options(old_opts), add = TRUE)

  # neither tier set
  res0 <- get_xtras_option("default_labs", xpdb_x)
  expect_null(res0$option)
  expect_null(res0$xpdb)
  expect_identical(res0$dominant, "neither")

  # only the option is set
  options(xpose.xtras.default_labs = list(caption = "session default"))
  res1 <- get_xtras_option("default_labs", xpdb_x)
  expect_identical(res1$option, list(caption = "session default"))
  expect_null(res1$xpdb)
  expect_identical(res1$dominant, "option")

  # xpdb-level default is set too: xpdb wins
  xpdb2 <- set_default_labs(xpdb_x, caption = "model-specific")
  res2 <- get_xtras_option("default_labs", xpdb2)
  expect_identical(res2$xpdb, list(caption = "model-specific"))
  expect_identical(res2$dominant, "xpdb")

  # no xpdb supplied: only the option is visible
  res3 <- get_xtras_option("default_labs")
  expect_null(res3$xpdb)
  expect_identical(res3$dominant, "option")
})

test_that("get_xtras_option has no xpdb tier for option-only settings", {
  old_opts <- options(xpose.xtras.save_dir = "figures")
  on.exit(options(old_opts), add = TRUE)

  res <- get_xtras_option("save_dir", xpdb_x)
  expect_identical(res$option, "figures")
  expect_null(res$xpdb)
  expect_identical(res$dominant, "option")
})

test_that("get_xtras_option validates its inputs", {
  expect_error(get_xtras_option("not_a_real_option"), regexp = "not_a_real_option")
  expect_error(get_xtras_option("default_labs", "not an xpdb"), regexp = "xpose_data|xp_xtras")
})
