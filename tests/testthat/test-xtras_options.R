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

test_that("set_xtras_options accepts save_fun and it's picked up by ggsave_xp", {
  old_opts <- options(xpose.xtras.save_fun = NULL)
  on.exit(options(old_opts), add = TRUE)

  mock_save <- function(plot, filename, path, ...) "from set_xtras_options"
  set_xtras_options(save_fun = mock_save)
  expect_identical(getOption("xpose.xtras.save_fun"), mock_save)

  data("xpdb_ex_pk", package = "xpose", envir = environment())
  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  expect_identical(ggsave_xp(p, filename = "out.png"), "from set_xtras_options")
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

test_that("has_default_watermark checks both the option and xpdb tiers", {
  old_opts <- options(xpose.xtras.default_watermark = NULL)
  on.exit(options(old_opts), add = TRUE)

  expect_false(has_default_watermark())
  expect_false(has_default_watermark(xpdb_x))

  options(xpose.xtras.default_watermark = list(label = "DRAFT"))
  expect_true(has_default_watermark())
  expect_true(has_default_watermark(xpdb_x))
  options(xpose.xtras.default_watermark = NULL)

  xpdb2 <- set_default_watermark(xpdb_x, label = "DRAFT")
  expect_true(has_default_watermark(xpdb2))
  expect_false(has_default_watermark(xpdb_x))
})

test_that("auto_apply_defaults respects xpose.xtras.auto_apply and only watermarks when configured", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  old_opts <- options(
    xpose.xtras.auto_apply = NULL,
    xpose.xtras.default_labs = NULL,
    xpose.xtras.default_watermark = NULL
  )
  on.exit(options(old_opts), add = TRUE)

  p <- xpose::dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)
  n_layers <- length(p$layers)

  # nothing configured: auto_apply defaults to TRUE but is a no-op
  p0 <- auto_apply_defaults(p)
  expect_identical(ggplot2::get_labs(p0), ggplot2::get_labs(p))
  expect_length(p0$layers, n_layers)

  # labels: applied automatically once configured
  options(xpose.xtras.default_labs = list(tag = "A"))
  expect_identical(ggplot2::get_labs(auto_apply_defaults(p))$tag, "A")

  # watermark: only added once default_watermark is configured (never an
  # unprompted "DRAFT" just because auto_apply defaults to TRUE)
  expect_length(auto_apply_defaults(p)$layers, n_layers)
  options(xpose.xtras.default_watermark = list(label = "DRAFT"))
  expect_length(auto_apply_defaults(p)$layers, n_layers + 1)

  # the master switch disables both, regardless of what's configured
  options(xpose.xtras.auto_apply = FALSE)
  p_off <- auto_apply_defaults(p)
  expect_identical(ggplot2::get_labs(p_off), ggplot2::get_labs(p))
  expect_length(p_off$layers, n_layers)
})

test_that("ggsave_xp's apply_labs/apply_watermark default to xpose.xtras.auto_apply", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  old_opts <- options(
    xpose.xtras.auto_apply = NULL,
    xpose.xtras.default_labs = list(tag = "A"),
    xpose.xtras.default_watermark = list(label = "DRAFT")
  )
  on.exit(options(old_opts), add = TRUE)

  p <- xpose::dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)
  n_layers <- length(p$layers)

  captured <- NULL
  mock_save <- function(plot, filename, path, ...) {
    captured <<- plot
    "mocked"
  }

  # auto_apply = TRUE (the default): both labels and watermark applied
  ggsave_xp(p, filename = "out.png", save_fun = mock_save)
  expect_identical(ggplot2::get_labs(captured)$tag, "A")
  expect_length(captured$layers, n_layers + 1)

  # auto_apply = FALSE: neither applied, without touching apply_labs/apply_watermark explicitly
  options(xpose.xtras.auto_apply = FALSE)
  ggsave_xp(p, filename = "out2.png", save_fun = mock_save)
  expect_null(ggplot2::get_labs(captured)$tag)
  expect_length(captured$layers, n_layers)
  options(xpose.xtras.auto_apply = NULL)

  # a per-call override still wins over the option
  ggsave_xp(p, filename = "out3.png", save_fun = mock_save, apply_watermark = FALSE)
  expect_identical(ggplot2::get_labs(captured)$tag, "A")
  expect_length(captured$layers, n_layers)
})
