test_that("set_default_labs stores and merges label templates on xpdb$options", {
  xpdb1 <- set_default_labs(xpdb_x, caption = "first caption")
  expect_identical(xpdb1$options$default_labs, list(caption = "first caption"))

  # merges with (rather than replacing) previously set defaults
  xpdb2 <- set_default_labs(xpdb1, title = "a title")
  expect_identical(
    xpdb2$options$default_labs,
    list(caption = "first caption", title = "a title")
  )

  # overwrites a previously set key of the same name
  xpdb3 <- set_default_labs(xpdb2, caption = "second caption")
  expect_identical(
    xpdb3$options$default_labs,
    list(caption = "second caption", title = "a title")
  )

  expect_s3_class(xpdb3, "xp_xtras")

  expect_error(
    set_default_labs(xpdb_x, foo = "not a real label"),
    regexp = "foo"
  )
  expect_error(
    set_default_labs(xpdb_x, title = 1),
    regexp = "character"
  )
})

test_that("apply_default_labs resolves title/subtitle/caption/tag with the right precedence", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  old_opts <- options(xpose.xtras.default_labs = NULL)
  on.exit(options(old_opts), add = TRUE)

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)

  # nothing set anywhere: no-op
  expect_identical(ggplot2::get_labs(apply_default_labs(p)), ggplot2::get_labs(p))

  # option-level default fills in only when overwrite = TRUE (dv_vs_ipred sets its own caption)
  options(xpose.xtras.default_labs = list(caption = "option caption"))
  expect_identical(ggplot2::get_labs(apply_default_labs(p))$caption, ggplot2::get_labs(p)$caption)
  expect_identical(ggplot2::get_labs(apply_default_labs(p, overwrite = TRUE))$caption, "option caption")

  # xpdb-level default (via set_default_labs) wins over the option
  xpdb2 <- set_default_labs(xpdb_x, caption = "xpdb caption")
  expect_identical(
    ggplot2::get_labs(apply_default_labs(p, xpdb = xpdb2, overwrite = TRUE))$caption,
    "xpdb caption"
  )

  # a directly-supplied label wins over both option and xpdb defaults
  expect_identical(
    ggplot2::get_labs(apply_default_labs(p, caption = "direct caption", xpdb = xpdb2, overwrite = TRUE))$caption,
    "direct caption"
  )

  # a label already on the plot is preserved unless overwrite = TRUE
  p_subtitle <- p + ggplot2::labs(subtitle = "already set")
  expect_identical(
    ggplot2::get_labs(apply_default_labs(p_subtitle, subtitle = "would replace"))$subtitle,
    "already set"
  )
  expect_identical(
    ggplot2::get_labs(apply_default_labs(p_subtitle, subtitle = "replaces", overwrite = TRUE))$subtitle,
    "replaces"
  )
})

test_that("apply_default_labs resolves @keyword placeholders", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  old_opts <- options(xpose.xtras.default_labs = NULL)
  on.exit(options(old_opts), add = TRUE)

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  nobs <- xpose::get_summary(xpdb_ex_pk) %>%
    dplyr::filter(problem == max(problem), label == "nobs") %>%
    dplyr::pull(value)

  # resolved using the reduced context xpose attaches to xpose_plot objects, when no xpdb given
  p1 <- apply_default_labs(p, caption = "@nobs observations", overwrite = TRUE)
  expect_identical(ggplot2::get_labs(p1)$caption, paste(nobs, "observations"))

  # resolved using an explicitly-supplied xpdb
  p2 <- apply_default_labs(p, caption = "@nobs observations", xpdb = xpdb_ex_pk, overwrite = TRUE)
  expect_identical(ggplot2::get_labs(p2)$caption, paste(nobs, "observations"))
})

test_that("apply_default_labs works on a plain (non-xpose) ggplot object", {
  old_opts <- options(xpose.xtras.default_labs = list(title = "plain ggplot title"))
  on.exit(options(old_opts), add = TRUE)

  pg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_identical(ggplot2::get_labs(apply_default_labs(pg))$title, "plain ggplot title")
})

test_that("apply_default_labs validates its inputs", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  expect_error(apply_default_labs("not a plot"), regexp = "ggplot")
  expect_error(apply_default_labs(p, foo = "bar"), regexp = "foo")
  expect_error(apply_default_labs(p, xpdb = "not an xpdb"), regexp = "xpose_data|xp_xtras")
})

test_that("ggsave_xp applies default labels and forwards output options to save_fun", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  old_opts <- options(
    xpose.xtras.default_labs = list(tag = "A"),
    xpose.xtras.save_dir = "some_dir",
    xpose.xtras.save_width = 9,
    xpose.xtras.save_height = 5
  )
  on.exit(options(old_opts), add = TRUE)

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  expect_null(ggplot2::get_labs(p)$tag)

  captured <- NULL
  mock_save <- function(plot, filename, path, width, height, ...) {
    captured <<- list(plot = plot, filename = filename, path = path, width = width, height = height)
    "mocked/path.png"
  }

  out <- ggsave_xp(p, filename = "out.png", save_fun = mock_save)

  expect_identical(out, "mocked/path.png")
  expect_identical(captured$path, "some_dir")
  expect_identical(captured$width, 9)
  expect_identical(captured$height, 5)
  expect_identical(ggplot2::get_labs(captured$plot)$tag, "A")

  # apply_labs = FALSE skips apply_default_labs()
  ggsave_xp(p, filename = "out2.png", apply_labs = FALSE, save_fun = mock_save)
  expect_null(ggplot2::get_labs(captured$plot)$tag)
})

test_that("ggsave_xp defaults to ggplot2::ggsave() and actually writes a file", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  old_opts <- options(xpose.xtras.default_labs = NULL)
  on.exit(options(old_opts), add = TRUE)

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  out <- ggsave_xp(p, filename = "out.png", path = tmp_dir, width = 4, height = 3)
  expect_true(file.exists(file.path(tmp_dir, "out.png")))
  expect_identical(out, file.path(tmp_dir, "out.png"))
})

test_that("ggsave_xp resolves @keyword placeholders in filename/path", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  old_opts <- options(xpose.xtras.default_labs = NULL)
  on.exit(options(old_opts), add = TRUE)

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  captured <- NULL
  mock_save <- function(plot, filename, path, ...) {
    captured <<- list(filename = filename, path = path)
    "mocked"
  }

  ggsave_xp(p, filename = "@run.png", path = "out_@run", save_fun = mock_save)
  expect_false(grepl("@run", captured$filename))
  expect_false(grepl("@run", captured$path))
})

test_that("ggsave_xp works with a save_fun whose signature differs from ggplot2::ggsave (e.g. reportifyr::ggsave_with_metadata)", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # mirrors reportifyr::ggsave_with_metadata()'s actual signature: filename
  # first, plot second, extra meta_* args, and -- critically -- no `path`
  # formal at all (it's only ever received via `...` and forwarded on to
  # ggplot2::ggsave() itself), so a caller's `path`/xpose.xtras.save_dir
  # must still reach it correctly
  mock_reportifyr <- function(filename, plot = ggplot2::last_plot(), meta_type = "NA",
                               meta_equations = NULL, meta_notes = NULL, meta_abbrevs = NULL, ...) {
    extra <- list(...)
    list(filename = filename, plot = plot, meta_type = meta_type, path = extra$path)
  }

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  res <- ggsave_xp(
    p, filename = "out.png", path = "custom_dir",
    save_fun = mock_reportifyr, meta_type = "table"
  )

  expect_identical(res$filename, "out.png")
  expect_identical(res$path, "custom_dir")
  expect_identical(res$meta_type, "table")
  expect_s3_class(res$plot, "ggplot")
})

test_that("ggsave_xp's save_fun falls back to the xpose.xtras.save_fun option", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  old_opts <- options(xpose.xtras.save_fun = NULL)
  on.exit(options(old_opts), add = TRUE)

  captured <- NULL
  options(xpose.xtras.save_fun = function(plot, filename, path, ...) {
    captured <<- filename
    "from option"
  })

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  out <- ggsave_xp(p, filename = "via_option.png")
  expect_identical(out, "from option")
  expect_identical(captured, "via_option.png")
})

test_that("ggsave_xp validates its inputs", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  expect_error(ggsave_xp("not a plot", filename = "out.png"), regexp = "ggplot")
  expect_error(ggsave_xp(p, filename = 1), regexp = "character|string")
  expect_error(ggsave_xp(p, filename = "out.png", save_fun = "not a function"), regexp = "function")
})
