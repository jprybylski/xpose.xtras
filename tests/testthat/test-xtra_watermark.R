test_that("add_watermark adds a single annotation_custom layer and preserves the plot", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  n_layers <- length(p$layers)

  pw <- add_watermark(p, label = "DRAFT")

  expect_s3_class(pw, "ggplot")
  expect_s3_class(pw, "xpose_plot")
  expect_length(pw$layers, n_layers + 1)

  wm_layer <- pw$layers[[length(pw$layers)]]
  expect_s3_class(wm_layer$geom, "GeomCustomAnn")

  # underlying data/labels/mapping are untouched
  expect_identical(ggplot2::get_labs(pw), ggplot2::get_labs(p))
  expect_identical(pw$data, p$data)
})

test_that("add_watermark's text grob reflects label/colour/alpha/angle/size", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  pw <- add_watermark(
    p,
    label = "CONFIDENTIAL",
    colour = "red",
    alpha = 0.5,
    size = 40,
    angle = 15,
    fontface = "italic"
  )

  wm_grob <- pw$layers[[length(pw$layers)]]$geom_params$grob
  expect_s3_class(wm_grob, "text")
  expect_identical(as.character(wm_grob$label), "CONFIDENTIAL")
  expect_identical(wm_grob$rot, 15)
  expect_identical(wm_grob$gp$fontsize, 40)
  expect_identical(unname(wm_grob$gp$font), 3L)
  expect_identical(wm_grob$gp$col, grDevices::adjustcolor("red", alpha.f = 0.5))
})

test_that("add_watermark works on a plain ggplot and on faceted plots", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  pg <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  pgw <- add_watermark(pg)
  expect_s3_class(pgw, "ggplot")
  expect_false(inherits(pgw, "xpose_plot"))

  pf <- xpose::dv_vs_ipred(xpdb_ex_pk, facets = "SEX")
  pfw <- add_watermark(pf, label = "DRAFT")
  expect_no_error(ggplot2::ggplot_build(pfw))
})

test_that("add_watermark validates its inputs", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  expect_error(add_watermark("not a plot"), regexp = "ggplot")
  expect_error(add_watermark(p, label = 1), regexp = "character|string")
  expect_error(add_watermark(p, alpha = 2), regexp = "alpha")
  expect_error(add_watermark(p, alpha = -1), regexp = "alpha")
  expect_error(add_watermark(p, xpdb = "not an xpdb"), regexp = "xpose_data|xp_xtras")
})

test_that("set_default_watermark stores and merges defaults on xpdb$options", {
  xpdb1 <- set_default_watermark(xpdb_x, label = "DRAFT")
  expect_identical(xpdb1$options$default_watermark, list(label = "DRAFT"))

  xpdb2 <- set_default_watermark(xpdb1, colour = "red")
  expect_identical(
    xpdb2$options$default_watermark,
    list(label = "DRAFT", colour = "red")
  )

  expect_s3_class(xpdb2, "xp_xtras")
  expect_error(set_default_watermark(xpdb_x, foo = "bar"), regexp = "foo")
})

test_that("add_watermark resolves settings with option < xpdb < direct precedence", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  old_opts <- options(xpose.xtras.default_watermark = NULL)
  on.exit(options(old_opts), add = TRUE)

  p <- xpose::dv_vs_ipred(xpdb_ex_pk)
  wm_label <- function(pw) pw$layers[[length(pw$layers)]]$geom_params$grob$label

  # built-in fallback
  expect_identical(wm_label(add_watermark(p)), "DRAFT")

  # option-level default
  options(xpose.xtras.default_watermark = list(label = "OPTION"))
  expect_identical(wm_label(add_watermark(p)), "OPTION")

  # xpdb-level default wins over option
  xpdb2 <- set_default_watermark(xpdb_x, label = "XPDB")
  expect_identical(wm_label(add_watermark(p, xpdb = xpdb2)), "XPDB")

  # direct argument wins over both
  expect_identical(wm_label(add_watermark(p, label = "DIRECT", xpdb = xpdb2)), "DIRECT")
})
