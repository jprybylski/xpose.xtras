test_that("xplot_boxplot", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # test both xpdb_x xpdb_ex_pk produce the same plot (d/t filling theme)
  wo_xpx <- xpdb_ex_pk %>% xplot_boxplot(aes(MED1,ETA1), quiet = TRUE)
  w_xpx <- xpdb_ex_pk %>% as_xpdb_x() %>% xplot_boxplot(aes(MED1,ETA1), quiet = TRUE)

  # test desired geoms are included
  # wrapper function to get geom type and outliers
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  has_outliers <- function(gg) any(purrr::map_lgl(gg$layers, ~"outlier.shape" %in% names(.x$geom_params) &&
                                                   !is.na(.x$geom_params$outlier.shape)))

  # bo is default
  def_bp <- xplot_boxplot(xpdb_x, aes(MED1,ETA1), quiet = TRUE)
  expect_setequal(
    geoms_lists(def_bp),
    "GeomBoxplot"
  )
  expect_true(
    has_outliers(def_bp)
  )
  expect_identical(
    def_bp,
    xplot_boxplot(xpdb_x, aes(MED1,ETA1), type= "bo", quiet = TRUE)
  )

  # b should not have outliers
  expect_false(
    has_outliers(xplot_boxplot(xpdb_x, aes(MED1,ETA1), type= "b", quiet = TRUE))
  )

  # bvl should have violin and hline
  expect_setequal(
    geoms_lists(xplot_boxplot(xpdb_x, aes(MED1,ETA1), type= "bvl", quiet = TRUE)),
    c("GeomBoxplot","GeomViolin","GeomHline")
  )
  # vl should have violin and hline
  expect_setequal(
    geoms_lists(xplot_boxplot(xpdb_x, aes(MED1,ETA1), type= "vl", quiet = TRUE)),
    c("GeomViolin","GeomHline")
  )
  # p should have dotplot
  expect_setequal(
    geoms_lists(xplot_boxplot(xpdb_x, aes(MED1,ETA1), type= "p", quiet = TRUE)),
    c("GeomDotplot")
  )
  # s should have smooth
  expect_setequal(
    geoms_lists(xplot_boxplot(xpdb_x, aes(MED1,ETA1), type= "s", quiet = TRUE)),
    c("GeomSmooth")
  )
  # j should have jitter
  expect_setequal(
    geoms_lists(xplot_boxplot(xpdb_x, aes(MED1,ETA1), type= "j", quiet = TRUE)),
    c("GeomPoint")
  )
  # c should have connecting lines for jitter
  expect_setequal(
    geoms_lists(xplot_boxplot(xpdb_x, aes(MED1,ETA1), type= "c", quiet = TRUE)),
    c("GeomPath")
  )


  # test orientation effects
  # orientation should affect line
  expect_setequal(
    geoms_lists(xplot_boxplot(xpdb_x, aes(ETA1,MED1), type= "bl", orientation = "y", quiet = TRUE)),
    c("GeomBoxplot","GeomVline")
  )
  # Should affect dotplot binaxis
  expect_equal(
    xplot_boxplot(xpdb_x, aes(ETA1,MED1),
                  type= "p",
                  #orientation = "y", # use default
                  quiet = TRUE)$layers[[1]]$geom_params$binaxis,
    "y"
  )
  expect_equal(
    xplot_boxplot(xpdb_x, aes(MED1,ETA1), # <- this did not need to change here, but would throw an error if actually plotted
                  type= "p",
                  orientation = "y",
                  quiet = TRUE)$layers[[1]]$geom_params$binaxis,
    "x"
  )


  # test quiet effects
  suppressMessages(expect_equal(
    xpdb_x$options$quiet,
    xplot_boxplot(xpdb_x, aes(MED1,ETA1))$plot_env$quiet
  ))
  expect_message(
    xplot_boxplot(xpdb_x, aes(MED1,ETA1)),
    "Using data from"
  )
  suppressMessages(expect_warning(
    xplot_boxplot(xpdb_x, aes(ETA1,MED1), orientation = "y"),
    "yscale is not discrete"
  ))
  expect_error(
    print(xplot_boxplot(xpdb_x, aes(MED1,ETA1), orientation = "y", quiet=TRUE)),
    "Discrete value(s)? supplied to continuous scale"
  )

  # test facet effects
  fct <- xplot_boxplot(xpdb_x, aes(MED1,ETA1), facets="SEX", quiet=TRUE)
  expect_true(
    "facets" %in% names(fct$facet$params) &&
      names(fct$facet$params$facets)=="SEX"
  )
  expect_false(
    "facets" %in% names(def_bp$facet$params) &&
      names(def_bp$facet$params$facets)=="SEX"
  )

  # test ggtheme
  expect_contains(
    xplot_boxplot(xpdb_x,
                  aes(MED1,ETA1),
                  gg_theme = xpose::theme_bw2(),
                  quiet=TRUE)$theme,
    xpose::theme_bw2()
    )
  expect_failure(
    expect_contains(
      def_bp$theme,
      xpose::theme_bw2()
    ))

  # xp-theme basic check
  expect_failure(expect_identical(
    def_bp,
    xplot_boxplot(xpdb_x, aes(MED1,ETA1), quiet = TRUE, xp_theme = xpose::theme_xp_xpose4())
  ))


  # No data error
  expect_error(
    xplot_boxplot(xpdb_x,
                  aes(MED1,ETA1),
                  quiet=TRUE,
                  opt = xpose::data_opt(filter = function(x) x[x$ETA1 >999,])),
    "No data available"
  )


  #### vdiffr tests to skip on CRAN
  skip_on_cran()
  skip_on_covr()
  skip()
  library(vdiffr)
  expect_doppelganger("from xpose_data", wo_xpx) # expect same as snapshot
  expect_doppelganger("from xp_xtra", w_xpx) # expect same as snapshot
  expect_doppelganger("from xpose_data", w_xpx) # expect same as xpose_data snapshot

})
