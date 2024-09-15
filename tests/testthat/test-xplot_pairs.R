test_that("xplot_pairs", {
  library(vdiffr)

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # Declare postprocessing so only columns needed are plotted
  opt_xp <- xpose::data_opt(.problem = 1,
                              filter = xpose::only_distinct(xpdb_ex_pk, 1, NULL, TRUE),
                              post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED")))
  opt_xtra <- xpose::data_opt(.problem = 1,
                              filter = xpose::only_distinct(xpdb_ex_pk %>% as_xpdb_x(), 1, NULL, TRUE),
                              post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED")))

  # test both xpdb_x xpdb_ex_pk produce the same plot (d/t filling theme)
  wo_xpx <- xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE)
  w_xpx <- xpdb_ex_pk %>% as_xpdb_x() %>% xplot_pairs(opt=opt_xtra, quiet = TRUE)

  ## Other
  opt_xtra <- xpose::data_opt(.problem = 1,
                              filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
                              post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED")))

  suppressMessages(expect_equal(
    xpdb_x$options$quiet,
    xplot_pairs(xpdb_x, opt=opt_xtra)$xpose$quiet
  ))
  expect_message(
    xplot_pairs(xpdb_x, opt=opt_xtra),
    "Using data from"
  )

  ## Check error catching
  expect_error(
    xplot_pairs(xpdb_x, opt=opt_xtra, quiet = TRUE,
                contcont_opts = list(other_fun=1)),
    "function.*not a.*numeric"
  )
  expect_error(
    xplot_pairs(xpdb_x, opt=opt_xtra, quiet = TRUE,
                catcont_opts = list(other_fun=1)),
    "function.*not a.*numeric"
  )

  #### vdiffr tests to skip on CRAN
  skip_on_cran()
  library(vdiffr)

  expect_doppelganger("from xpose_data", wo_xpx) # expect same as snapshot
  expect_doppelganger("from xp_xtra", w_xpx) # expect same as snapshot
  expect_doppelganger("from xpose_data", w_xpx) # expect same as xpose_data snapshot

  ## Expect options to make plot change (not dissecting specific changes)

  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               cont_opts=list(guide=TRUE))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               cont_opts=list(type="p"))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               dist_opts=list(type="d"))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               cat_opts=list(type="vl"))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               contcont_opts = list(stars=TRUE))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               catcont_opts = list(stars=TRUE))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               catcat_opts = list(use_rho=FALSE))
  ))


  # test ggtheme basic check
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               gg_theme = xpose::theme_bw2())
  ))

  # xp-theme basic check
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               xp_theme = xpose::theme_xp_xpose4())
  ))


})
