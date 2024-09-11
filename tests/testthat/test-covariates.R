test_that("grid plots appear as expected", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # test both xpdb_x xpdb_ex_pk produce the same plot
  wo_xpx <- xpdb_ex_pk %>% eta_grid(quiet = TRUE)
  w_xpx <- xpdb_ex_pk %>% as_xpdb_x() %>% eta_grid(quiet = TRUE)

  ## General tests
  xpdb_x <- set_option(xpdb_x, quiet=TRUE)
  eta1_grid <- eta_grid(xpdb_x, etavar = ETA1)
  eta2_grid <- eta_grid(xpdb_x, etavar = ETA2)
  eta12_grid <- eta_grid(xpdb_x, etavar = c(ETA1,ETA2))

  cont_p <- cov_grid(xpdb_x, covtypes = "cont")
  expect_setequal(
    names(cont_p$data),
    xp_var(xpdb_x, type="contcov")$col
  )
  cat_p <- cov_grid(xpdb_x, covtypes = "cat")
  expect_setequal(
    names(cat_p$data),
    xp_var(xpdb_x, type="catcov")$col
  )
  both_p <- cov_grid(xpdb_x)
  expect_setequal(
    names(both_p$data),
    xp_var(xpdb_x, type=c("contcov","catcov"))$col
  )

  labl_x <- xpdb_x %>%
    xpose::set_var_labels(AGE="Age", MED1 = "Digoxin", .problem = 1) %>%
    xpose::set_var_units(AGE="yrs", .problem = 1) %>%
    set_var_levels(SEX=lvl_sex(), MED1 = lvl_bin(), .problem = 1)
  cov_labd <- cov_grid(labl_x)
  expect_true(
    any(
      grepl("Age",names(cov_labd$data))
    )
  )
  expect_true(
    any(
      grepl("yrs",names(cov_labd$data))
    )
  )
  expect_true(
    any(
      grepl("Digoxin",names(cov_labd$data))
    )
  )
  expect_true(
    any(
      grepl("(Yes|No)",cov_labd$data$Digoxin)
    )
  )
  expect_true(
    any(
      grepl("(Male|Female)",cov_labd$data$SEX )
    )
  )
  expect_true(
    any(
      grepl("N\\s*=\\s*\\d",cov_labd$data$SEX )
    )
  )
  expect_false(
    any(
      grepl("N\\w*=\\w*\\d",cov_grid(labl_x, show_n = FALSE)$data$SEX )
    )
  )

  etacont_p <- eta_vs_cov_grid(xpdb_x, covtypes = "cont")
  transform_eta <- function(x) stringr::str_replace(x, stringr::regex("^ET(A?)(\\d+)$"),
                                                    "ETA(\\2)")
  expect_setequal(
    names(etacont_p$data),
    xp_var(xpdb_x, type=c("contcov","eta"))$col %>%
      transform_eta()
  )
  etacont_p <- eta_vs_cov_grid(xpdb_x, covtypes = "cat")
  expect_setequal(
    names(etacont_p$data),
    xp_var(xpdb_x, type=c("catcov","eta"))$col %>%
      transform_eta()
  )
  etacont_p <- eta_vs_cov_grid(xpdb_x)
  expect_setequal(
    names(etacont_p$data),
    xp_var(xpdb_x, type=c("contcov","catcov","eta"))$col %>%
      transform_eta()
  )


  #### vdiffr tests to skip on CRAN
  skip_on_cran()
  library(vdiffr)
  expect_doppelganger("from xpose_data", wo_xpx) # expect same as snapshot
  expect_doppelganger("from xp_xtra", w_xpx) # expect same as snapshot
  expect_doppelganger("from xpose_data", w_xpx) # expect same as xpose_data snapshot


  expect_doppelganger("eta1 plot", eta1_grid)
  expect_doppelganger("eta12 plot", eta12_grid)
  expect_failure(expect_doppelganger(
    "eta1 plot", eta2_grid
  ))
  expect_failure(expect_doppelganger(
    "eta1 plot", eta12_grid
  ))
  expect_failure(expect_doppelganger(
    "eta1 plot", eta_grid(xpdb_x)
  ))
  expect_failure(expect_doppelganger(
    "eta12 plot", eta_grid(xpdb_x, etavar = c(ETA1,ETA2),
                           pairs_opts = list(contcont_opts=list(stars=TRUE)))
  ))


})

test_that("individual eta-cov plots", {
  xpdb_x <- set_option(xpdb_x, quiet=TRUE)

  expect_s3_class(
    eta_vs_contcov(xpdb_x, etavar = ETA1),
    "xpose_plot"
  )
  expect_length(
    eta_vs_contcov(xpdb_x),
    nrow(xp_var(xpdb_x, type = "eta"))
  )

  expect_s3_class(
    eta_vs_catcov(xpdb_x, etavar = ETA1),
    "xpose_plot"
  )
  expect_length(
    eta_vs_catcov(xpdb_x),
    nrow(xp_var(xpdb_x, type = "eta"))
  )

  expect_true(
    any(
      grepl("N\\s*=\\s*\\d",eta_vs_catcov(xpdb_x, etavar=ETA1)$data$value )
    )
  )
  expect_false(
    any(
      grepl("N\\w*=\\w*\\d",eta_vs_catcov(xpdb_x, etavar=ETA1, show_n = FALSE)$data$value )
    )
  )
  expect_false(
    any(
      grepl("N\\w*=\\w*\\d",eta_vs_catcov(xpose::xpdb_ex_pk,
                                          etavar=ETA1, show_n = TRUE, quiet=TRUE)$data$value )
    )
  )

  expect_failure(expect_identical(
    eta_vs_catcov(xpdb_x, etavar=ETA1),
    eta_vs_catcov(xpdb_x, etavar=ETA1, orientation = "y")
  ))

})
