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
  cont_p <- cov_grid(xpdb_x, cols = c(SEX,CLCR))
  expect_setequal(
    names(cont_p$data),
    c("SEX","CLCR")
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
  etacont_p <- eta_vs_cov_grid(xpdb_x, etavar = ETA1)
  expect_setequal(
    names(etacont_p$data),
    c(xp_var(xpdb_x, type=c("contcov","catcov"))$col, "ETA1") %>%
      transform_eta()
  )
  etacont_p <- eta_vs_cov_grid(xpdb_x, cols = CLCR, etavar = ETA1)
  expect_setequal(
    names(etacont_p$data),
    c("CLCR", "ETA1") %>%
      transform_eta()
  )
  expect_identical(
    names(etacont_p$data),
    c("CLCR", "ETA1") %>%
      transform_eta()
  )
  etacont_p <- eta_vs_cov_grid(xpdb_x, cols = CLCR, etavar = ETA1, etacov = FALSE)
  expect_identical(
    names(etacont_p$data),
    c("ETA1","CLCR") %>%
      transform_eta()
  )


  #### vdiffr tests to skip on CRAN
  skip_on_cran()
  skip_on_covr()
  skip()
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

test_that("errors and special plot circumstances are correctly caught", {
  expect_error(
    vismo_pomod %>% eta_grid(etavar = P1, quiet = TRUE),
    "should only include etas.*P1"
  )
  expect_error(
    vismo_pomod %>%
      set_var_types(catcov=COHORT,contcov=AGE) %>%
      eta_vs_cov_grid(etavar = P1, quiet = TRUE, drop_fixed = FALSE),
    "should only include etas.*P1"
  )
  expect_error(
    vismo_pomod %>%
      set_var_types(catcov=COHORT,contcov=AGE) %>%
      eta_vs_contcov(etavar = P1, quiet = TRUE, drop_fixed = FALSE),
    "should only include etas.*P1"
  )
  expect_error(
    vismo_pomod %>%
      set_var_types(catcov=COHORT,contcov=AGE) %>%
      eta_vs_catcov(etavar = P1, quiet = TRUE, drop_fixed = FALSE),
    "should only include etas.*P1"
  )
  expect_error(
    pheno_base %>% cov_grid(covtypes = "bbb", quiet=TRUE),
    "Invalid.*bbb"
  )
  expect_error(
    pheno_base %>% eta_vs_cov_grid(covtypes = "bbb", quiet=TRUE),
    "Invalid.*bbb"
  )
  expect_error(
    pheno_base %>% cov_grid(cols=WT,covtypes = "cat", quiet=TRUE),
    "should only include.*cat.*WT"
  )
  expect_error(
    pheno_base %>% cov_grid(cols=APGR,covtypes = "cont", quiet=TRUE),
    "should only include.*cont.*APGR"
  )
  expect_error(
    pheno_base %>% eta_vs_cov_grid(cols=WT,covtypes = "cat", quiet=TRUE),
    "should only include.*cat.*WT"
  )
  suppressMessages(expect_message(
    xpose::xpdb_ex_pk %>% cov_grid(),
    "Cannot show N"
  ))
  suppressMessages(expect_message(
    xpose::xpdb_ex_pk %>% eta_vs_cov_grid(),
    "Cannot show N"
  ))
  suppressMessages(expect_no_message(
    xpose::xpdb_ex_pk %>% cov_grid(show_n = FALSE),
    message="Cannot show N"
  ))



})
