test_that("logLik/AIC/BIC work for a single xpose_data/xp_xtras model", {
  ofv  <- as.numeric(get_prop(xpdb_x, "ofv"))
  nobs <- as.numeric(get_prop(xpdb_x, "nobs"))
  npar <- get_prm(xpdb_x, quiet = TRUE) %>%
    dplyr::pull(fixed) %>%
    magrittr::not() %>%
    sum()

  ll <- logLik(xpdb_x)
  expect_s3_class(ll, "logLik")
  expect_equal(as.numeric(ll), -ofv / 2)
  expect_equal(attr(ll, "df"), npar)
  expect_equal(attr(ll, "nobs"), nobs)

  expect_equal(AIC(xpdb_x), ofv + 2 * npar)
  expect_equal(BIC(xpdb_x), ofv + npar * log(nobs))
})

test_that("logLik() threads .problem/.subprob/.method through to the summary/parameter lookup", {
  prob <- xpose::all_data_problem(xpdb_x)[1]
  ofv  <- as.numeric(get_prop(xpdb_x, "ofv", .problem = prob))

  expect_equal(as.numeric(logLik(xpdb_x, .problem = prob)), -ofv / 2)
})

test_that("logLik/AIC/BIC refuse a model-averaged (franken) xpdb", {
  fxpdb <- franken_xpdb(pheno_base, pheno_final, .types = "catcov")
  expect_true(is_franken_xpdb(fxpdb))
  expect_false(is_franken_xpdb(xpdb_x))

  expect_error(logLik(fxpdb), "model-averaged")
  expect_error(AIC(fxpdb), "model-averaged")
  expect_error(BIC(fxpdb), "model-averaged")

  ma <- pheno_set %>% modavg_xpdb(avg_cols = DV, auto_backfill = TRUE, quiet = TRUE)
  expect_true(is_franken_xpdb(ma))
  expect_error(logLik(ma), "model-averaged")
})

test_that("logLik/AIC/BIC on an xpose_set follow diff()'s lineage guessing", {
  random_subset <- pheno_set[sample(length(pheno_set), 5)] %>%
    # extract xpdbs
    purrr::map(~.x$xpdb) %>%
    # create new set with ordered lineage
    {xpose_set(!!!., .as_ordered = TRUE)}

  expected_ll  <- random_subset %>% purrr::map_dbl(~as.numeric(logLik(.x$xpdb))) %>% unname()
  expected_aic <- random_subset %>% purrr::map_dbl(~stats::AIC(.x$xpdb)) %>% unname()
  expected_bic <- random_subset %>% purrr::map_dbl(~stats::BIC(.x$xpdb)) %>% unname()

  expect_identical(logLik(random_subset), expected_ll)
  expect_identical(AIC(random_subset), expected_aic)
  expect_identical(BIC(random_subset), expected_bic)

  # custom AIC penalty `k` is respected
  expected_aic_k4 <- random_subset %>% purrr::map_dbl(~stats::AIC(.x$xpdb, k = 4)) %>% unname()
  expect_identical(AIC(random_subset, k = 4), expected_aic_k4)

  # multiple lineage labels -> named list, as with diff()
  multi <- logLik(pheno_set, run3, run6)
  expect_true(is.list(multi))
  expect_named(multi, c("run3", "run6"))
})
