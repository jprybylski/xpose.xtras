# --- Helpers for fabricating minimal nlmixr2 fit objects -------------------
# Several tests below need to exercise nlmixr2_prm_associations()'s handling
# of eta transformations that the real example fits never produce (custom,
# unresolvable, or boxCox/yeoJohnson-style transforms). Rather than fitting
# additional real models for each case, these helpers splice a fabricated
# `$fit` (with just the fields nlmixr2_prm_associations() reads) onto a real,
# already-fitted xpdb so xpose::software()/check_xpdb() etc still see a
# legitimate nlmixr2 xp_xtras object.
make_fake_nlmixr2_fit <- function(param_defs) {
  theta_names <- vapply(param_defs, function(p) p$theta, character(1))
  eta_names   <- vapply(param_defs, function(p) p$eta, character(1))
  params      <- vapply(param_defs, function(p) p$param, character(1))
  curevals    <- vapply(param_defs, function(p) p$curEval, character(1))
  theta_lhs <- stats::setNames(params, theta_names)
  eta_lhs   <- stats::setNames(params, eta_names)
  muref_tbl <- data.frame(theta = theta_names, eta = eta_names, level = "id", stringsAsFactors = FALSE)
  muref_cureval <- data.frame(
    parameter = c(eta_names, theta_names),
    curEval = c(curevals, curevals),
    low = NA_real_, hi = NA_real_,
    stringsAsFactors = FALSE
  )
  inidf <- data.frame(name = theta_names, backTransform = NA_character_, stringsAsFactors = FALSE)
  fake_fit <- list(
    iniUi = list(iniDf = inidf),
    ui = list(muRefCurEval = muref_cureval, muRefTable = muref_tbl, etaLhs = eta_lhs, thetaLhs = theta_lhs)
  )
  class(fake_fit) <- c("nlmixr2FitData", "list")
  fake_fit
}
make_fake_nlmixr2_xpdb <- function(param_defs) {
  base_xpdb <- cached_nlmixr_example("xpdb_nlmixr2")
  fake_xpdb <- base_xpdb
  fake_xpdb$fit <- make_fake_nlmixr2_fit(param_defs)
  fake_xpdb
}
# rxode2 gets attached to the search path as a side effect of running an
# nlmixr2 fit, which lets `probitInv`/`probit` resolve via .GlobalEnv before
# nlmixr2_prm_associations() ever tries the rxode2 namespace. A couple of
# tests need that namespace lookup to actually be exercised (and mockable),
# so temporarily detach it.
detach_rxode2 <- function() {
  was_attached <- "package:rxode2" %in% search()
  if (was_attached) suppressWarnings(detach("package:rxode2", unload = FALSE, character.only = TRUE))
  was_attached
}
reattach_rxode2 <- function(was_attached) {
  if (was_attached) suppressWarnings(try(attachNamespace("rxode2"), silent = TRUE))
}

test_that("old fit detection works correctly", {
  # Skip if rxode2 < 5.0 due to serialization incompatibility
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  # Without nlmixr2est the fit $ accessor falls back to data.frame and returns NA
  skip_if_not_installed("nlmixr2est")

  # Test with new (compatible) fit
  expect_false(
    test_nlmixr2_is_old_fit(cached_nlmixr_example("xpdb_nlmixr2"))
  )

  # Test with old (incompatible) fit
  expect_true(
    test_nlmixr2_is_old_fit(get_xpdb_nlmixr2_old())
  )

  # Test with non-nlmixr2 object
  expect_true(
    is.na(test_nlmixr2_is_old_fit(xpdb_x))
  )
})

test_that("test_nlmixr2_is_old_fit returns NA when rxode2 is unavailable", {
  real_is_installed <- rlang::is_installed
  testthat::local_mocked_bindings(
    is_installed = function(pkg, ...) if (identical(pkg, "rxode2")) FALSE else real_is_installed(pkg, ...),
    .package = "rlang"
  )
  expect_true(is.na(test_nlmixr2_is_old_fit(xpdb_x)))
})

test_that("test_nlmixr2_is_old_fit returns NA when nlmixr2est is unavailable", {
  real_is_installed <- rlang::is_installed
  testthat::local_mocked_bindings(
    is_installed = function(pkg, ...) if (identical(pkg, "nlmixr2est")) FALSE else real_is_installed(pkg, ...),
    .package = "rlang"
  )
  expect_true(is.na(test_nlmixr2_is_old_fit(xpdb_x)))
})

test_that("test_nlmixr2_is_old_fit detects the old-fit decompression warning", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  # A fit that raises the specific old-rxode2 decompression warning (rather
  # than erroring outright, as the on-disk fixture does) should still be
  # flagged old via the warning-catching branch, not the try-error fallback.
  base_xpdb <- cached_nlmixr_example("xpdb_nlmixr2")
  fake_fit <- new.env()
  makeActiveBinding("finalUi", function() {
    warning("decompression of an rxUi object from rxode2 (< 4.0) occurred", call. = FALSE)
    list(iniDf = data.frame(a = 1))
  }, fake_fit)
  class(fake_fit) <- "nlmixr2FitData"
  fake_xpdb <- base_xpdb
  fake_xpdb$fit <- fake_fit

  expect_true(test_nlmixr2_is_old_fit(fake_xpdb))
})

test_that("backfill throws error for old fits", {
  # Skip if rxode2 < 5.0 due to serialization incompatibility
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  # Without nlmixr2est, old fit detection returns NA and no error is thrown
  skip_if_not_installed("nlmixr2est")

  expect_error(
    backfill_nlmixr2_props(get_xpdb_nlmixr2_old()),
    regexp = "Incompatible nlmixr2/rxode2 fit object"
  )
})

test_that("backfill_nlmixr2_props falls back to the legacy nlmixr2est<5.0 sigdig lookup", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  xpdb_nlmixr2 <- cached_nlmixr_example("xpdb_nlmixr2")

  # Pretend nlmixr2est<5.0 and qs are both available so backfill_nlmixr2_props
  # takes the legacy `rxode2::rxGetControl(xpdb$fit$ui, "sigdig", 3L)` branch
  # instead of the current `xpdb$fit$control$rxControl$sigdig` lookup.
  real_is_installed <- rlang::is_installed
  testthat::local_mocked_bindings(
    is_installed = function(pkg, ...) if (identical(pkg, "qs")) TRUE else real_is_installed(pkg, ...),
    .package = "rlang"
  )
  real_packageVersion <- utils::packageVersion
  testthat::local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "nlmixr2est")) as.package_version("4.0.0") else real_packageVersion(pkg, ...)
    },
    .package = "utils"
  )

  res <- backfill_nlmixr2_props(xpdb_nlmixr2)
  # Whether or not rxGetControl succeeds on this fit's $ui, the result should
  # be a valid, single sigdig value (defaulting to 3 via the try-error guard
  # if it doesn't).
  expect_true(!is.na(suppressWarnings(as.numeric(get_prop(res, "nsig")))))
})

test_that("nlmixr2_as_xtra skips backfill for old fits", {
  # Skip if rxode2 < 5.0 due to serialization incompatibility
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")

  # Old fit should work with nlmixr2_as_xtra (backfill skipped)
  # but the fit object itself is old, so we can't actually test this
  # unless we use the attached fit directly
  expect_no_error(
    as_xp_xtras(get_xpdb_nlmixr2_old())
  )
})

test_that("nlmixr2 is compatible", {
  # Skip if rxode2 < 5.0 due to serialization incompatibility
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  xpdb_nlmixr2     <- cached_nlmixr_example("xpdb_nlmixr2")
  xpdb_nlmixr2_saem <- cached_nlmixr_example("xpdb_nlmixr2_saem")
  nlmixr2_warfarin  <- cached_nlmixr_example("nlmixr2_warfarin")
  nlmixr2_m3        <- cached_nlmixr_example("nlmixr2_m3")

  expect_no_error(
    as_xp_xtras(xpdb_nlmixr2)
  )
  expect_no_error(
    as_xp_xtras(xpdb_nlmixr2_saem)
  )
  fit_example <- nlmixr2_m3$fit
  expect_no_error(
    nlmixr2_as_xtra(fit_example)
  )
  expect_no_error(
    nlmixr2_as_xtra(fit_example, .skip_assoc = TRUE)
  )
  expect_failure(expect_identical(
    nlmixr2_as_xtra(fit_example),
    nlmixr2_as_xtra(fit_example, .skip_assoc = TRUE)
  ))
  # Another example
  expect_no_error(
    nlmixr2_as_xtra(nlmixr2_warfarin$fit, quiet = TRUE)
  )
  expect_no_error(
    nlmixr2_as_xtra(nlmixr2_warfarin$fit, .skip_assoc = TRUE, quiet = TRUE)
  )

  # Make sure properties can be found and manipulated
  # Including: get_prop, set_prop, backfill and options functions
  fill_test <- function(xpdb,...) {
    fill_prob_subprob_method(xpdb, ...)
    as.list(environment(), all.names = TRUE)
  }
  expect_identical(
    fill_test(xpdb_nlmixr2)$.method,
    "focei"
  )
  expect_identical(
    fill_test(xpdb_nlmixr2_saem)$.method,
    "saem"
  )
  no_summ_test <- xpdb_nlmixr2
  no_summ_test$summary <- dplyr::filter(no_summ_test$summary,label!="method")
  expect_warning(
    fill_test(as_xp_xtras(no_summ_test)),
    regexp = "may not be compatible"
  )
  expect_equal(
    as.numeric(get_prop(xpdb_nlmixr2, "condn")),
    xpdb_nlmixr2$fit$conditionNumberCov,
    ignore_attr = TRUE
  )
  random_string <- paste(sample(letters,12), collapse="")
  expect_identical(
    set_prop(xpdb_nlmixr2, descr = random_string) %>%
      get_prop("descr"),
    random_string
  )
  expect_no_error(
    backfill_iofv(xpdb_nlmixr2)
  )
  expect_message(
    backfill_iofv(set_option(xpdb_nlmixr2_saem, quiet=FALSE)),
    "Some iOFV values for problem.*are not finite"
  )
  expect_in(
    random_string,
    names(xpose::get_data(backfill_iofv(xpdb_nlmixr2, .label = random_string), quiet = TRUE))
  )
  expect_no_error(
    # pulling a typical problem 0 property is nonmem that is a problem 1 prop for nlmixr2
    get_prop(xpdb_nlmixr2, "file", .problem = 0)
  )

  # Make sure new single xpdb functions can be run without error
  expect_no_error(
    eta_grid(xpdb_nlmixr2, quiet=TRUE)
  )
  expect_no_error(
    eta_vs_contcov(xpdb_nlmixr2, quiet=TRUE)
  )
  expect_no_error(
    eta_vs_catcov(nlmixr2_warfarin, quiet=TRUE)
  )
  expect_no_error(
    eta_vs_cov_grid(nlmixr2_warfarin, quiet=TRUE)
  )
  expect_no_error({
    nlmixr2_m3 %>% # modified from catdv_vs_dvprobs example
      set_var_types(catdv=CENS,dvprobs=BLQLIKE) %>%
      set_dv_probs(1, 1~BLQLIKE, .dv_var = CENS) %>%
      set_var_levels(1, CENS = lvl_bin()) %>%
      catdv_vs_dvprobs(xlab = "basic", quiet = TRUE)
  })
  suppressMessages(expect_no_error(
    list_vars(nlmixr2_warfarin)
  ))

  # Make sure xpose_sets can be made (several iterations)
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      xpdb_nlmixr2_saem
    )
  )
  expect_no_warning(
    xpose_set(
      xpdb_nlmixr2,
      xpdb_nlmixr2_saem
    )
  )
  expect_length(
    xpose_set(
      xpdb_nlmixr2,
      xpdb_nlmixr2_saem,
      foo=xpdb_nlmixr2_saem,
      foo2=xpdb_nlmixr2_saem
    ),
    4
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3
    ) %>%
      focus_qapply(backfill_iofv)
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3, .relationships = nlmixr2_m3 ~ xpdb_nlmixr2
    )
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3,
      .as_ordered = TRUE
    )
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3
    ) %>%
      expose_param(tka)
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3
    ) %>%
      expose_property(file)
  )

  # Make sure model comparison plots can be created
  comparison <- xpose_set(
    xpdb_nlmixr2,
    nlmixr2_m3
  ) %>%
    focus_qapply(backfill_iofv)
  suppressMessages(expect_no_error(
    ipred_vs_ipred(comparison, quiet = TRUE)
  ))
  suppressMessages(expect_message(
    ipred_vs_ipred(comparison, quiet = TRUE),
    "Duplicate.*axis.text"
  ))
  suppressMessages(expect_message(
    ipred_vs_ipred(comparison, quiet = TRUE),
    "nlmixr2.*@file.*@run"
  ))
  expect_no_message(
    ipred_vs_ipred(comparison, quiet = TRUE, axis.text = "@file")
  )
  expect_no_error(
    pred_vs_pred(comparison, quiet = TRUE, axis.text = "@file")
  )
  expect_no_error(
    iofv_vs_mod(comparison, quiet = TRUE, axis.text = "@file")
  )
  expect_no_error(
    prm_waterfall(comparison, quiet = TRUE)
  )
  expect_no_error(
    eta_waterfall(comparison, quiet = TRUE)
  )
  expect_no_error(
    iofv_waterfall(comparison, quiet = TRUE)
  )
  expect_no_error(
    shark_plot(comparison, quiet = TRUE, df=1)
  )

  # Make sure model-averaging plots can be created
  expect_no_error(
    ipred_vs_idv_modavg(comparison, quiet = TRUE)
  )
  expect_no_error(
    pred_vs_idv_modavg(comparison, quiet = TRUE)
  )
  expect_no_error(
    dv_vs_ipred_modavg(comparison, quiet = TRUE)
  )
  expect_no_error(
    dv_vs_pred_modavg(comparison, quiet = TRUE)
  )


  # Make sure get_prm and prm associations work
  expect_no_error(
    get_prm(xpdb_nlmixr2, quiet = TRUE)
  )
  expect_message(
    get_prm(xpdb_nlmixr2, quiet = FALSE),
    regexp = "does not provide SE.*random effect"
  )
  suppressMessages(expect_message(
    nlmixr2_prm_associations(nlmixr2_warfarin, quiet = FALSE),
    regexp = "need to untransform thetas"
  ))
  suppressMessages(expect_message(
    nlmixr2_prm_associations(nlmixr2_warfarin, quiet = FALSE),
    regexp = "mutate_prm"
  ))
  expect_no_message(
    nlmixr2_prm_associations(xpdb_nlmixr2),
    message = "need to untransform thetas"
  )
  expect_equal(
    # all etas are log
    nlmixr2_prm_associations(set_option(xpdb_nlmixr2,quiet=TRUE)) %>%
      get_prm(),
    get_prm(xpdb_nlmixr2,quiet=TRUE),
    ignore_attr = TRUE
  )
  suppressWarnings(expect_failure(expect_equal(
    # warfarin model has logit exp
    nlmixr2_prm_associations(set_option(nlmixr2_warfarin,quiet=TRUE)) %>%
      get_prm(),
    get_prm(nlmixr2_warfarin,quiet=TRUE),
    ignore_attr = TRUE
  )))
  suppressWarnings(expect_warning(
    nlmixr2_prm_associations(set_option(nlmixr2_warfarin,quiet=TRUE)) %>%
      get_prm(),
    "NaNs produced"
  ))
  expect_no_warning(
    nlmixr2_prm_associations(set_option(nlmixr2_warfarin,quiet=TRUE)) %>%
      mutate_prm(temax~plogis) %>%
      get_prm(),
    message = "NaNs produced"
  )

})

test_that("xpdb_nlmixr2_nocov example (covMethod = \"\") works with nlmixr2_as_xtra", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  xpdb_nlmixr2_nocov <- cached_nlmixr_example("xpdb_nlmixr2_nocov")

  expect_no_error(
    as_xp_xtras(xpdb_nlmixr2_nocov)
  )
  expect_false(
    test_nlmixr2_is_old_fit(xpdb_nlmixr2_nocov)
  )
  expect_no_error(
    backfill_nlmixr2_props(xpdb_nlmixr2_nocov)
  )
  # No covariance step means get_cov_matrix() has nothing to report (see also
  # the more thorough coverage of this in test-colinearity.R)
  expect_error(
    get_cov_matrix(xpdb_nlmixr2_nocov, quiet = TRUE),
    "No correlation matrix available"
  )
})

test_that("nlmixr2_prm_associations: boxCox message, identity/custom transform, unknown transform, dry_run", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  # A fabricated fit with: an untransformed ("") eta -- the common "custom"
  # identity case, an eta transformed by a *known* custom distribution
  # (probitInv/probit), an eta needing extra parameters (boxCox, which isn't
  # auto-handled), and an eta with a transform this function doesn't
  # recognize at all.
  params <- list(
    list(theta = "tid", eta = "eta.id", param = "identity_par", curEval = ""),
    list(theta = "tpi", eta = "eta.pi", param = "probit_par",   curEval = "probitInv"),
    list(theta = "tbc", eta = "eta.bc", param = "boxcox_par",   curEval = "boxCox"),
    list(theta = "tuk", eta = "eta.uk", param = "unknown_par",  curEval = "reallyUnknownTransXyz")
  )
  fake_xpdb <- make_fake_nlmixr2_xpdb(params)

  expect_message(
    dry <- nlmixr2_prm_associations(fake_xpdb, dry_run = TRUE, quiet = FALSE),
    "additional parameters"
  )
  expect_s3_class(dry, "data.frame")
  expect_setequal(dry$param, c("identity_par", "probit_par", "boxcox_par", "unknown_par"))
  # boxCox needs manual handling, and the unrecognized transform is left alone
  expect_true(dry$ignore[dry$param == "boxcox_par"])
  expect_true(dry$ignore[dry$param == "unknown_par"])
  # the untransformed and probitInv-transformed etas both get a custom formula
  expect_false(dry$ignore[dry$param == "identity_par"])
  expect_false(dry$ignore[dry$param == "probit_par"])
})

test_that("nlmixr2_prm_associations warns when a custom transform cannot be resolved in any environment", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  fake_xpdb <- make_fake_nlmixr2_xpdb(list(
    list(theta = "tpi2", eta = "eta.pi2", param = "probit_par2", curEval = "probitInv")
  ))
  was_attached <- detach_rxode2()
  on.exit(reattach_rxode2(was_attached))
  real_ns_env <- rlang::ns_env
  testthat::local_mocked_bindings(
    ns_env = function(pkg, ...) if (identical(pkg, "rxode2")) emptyenv() else real_ns_env(pkg, ...),
    .package = "rlang"
  )

  expect_warning(
    dry <- nlmixr2_prm_associations(fake_xpdb, dry_run = TRUE, quiet = FALSE),
    "not in global or rxode2 environment"
  )
  expect_true(dry$ignore[dry$param == "probit_par2"])
})

test_that("nlmixr2_prm_associations warns when the custom pdist function errors or is non-numeric", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  fake_xpdb_err <- make_fake_nlmixr2_xpdb(list(
    list(theta = "tpi3", eta = "eta.pi3", param = "probit_par3", curEval = "probitInv")
  ))
  was_attached <- detach_rxode2()
  on.exit(reattach_rxode2(was_attached))
  testthat::local_mocked_bindings(probitInv = function(x, ...) stop("mock pdist failure"), .package = "rxode2")
  expect_warning(
    dry_err <- nlmixr2_prm_associations(fake_xpdb_err, dry_run = TRUE, quiet = FALSE),
    "cannot be evaluated without error"
  )
  expect_true(dry_err$ignore[dry_err$param == "probit_par3"])
})

test_that("nlmixr2_prm_associations warns when the custom pdist function returns non-numeric", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  fake_xpdb_nn <- make_fake_nlmixr2_xpdb(list(
    list(theta = "tpi4", eta = "eta.pi4", param = "probit_par4", curEval = "probitInv")
  ))
  was_attached <- detach_rxode2()
  on.exit(reattach_rxode2(was_attached))
  testthat::local_mocked_bindings(probitInv = function(x, ...) "oops", .package = "rxode2")
  expect_warning(
    dry_nn <- nlmixr2_prm_associations(fake_xpdb_nn, dry_run = TRUE, quiet = FALSE),
    "does not return numeric values"
  )
  expect_true(dry_nn$ignore[dry_nn$param == "probit_par4"])
})

test_that("nlmixr2_prm_associations warns when the custom qdist function errors", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  fake_xpdb_qerr <- make_fake_nlmixr2_xpdb(list(
    list(theta = "tpi5", eta = "eta.pi5", param = "probit_par5", curEval = "probitInv")
  ))
  was_attached <- detach_rxode2()
  on.exit(reattach_rxode2(was_attached))
  testthat::local_mocked_bindings(probit = function(x, ...) stop("mock qdist failure"), .package = "rxode2")
  expect_warning(
    dry_qerr <- nlmixr2_prm_associations(fake_xpdb_qerr, dry_run = TRUE, quiet = FALSE),
    "Transformed.*cannot be evaluated"
  )
  expect_true(dry_qerr$ignore[dry_qerr$param == "probit_par5"])
})

test_that("nlmixr2_prm_associations warns when the custom qdist function is not reversible", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  fake_xpdb_qrev <- make_fake_nlmixr2_xpdb(list(
    list(theta = "tpi6", eta = "eta.pi6", param = "probit_par6", curEval = "probitInv")
  ))
  was_attached <- detach_rxode2()
  on.exit(reattach_rxode2(was_attached))
  testthat::local_mocked_bindings(probit = function(x, ...) 42, .package = "rxode2")
  expect_warning(
    dry_qrev <- nlmixr2_prm_associations(fake_xpdb_qrev, dry_run = TRUE, quiet = FALSE),
    "not reversible"
  )
  expect_true(dry_qrev$ignore[dry_qrev$param == "probit_par6"])
})

test_that("nlmixr2_prm_associations returns xpdb unmodified when every association is ignored", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  fake_xpdb_none <- make_fake_nlmixr2_xpdb(list(
    list(theta = "tbc2", eta = "eta.bc2", param = "boxcox_par2",  curEval = "boxCox"),
    list(theta = "tuk2", eta = "eta.uk2", param = "unknown_par2", curEval = "reallyUnknownTransXyz2")
  ))

  expect_message(
    result <- nlmixr2_prm_associations(fake_xpdb_none, quiet = FALSE),
    "No valid associations to add"
  )
  expect_identical(result, fake_xpdb_none)
})


test_that("pure LL fits can be used", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")
  # Likelihood models in nlmixr2 trigger a dependency on 'qs' package
  skip_if_not_installed("qs")

  # From https://github.com/nlmixr2/nlmixr2est/issues/218#issue-1366433669
  markov_nlmixr <- function() {
    ini({
      logitp02 <- logit(0.2) ; label("Probablity of transition from 0 to 2")
      logitp20 <- logit(0.2) ; label("Probablity of transition from 2 to 0")
      eta.p02 ~ 0.1 # need IIV https://github.com/nlmixr2/xpose.nlmixr2/issues/8#issue-3304662799
    })
    model({
      tp02 <- expit(logitp02)
      tp00 <- 1 - tp02
      p02 <- expit(logitp02 + eta.p02)
      p00 <- 1 - p02
      p20 <- expit(logitp20)
      p22 <- 1 - p20

      current_p <-
        p02*(PDV == 0 & DV == 2) +
        p00*(PDV == 0 & DV == 0) +
        p20*(PDV == 2 & DV == 0) +
        p22*(PDV == 2 & DV == 2)
      ll(err) ~ log(current_p)

      # Need pred and res https://github.com/nlmixr2/xpose.nlmixr2/issues/7#issue-3304654465
      # user pop predicted
      pred  <-
        tp02*(PDV == 0 & DV == 2) +
        p00*(PDV == 0 & DV == 0) +
        p20*(PDV == 2 & DV == 0) +
        p22*(PDV == 2 & DV == 2)

      # User pwres
      p0 = p20 + p00
      p2 = p02 + p22
      pipred = 0*p0 + 2*p2
      sdpred = sqrt( p0*(0 - pipred)^2 + p2*(2 - pipred)^2  )
      pwres = (DV - pipred)/sdpred
    })
  }

  d_mod <-
    data.frame(
      ID=rep(1:10, each=11),
      CMT="markov"
    ) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(
      DV=
        dplyr::case_when(
          (ID %% 2) == 1~c(rep(0, 6), rep(2, 5)),
          TRUE~c(rep(0, 3), rep(2, 3), rep(0, 5))
        ),
      PDV=dplyr::lag(DV, 1),
      TIME=seq_len(dplyr::n()) - 2
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(PDV))

  mmfit <- suppressMessages({
    nlmixr2est::nlmixr(object = markov_nlmixr, data = d_mod,
                       est = "focei", control = list(print = 0, outerOpt = "bobyqa"))
  })

  expect_no_error(
    xpose.nlmixr2::xpose_data_nlmixr(mmfit, pred = "pred", wres = "pwres")
  )
  expect_no_error(
    xpose.nlmixr2::xpose_data_nlmixr(mmfit, pred = "pred", wres = "pwres") %>%
      as_xp_xtras()
  )
  expect_no_error(
    xpose.nlmixr2::xpose_data_nlmixr(mmfit, pred = "pred", wres = "pwres") %>%
      attach_nlmixr2(mmfit)
  )
  mm_xpdb <- xpose.nlmixr2::xpose_data_nlmixr(mmfit, pred = "pred", wres = "pwres") %>%
    attach_nlmixr2(mmfit) %>%
    as_xp_xtras()
  expect_true(
    test_nlmixr2_has_fit(mm_xpdb)
  )
  expect_no_error(
    nlmixr2_as_xtra(mmfit, pred = "pred", wres = "pwres", .skip_assoc = TRUE)
  )
  expect_no_error(
    nlmixr2_as_xtra(mmfit, pred = "pred", wres = "pwres",
                    quiet=TRUE, .skip_assoc = FALSE)
  )
  expect_no_warning(
    nlmixr2_as_xtra(mmfit, pred = "pred", wres = "pwres",
                    quiet=TRUE, .skip_assoc = FALSE) %>%
      mutate_prm(the1~plogis,the2~plogis) %>%
      get_prm()
  )

})
