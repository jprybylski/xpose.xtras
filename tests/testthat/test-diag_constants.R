
test_that('diagnose_constants requires xpdb or df', {
  expect_error(diagnose_constants(), 'Need `xpdb` or `df`')
})

test_that('diagnose_constants errors for custom fo_rates', {
  df <- data.frame(KA = 1, ALPHA = 2)
  expect_error(diagnose_constants(df = df, fo_rates = 'custom'), 'Instead of')
})

test_that('diagnose_constants requires single absorption parameter', {
  df <- data.frame(KA = 1)
  expect_error(diagnose_constants(df = df, fo_abs = c('KA','KB')), 'For this check')
})

test_that('diagnose_constants needs matching columns', {
  df <- data.frame(A = 1)
  expect_error(diagnose_constants(df = df, fo_abs = 'KA', micro_pattern = '^X$', vol_pattern = '^Y$'), 'Need some columns')
})

test_that('diagnose_constants volume matches only one column', {
  df <- data.frame(KA = 1, V = 2, V2 = 3)
  expect_error(diagnose_constants(df = df, vol_pattern = '^V'), 'Volume should only match 1')
})

test_that('diagnose_constants checks must be list', {
  df <- data.frame(KA = 1, ALPHA = 2)
  expect_error(diagnose_constants(df = df, checks = 'nope'), 'Checks should be a list')
})

test_that('diagnose_constants invalid check names', {
  df <- data.frame(KA = 1, ALPHA = 2)
  expect_error(diagnose_constants(df = df, checks = list(bad = TRUE)), 'Invalid check requested')
})

test_that('diagnose_constants errors when requested check cannot run', {
  df <- data.frame(KA = 1)
  expect_error(diagnose_constants(df = df, checks = list(flip_flop = TRUE)), 'Needed info not available')
  expect_error(diagnose_constants(df = df, checks = list(neg_microvol = TRUE)), 'Needed info not available')
  expect_error(diagnose_constants(df = df, checks = list(units_match = TRUE)), 'Needed info not available')
  # In the case where all false (results in no matching columns0:
  all_false <- eval(formals(diagnose_constants)$checks) %>% purrr::map(~FALSE)
  expect_error(diagnose_constants(df = df, checks = all_false), 'Need some columns')
  # In the case where no valid test after checks
  expect_warning(diagnose_constants(df = df, checks =
                                      modifyList(all_false,
                                                 list(flip_flop = NULL),
                                                 keep.null = TRUE)),
                 'No valid checks')
})

test_that('diagnose_constants flip_flop check works', {
  df_bad <- data.frame(KA = 0.5, ALPHA = 1)
  suppressMessages(expect_message(
    diagnose_constants(
      df = df_bad,
      fo_rates = 'ALPHA',
      checks = list(flip_flop = TRUE, neg_microvol = FALSE, units_match = FALSE)
    ),
    'suggestive of flip-flop'
  ))
  df_good <- data.frame(KA = 1, ALPHA = 0.5)
  suppressMessages(expect_message(
    diagnose_constants(
      df = df_good,
      fo_rates = 'ALPHA',
      checks = list(flip_flop = TRUE, neg_microvol = FALSE, units_match = FALSE)
    ),
    'not suggestive of flip-flop'
  ))
})

test_that('diagnose_constants detects negative microconstants or volumes', {
  df_bad <- data.frame(KA = 1, KEL = -0.2, V = 5)
  suppressMessages(expect_message(
    diagnose_constants(
      df = df_bad,
      fo_abs = 'KA',
      micro_pattern = '^K',
      vol_pattern = '^V$',
      checks = list(flip_flop = FALSE, neg_microvol = TRUE, units_match = FALSE)
    ),
    'negative microconstants or volumes'
  ))
  df_good <- data.frame(KA = 1, KEL = 0.2, V = 5)
  suppressMessages(expect_message(
    diagnose_constants(
      df = df_good,
      fo_abs = 'KA',
      micro_pattern = '^K',
      vol_pattern = '^V$',
      checks = list(flip_flop = FALSE, neg_microvol = TRUE, units_match = FALSE)
    ),
    'do not have negative microconstants or volumes'
  ))
})

test_that('diagnose_constants checks unit consistency', {
  df <- data.frame(KA = 1, ALPHA = 2)
  bad_units <- list(KA = '1/hr', ALPHA = '1/min')
  suppressMessages(expect_message(
    diagnose_constants(
      df = df,
      fo_rates = 'ALPHA',
      checks = list(flip_flop = TRUE, neg_microvol = FALSE, units_match = TRUE),
      df_units = bad_units
    ),
    "Units don't match"
  ))
  good_units <- list(KA = '1/hr', ALPHA = '1/hr')
  suppressMessages(expect_message(
    diagnose_constants(
      df = df,
      fo_rates = 'ALPHA',
      checks = list(flip_flop = TRUE, neg_microvol = FALSE, units_match = TRUE),
      df_units = good_units
    ),
    'All relevant units seem to match'
  ))
})

test_that('derive_prm requires rxode2', {
  skip_if(requireNamespace('rxode2', quietly = TRUE) &&
    'rxDerived' %in% getNamespaceExports('rxode2'))
  expect_error(derive_prm(xpdb = 1), 'Need `rxode2`')
})

test_that('backfill_derived requires rxode2', {
  skip_if(requireNamespace('rxode2', quietly = TRUE) &&
    'rxDerived' %in% getNamespaceExports('rxode2'))
  expect_error(backfill_derived(xpdb = 1), 'Need `rxode2`')
})

test_that('derive_prm adds derived parameters', {
  skip_if_not_installed('rxode2')
  skip_if(!'rxDerived' %in% getNamespaceExports('rxode2'))
  orig <- xpose::get_data(pheno_base, quiet = TRUE)
  expect_error(derive_prm(pheno_base, quiet=TRUE),
               "Need to declare.*prm.*at least one.*param")
  expect_no_error(derive_prm(xpdb_x, quiet=TRUE),
               message = "Need to declare.*prm.*at least one.*param")
  derived <- derive_prm(pheno_base, .prm = c(CL, V), quiet=TRUE)
  expect_gt(ncol(derived), ncol(orig))
  pref <- derive_prm(pheno_base, .prm = c(CL, V), prefix = 'calc_', quiet=TRUE)
  expect_true(any(grepl('^calc_', names(pref))))
})

test_that('backfill_derived augments xpdb with derived parameters', {
  skip_if_not_installed('rxode2')
  skip_if(!'rxDerived' %in% getNamespaceExports('rxode2'))
  xp1 <- nlmixr2_m3 %>% set_option(quiet=TRUE)
  orig_cols <- names(xpose::get_data(xp1, quiet = TRUE))
  xp2 <- backfill_derived(xp1, .prm = c(CL, V))
  new_cols <- names(xpose::get_data(xp2, quiet = TRUE))
  expect_gt(length(new_cols), length(orig_cols))
  orig_params <- xp_var(xp1, 1, type = 'param')$col
  new_params <- xp_var(xp2, 1, type = 'param')$col
  expect_gt(length(new_params), length(orig_params))
})

test_that("diagnose_constants accepts xpose data", {
  xp1 <- set_option(nlmixr2_m3 ,quiet=TRUE)
  suppressMessages(expect_no_error(diagnose_constants(xp1)))
  bf_xp1 <- backfill_derived(xp1)
  expect_error(diagnose_constants(bf_xp1),
               "Volume.*match.*1.*column")
  suppressMessages(expect_no_error(diagnose_constants(bf_xp1, vol_pattern = "^V$")))
  # Just make sure vector (meaningless in this case) can be passed to volume
  suppressMessages(expect_no_error(diagnose_constants(bf_xp1, vol_pattern = c("^V$","^$"))))
})
