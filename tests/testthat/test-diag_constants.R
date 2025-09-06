
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
})

test_that('diagnose_constants flip_flop check works', {
  withr::local_options(list(cli.num_colors = 1))
  df_bad <- data.frame(KA = 0.5, ALPHA = 1)
  expect_output(
    diagnose_constants(
      df = df_bad,
      fo_rates = 'ALPHA',
      checks = list(flip_flop = TRUE, neg_microvol = FALSE, units_match = FALSE)
    ),
    'suggestive of flip-flop'
  )
  df_good <- data.frame(KA = 1, ALPHA = 0.5)
  expect_output(
    diagnose_constants(
      df = df_good,
      fo_rates = 'ALPHA',
      checks = list(flip_flop = TRUE, neg_microvol = FALSE, units_match = FALSE)
    ),
    'not suggestive of flip-flop'
  )
})

test_that('diagnose_constants detects negative microconstants or volumes', {
  withr::local_options(list(cli.num_colors = 1))
  df_bad <- data.frame(KA = 1, KEL = -0.2, V = 5)
  expect_output(
    diagnose_constants(
      df = df_bad,
      fo_abs = 'KA',
      micro_pattern = '^K',
      vol_pattern = '^V$',
      checks = list(flip_flop = FALSE, neg_microvol = TRUE, units_match = FALSE)
    ),
    'negative microconstants or volumes'
  )
  df_good <- data.frame(KA = 1, KEL = 0.2, V = 5)
  expect_output(
    diagnose_constants(
      df = df_good,
      fo_abs = 'KA',
      micro_pattern = '^K',
      vol_pattern = '^V$',
      checks = list(flip_flop = FALSE, neg_microvol = TRUE, units_match = FALSE)
    ),
    'do not have negative microconstants or volumes'
  )
})

test_that('diagnose_constants checks unit consistency', {
  withr::local_options(list(cli.num_colors = 1))
  df <- data.frame(KA = 1, ALPHA = 2)
  bad_units <- list(KA = '1/hr', ALPHA = '1/min')
  expect_output(
    diagnose_constants(
      df = df,
      fo_rates = 'ALPHA',
      checks = list(flip_flop = TRUE, neg_microvol = FALSE, units_match = TRUE),
      df_units = bad_units
    ),
    "Units don't match"
  )
  good_units <- list(KA = '1/hr', ALPHA = '1/hr')
  expect_output(
    diagnose_constants(
      df = df,
      fo_rates = 'ALPHA',
      checks = list(flip_flop = TRUE, neg_microvol = FALSE, units_match = TRUE),
      df_units = good_units
    ),
    'All relevant units seem to match'
  )
})
