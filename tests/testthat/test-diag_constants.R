
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
