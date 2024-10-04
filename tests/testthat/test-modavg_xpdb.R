test_that("model averaging xpdb (modavg_xpdb) works", {

  #selection
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(run3, .lineage = TRUE, avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(run3,run8,run9, avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(run15~run7+run6, avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(run5:run8, avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )

  # Test setup for calculations and algorithm
  backfilled_set <- pheno_set %>% focus_qapply(backfill_iofv)
  random_two <- sample(length(backfilled_set),2)
  subsetted <- backfilled_set[random_two]
  # Two individuals with one having lower ofv in mod1 and the other having higher
  # test algorithm calcs

})
