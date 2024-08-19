test_that("set_var_types with tidyselect", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_error(set_var_types_x(xpdb_ex_pk, .problem = 99)) # trivial, found in part copied from xpose

  # Strip covariates
  xpdb_base <- xpose::set_var_types(
    xpdb_ex_pk,
    .problem = 1,
    na = c("CLCR", "AGE", "WT", "SEX", "MED1", "MED2")
  )

  xpdb_2 <- set_var_types_x(
    xpdb_base, .problem = 1,
    idv = TAD,
    catcov = c(starts_with("MED")),
    contcov = c(CLCR,AGE)
  )

  expect_error(xpose::xp_var(xpdb_base, .problem = 1, type="catcov"))
  expect_error(xpose::xp_var(xpdb_base, .problem = 1, type="contcov"))
  expect_no_message(xpose::xp_var(xpdb_2, .problem = 1, type="catcov"))

  xpose::xp_var(xpdb_2, .problem = 1, type="catcov") %>%
    dplyr::pull(col) %>%
    expect_setequal(c("MED1", "MED2"))

  xpose::xp_var(xpdb_2, .problem = 1, type="contcov") %>%
    dplyr::pull(col) %>%
    expect_setequal(c("CLCR", "AGE"))

  xpose::xp_var(xpdb_2, .problem = 1, type="idv") %>%
    dplyr::pull(col) %>%
    expect_setequal(c("TAD"))

  xpdb_3 <- set_var_types_x(
    xpdb_base,
    contcov = c(CLCR, AGE, WT)
  )

  expect_identical(
    xpose::xp_var(xpdb_ex_pk, .problem = 1, type="contcov"),
    xpose::xp_var(xpdb_3, .problem = 1, type="contcov")
  )

})


# imported from patch fork for irep
test_that('irep works properly', {
  expect_message(irep_out <- irep(rep(1:5, time = 3), quiet = FALSE),
                 regexp = '3 simulations found')
  expect_equal(irep_out, rep(1:3, each = 5))
  expect_message(irep_out2 <- irep(rep(c(10,5,6), time = 7), quiet = FALSE),
                 regexp = '7 simulations found')
  expect_equal(irep_out2, rep(1:7, each = 3))

  # Trivial errors
  expect_error(irep())
  expect_identical(
    irep(rep(1:5, time = 3), quiet = TRUE),
    irep(factor(paste(rep(1:5, time = 3))), quiet = TRUE)
  )
  expect_identical(
    irep(factor(paste(rep(1:5, time = 3))), quiet = TRUE),
    irep(c(paste(rep(1:5, time = 3))), quiet = TRUE) # factor not really relevant, demo
  )

})

