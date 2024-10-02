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


test_that("edit_xpose_data is essentially the same as in xpose, with some improvement", {
  ## Some basic behavior tests and trivial error checking, to cover all bases and get desired coverage

  expect_identical(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                    NEWCOLUMN = 1),
    xpose::edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                           NEWCOLUMN = 1) %>% as_xp_xtras()
  )
  expect_identical(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = xpose::xpdb_ex_pk,
                    NEWCOLUMN = 1),
    xpose::edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = xpose::xpdb_ex_pk,
                           NEWCOLUMN = 1)
  )
  dynamic_column <- "TIME"
  expect_error(
    xpose::edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                           NEWCOLUMN = .data[[dynamic_column]]/24),
    "missing.*\\.data"
  )
  expect_no_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                    NEWCOLUMN = .data[[dynamic_column]]/24),
    message="missing.*\\.data"
  )
  expect_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                    NEWCOLUMN = .data[[dynamic_column]]/24, check_quos = TRUE),
    "missing.*\\.data"
  )
  expect_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                    NEWCOLUMN = .data[[dynamic_column]]/24, .problem=99),
    "99"
  )
  expect_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                    NEWCOLUMN = .data[[dynamic_column]]/24, .source=letters),
    "length 1"
  )

  expect_identical(
    mutate_x(pheno_base, NEWCOLUMN = 1),
    xpose::mutate(pheno_base, NEWCOLUMN = 1) %>% as_xp_xtras()
  )

  expect_identical(
    rename_x(pheno_base, DV2 = DV),
    xpose::rename(pheno_base, DV2 = DV) %>% as_xp_xtras()
  )

  expect_identical(
    group_by_x(pkpd_m3, DOSE),
    xpose::group_by(pkpd_m3, DOSE) %>% as_xp_xtras()
  )
  # bug in ungroup?
  expect_failure(expect_identical(
    group_by_x(pkpd_m3, DOSE) %>% ungroup_x(),
    xpose::group_by(pkpd_m3, DOSE) %>% xpose::ungroup() %>% as_xp_xtras()
  ))
  expect_failure(expect_false(
    xpose::group_by(pkpd_m3, DOSE) %>% xpose::ungroup() %>%
      xpose::get_data(quiet = TRUE) %>% dplyr::is_grouped_df()
  ))
  expect_false( # xtra version does not fail this
    group_by_x(pkpd_m3, DOSE) %>% ungroup_x() %>%
      xpose::get_data(quiet = TRUE) %>% dplyr::is_grouped_df()
  )

  special_xpdb <- xpdb_x
  special_xpdb$special <- special_xpdb$data %>%
    dplyr::mutate(method="vpc")
  special_xpdb <- as_xp_xtras(special_xpdb)
  suppressWarnings(suppressMessages(expect_warning(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = special_xpdb,
                    NEWCOLUMN = 1, .source = "special", .where="data"),
    "elements data not found"
  )))
  expect_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = special_xpdb,
                    NEWCOLUMN = 1, .source = "special", .where="data", .problem=99),
    "99"
  )
  special_xpdb$special <- special_xpdb$data %>%
    dplyr::mutate(method="fakemethod")
  special_xpdb <- as_xp_xtras(special_xpdb)
  suppressMessages(expect_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = special_xpdb,
                    NEWCOLUMN = 1, .source = "special", .where="data"),
    "fakemethod"
  ))

  expect_no_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                    NEWCOLUMN = OBJ, .source = "phi")
  )
  expect_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                    NEWCOLUMN = OBJ, .source = "fake")
  )
  expect_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                    NEWCOLUMN = OBJ, .source = "phi", .problem = 99)
  )
  expect_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = pheno_base,
                    NEWCOLUMN = .data[["OBJ"]], .source = "phi", check_quos = TRUE)
  )

})
