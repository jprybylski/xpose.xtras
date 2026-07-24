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


test_that("join_backfill coalesces shared columns instead of suffixing them", {
  x <- tibble::tibble(id = 1:3, val = c(1, NA, 3))
  y <- tibble::tibble(id = 1:3, val = c(10, 20, 30), extra = c("a", "b", "c"))

  out <- join_backfill(x, y, by = "id")

  # Missing values are backfilled, existing values are untouched
  expect_equal(out$val, c(1, 20, 3))
  # No .x/.y suffix columns leak through
  expect_false(any(grepl("\\.[xy]$", names(out))))
  # Columns only present in y are still brought in as-is
  expect_equal(out$extra, c("a", "b", "c"))

  # A plain left_join would have produced val.x/val.y instead
  plain <- dplyr::left_join(x, y, by = "id")
  expect_true(all(c("val.x", "val.y") %in% names(plain)))

  # Custom suffixes are honored and still fully consumed
  out_suffix <- join_backfill(x, y, by = "id", suffix = c("_x", "_y"))
  expect_equal(out_suffix$val, c(1, 20, 3))
  expect_false(any(grepl("_[xy]$", names(out_suffix))))

  # No shared non-key columns: behaves like a plain left_join
  y_no_overlap <- tibble::tibble(id = 1:3, extra = c("a", "b", "c"))
  expect_identical(
    join_backfill(x, y_no_overlap, by = "id"),
    dplyr::left_join(x, y_no_overlap, by = "id")
  )

  # Empty suffixes short-circuit to the plain left_join result
  expect_identical(
    join_backfill(x, y_no_overlap, by = "id", suffix = c("", "")),
    dplyr::left_join(x, y_no_overlap, by = "id", suffix = c("", ""))
  )

  # keep = TRUE duplicates the join key with the suffix pattern too, so it
  # gets coalesced back into a single key column like any other shared column
  out_keep <- join_backfill(x, y, by = "id", keep = TRUE)
  expect_equal(out_keep$id, 1:3)
  expect_false(any(grepl("\\.[xy]$", names(out_keep))))
})

test_that("left_join_x() backfills a partially-missing variable via a join key", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # WT is missing for two subjects
  xpdb_missing <- mutate_x(
    xpdb_ex_pk,
    WT = dplyr::if_else(ID %in% c("110", "112"), NA, WT)
  )
  wt_lookup <- xpose::get_data(xpdb_ex_pk, quiet = TRUE) %>%
    dplyr::distinct(ID, WT)

  filled <- left_join_x(xpdb_missing, wt_lookup, by = "ID")

  # Class is unchanged (xpdb_ex_pk is a plain xpose_data object)
  expect_identical(class(filled), class(xpdb_ex_pk))

  d_orig <- xpose::get_data(xpdb_ex_pk, quiet = TRUE)
  d_missing <- xpose::get_data(xpdb_missing, quiet = TRUE)
  d_filled <- xpose::get_data(filled, quiet = TRUE)

  expect_true(any(is.na(d_missing$WT)))
  expect_false(any(is.na(d_filled$WT)))
  expect_equal(d_filled$WT, d_orig$WT)
  expect_false(any(grepl("\\.[xy]$", names(d_filled))))

  # Invalid .problem is rejected the same way as the other _x functions
  expect_error(
    left_join_x(xpdb_missing, wt_lookup, by = "ID", .problem = 99),
    "99"
  )

  # Explicit .problem restricts which problem's data is joined into;
  # xpdb_ex_pk has two problems, so problem 2 is left untouched here
  scoped <- left_join_x(xpdb_missing, wt_lookup, by = "ID", .problem = 1)
  expect_identical(scoped$data$data[[2]], xpdb_missing$data$data[[2]])
  expect_false(identical(scoped$data$data[[1]], xpdb_missing$data$data[[1]]))
})

test_that("left_join_x() preserves the class of its input (xpose_data stays xpose_data, xp_xtras stays xp_xtras)", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  lookup <- xpose::get_data(xpdb_ex_pk, quiet = TRUE) %>%
    dplyr::distinct(ID, WT)

  # Plain xpose_data in -> plain xpose_data out (not promoted to xp_xtras)
  plain_out <- left_join_x(xpdb_ex_pk, lookup, by = "ID")
  expect_identical(class(plain_out), c("xpose_data", "uneval"))
  expect_false(is_xp_xtras(plain_out))

  # xp_xtras in -> xp_xtras out
  xtras_out <- left_join_x(as_xpdb_x(xpdb_ex_pk), lookup, by = "ID")
  expect_identical(class(xtras_out), c("xp_xtras", "xpose_data", "uneval"))
  expect_true(is_xp_xtras(xtras_out))
})

test_that("left_join() S3 method dispatches like left_join_x() for xpose_data and xp_xtras", {
  apgr_lookup <- xpose::get_data(pheno_base, quiet = TRUE) %>%
    dplyr::distinct(ID, APGR)
  xpdb_missing <- mutate_x(
    pheno_base,
    APGR = dplyr::if_else(ID %in% c("1", "2"), NA, APGR)
  )

  # pheno_base is already xp_xtras; dispatch relies on xpose_data inheritance
  expect_true(is_xp_xtras(xpdb_missing))
  via_generic <- dplyr::left_join(xpdb_missing, apgr_lookup, by = "ID")
  via_x <- left_join_x(xpdb_missing, apgr_lookup, by = "ID")
  expect_identical(via_generic, via_x)
  expect_true(is_xp_xtras(via_generic))
  expect_false(any(is.na(xpose::get_data(via_generic, quiet = TRUE)$APGR)))

  # Also works starting from a plain xpose_data object
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  wt_lookup <- xpose::get_data(xpdb_ex_pk, quiet = TRUE) %>%
    dplyr::distinct(ID, WT)
  xpdb_missing_plain <- mutate_x(
    xpdb_ex_pk,
    WT = dplyr::if_else(ID %in% c("110", "112"), NA, WT)
  )
  expect_false(is_xp_xtras(xpdb_missing_plain))
  expect_identical(
    dplyr::left_join(xpdb_missing_plain, wt_lookup, by = "ID"),
    left_join_x(xpdb_missing_plain, wt_lookup, by = "ID")
  )
})

test_that("left_join_x() updates the index when the join introduces a new column", {
  id_lookup <- xpose::get_data(pheno_base, quiet = TRUE) %>%
    dplyr::distinct(ID) %>%
    dplyr::mutate(NEWVAR = as.numeric(ID))

  joined <- left_join_x(pheno_base, id_lookup, by = "ID")

  expect_true("NEWVAR" %in% get_index(joined)$col)
  expect_true("NEWVAR" %in% names(xpose::get_data(joined, quiet = TRUE)))
})

test_that("patch_condn corrects the condition number for multi-method runs (issue #60)", {
  # pheno_saem's run has SAEM followed by importance sampling, each with its own
  # 'EIGENVALUES OF COR MATRIX OF ESTIMATE' block; xpose's sum_condn() always used
  # the first block (SAEM) instead of the last (importance sampling, the final
  # estimate), so the correct condn is max/min of the *last* block: 1.77/0.21
  expected <- as.character(round(1.77 / 0.21, pheno_saem$xp_theme$rounding))

  # Force a known-wrong value so this doesn't depend on whether the bundled
  # `pheno_saem` data was (re-)built before or after this patch existed
  corrupted <- pheno_saem
  corrupted$summary$value[corrupted$summary$label == "condn"] <- "0"

  patched <- patch_condn(corrupted)
  expect_equal(
    patched$summary$value[patched$summary$label == "condn"],
    expected
  )

  # automatically applied through as_xpdb_x()
  reconverted <- as_xpdb_x(corrupted)
  expect_equal(
    reconverted$summary$value[reconverted$summary$label == "condn"],
    expected
  )

  # single-method models (one EIGENVALUES block) are unaffected
  expect_identical(
    patch_condn(xpose::xpdb_ex_pk) %>% xpose::get_summary(),
    xpose::get_summary(xpose::xpdb_ex_pk)
  )
})

test_that("print.xpose_plot() auto-applies configured defaults via auto_apply_defaults()", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  p <- xpose::dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)

  called_with <- NULL
  testthat::local_mocked_bindings(
    auto_apply_defaults = function(plot, xpdb = NULL) {
      called_with <<- plot
      plot
    }
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(p)

  expect_identical(called_with, p)
})
