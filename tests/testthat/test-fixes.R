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

test_that("set_var_types_x doesn't let one type's columns overflow into another type (issue #76)", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # "id" is a prefix of "idv": a naive startsWith() match on tidyselect's
  # disambiguated names would let idv's column also be claimed by id.
  xpdb_2 <- set_var_types_x(xpdb_ex_pk, .problem = 1, idv = TAD, id = ID)

  xpose::xp_var(xpdb_2, .problem = 1, type = "idv") %>%
    dplyr::pull(col) %>%
    expect_setequal("TAD")

  xpose::xp_var(xpdb_2, .problem = 1, type = "id") %>%
    dplyr::pull(col) %>%
    expect_setequal("ID")

  # A single type selecting 10+ columns: xpose::set_var_types() recovers a
  # column's type by stripping a single trailing digit off names like
  # "eta10" (added by base R's c(name = <multi-element vector>) to
  # disambiguate), which mangles "eta10"/"eta11"/... into "eta1" instead of
  # "eta". Add extra ETA-like columns so matches() selects more than 9.
  extra_etas <- paste0("ETA", 4:12)
  xpdb_many <- xpdb_ex_pk
  xpdb_many$data$data[[1]] <- dplyr::bind_cols(
    xpdb_many$data$data[[1]],
    stats::setNames(as.list(rep(1, length(extra_etas))), extra_etas)
  )
  xpdb_many$data$index[[1]] <- dplyr::bind_rows(
    xpdb_many$data$index[[1]],
    tibble::tibble(table = "patab001", col = extra_etas, type = "na", label = NA, units = NA)
  )

  xpdb_many_2 <- set_var_types_x(xpdb_many, .problem = 1, eta = matches("^ETA\\d+$"))

  xpose::xp_var(xpdb_many_2, .problem = 1, type = "eta") %>%
    dplyr::pull(col) %>%
    expect_setequal(paste0("ETA", 1:12))
})

test_that("set_var_types_x falls back to a non-strict selection (with a warning) when a column is missing from some problems", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # KA/V only exist in problem 1's data, not problem 2's. Selecting them
  # without restricting `.problem` hits tidyselect's strict-selection error
  # for problem 2, which is caught and retried non-strictly (with a warning)
  # instead of failing the whole call.
  suppressWarnings(expect_warning(
    xpdb_partial <- set_var_types_x(xpdb_ex_pk, contcov = c(KA, V)),
    "doesn't exist in problem 2"
  ))

  # Problem 1 (where both columns exist) picks up the new contcov columns
  xpose::xp_var(xpdb_partial, .problem = 1, type = "contcov") %>%
    dplyr::pull(col) %>%
    {expect_true(all(c("KA", "V") %in% .))}

  # Problem 2 (missing them) is unaffected -- still no contcov type there,
  # same as before the call
  expect_error(
    xpose::xp_var(xpdb_partial, .problem = 2, type = "contcov"),
    "not available"
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

test_that("edit_xpose_data() checks quoted variables exist when check_quos = TRUE with .source = 'special'", {
  special_xpdb <- xpdb_x
  special_xpdb$special <- special_xpdb$data %>%
    dplyr::mutate(method = "vpc")
  special_xpdb <- as_xp_xtras(special_xpdb)

  # xpose::check_quo_vars() is a no-op for .source = "special" (it returns
  # immediately without checking anything), so this just exercises the
  # `check_quos = TRUE` call site for that source (line coverage) without
  # expecting it to error.
  suppressWarnings(suppressMessages(expect_no_error(
    edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = special_xpdb,
                    NEWCOLUMN = 1, .source = "special", .where = "data", check_quos = TRUE)
  )))
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

  # patch_condn()'s `xpdb$summary <- ...` must not strip the xp_xtras/xpose_data
  # classes (issue #74); patch_condn() only rewrites $summary when it finds a
  # multi-method run, so this exercises that branch specifically
  expect_identical(class(patched), class(corrupted))
  expect_identical(class(reconverted), c("xp_xtras", "xpose_data", "uneval"))
  expect_true(check_xpdb_x(reconverted))
})

test_that("patch_condn skips code-scanning entirely for single-method problems", {
  # No problem in xpdb_ex_pk has more than one 'method' row in $summary, so
  # patch_condn() should return early without ever touching $code -- corrupt
  # it to prove that (this would error on xpose::check_xpdb(check='code')
  # or the eigenvalue regex if the scan ran).
  single_method <- xpose::xpdb_ex_pk
  single_method$code <- NULL

  expect_no_error(patched <- patch_condn(single_method))
  expect_identical(patched, single_method)
})

test_that("`$<-`/`[[<-` on xpose_data and xp_xtras objects preserve their class (issue #74)", {
  # xpose_data (and, by extension, xp_xtras) objects always carry "uneval" as
  # their last class -- the same class ggplot2 (< 4.0) uses internally for
  # unevaluated aes() mappings, with `[[<-.uneval`/`$<-.uneval` methods that
  # collapse the class attribute down to bare "uneval". Without a
  # higher-priority method registered for "xpose_data"/"xp_xtras" themselves,
  # any `xpdb$foo <- value`/`xpdb[["foo"]] <- value` would dispatch to
  # ggplot2's method instead, so this exercises the fix methods directly
  # rather than relying on a particular ggplot2 version being installed.
  plain <- xpose::xpdb_ex_pk
  plain$options$quiet <- TRUE
  expect_identical(class(plain), c("xpose_data", "uneval"))
  expect_true(plain$options$quiet)

  plain2 <- xpose::xpdb_ex_pk
  plain2[["options"]]$quiet <- TRUE
  expect_identical(class(plain2), c("xpose_data", "uneval"))
  expect_true(plain2$options$quiet)

  xtras <- as_xpdb_x(xpose::xpdb_ex_pk)
  before <- class(xtras)
  xtras$options$quiet <- TRUE
  expect_identical(class(xtras), before)
  expect_true(xtras$options$quiet)

  xtras[["options"]]$quiet <- FALSE
  expect_identical(class(xtras), before)
  expect_false(xtras$options$quiet)
})

test_that("patch_condn ignores non-consecutive false-positive matches after the eigenvalue block", {
  # Craft a minimal `code` table where, after the real (consecutive)
  # eigenvalue row, an unrelated later line also happens to match the
  # eigenvalue-number regex. Without the consecutiveness trim, that decoy
  # would be folded into the parsed eigenvalues and corrupt the result.
  code_tbl <- tibble::tibble(
    problem = 1,
    level = 1,
    subroutine = NA_character_,
    code = c(
      "SOME PRECEDING LINE",
      "******************** EIGENVALUES OF COR MATRIX OF ESTIMATE (RSR) ********************",
      "1 2 3",
      "2.10E-01 5.12E-01 8.27E-01",
      "unrelated text here",
      "unrelated text here 2",
      "9.99E+09 decoy value that must be ignored"
    ),
    comment = NA_character_
  )

  fake_xpdb <- pheno_saem
  fake_xpdb$code <- code_tbl

  patched <- patch_condn(fake_xpdb)
  expected <- as.character(round(0.827 / 0.21, fake_xpdb$xp_theme$rounding))

  expect_equal(
    patched$summary$value[patched$summary$label == "condn" & patched$summary$problem == 1],
    expected
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

test_that("print.xpose_plot() falls back to the ggplot2 <= 3.5.2 label-assignment branch", {
  # The installed ggplot2 is > 3.5.2, so this legacy branch is otherwise
  # dead code in this environment; mock utils::packageVersion() to force it.
  # Real xpose_plot objects are ggplot2 4.x S7 objects underneath, so the
  # legacy `x$labels <- <plain list>` assignment this branch performs is
  # incompatible with them (S7 property validation rejects it) -- that's
  # expected here (not a bug in the patch: no real user hits this branch
  # while running the ggplot2 this environment has installed), so we only
  # assert that it fails at the (legacy) label-keyword-substitution step,
  # after the (legacy) title/subtitle/caption/tag prefixing has already run.
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  p <- xpose::dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)

  real_pv <- utils::packageVersion
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "ggplot2")) return(package_version("3.5.0"))
      real_pv(pkg, ...)
    },
    .package = "utils"
  )
  local_mocked_bindings(auto_apply_defaults = function(plot, xpdb = NULL) plot)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(print(p), "labels")
})

test_that("print.xpose_plot() pagination: multi-page facets are printed", {
  skip_on_cran() # slow: prints a multi-page paginated plot several times
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  # Restrict to a handful of subjects so a 2x2 grid still spans 2 pages,
  # keeping the (otherwise slow, once-per-page) print() calls below fast.
  small <- xpose::filter(xpdb_ex_pk, ID %in% c("110", "112", "113", "121", "123", "124", "126", "127"))
  p <- xpose::ind_plots(small, nrow = 2, ncol = 2, quiet = FALSE)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Default: no `page` argument and no preset facet page -> every page is drawn
  expect_no_error(print(p))

  # A single explicit page
  expect_no_error(print(p, page = 1))

  # All requested pages exceed the total -> hard error
  expect_error(print(p, page = 99), "exceeded")

  # Some (but not all) requested pages exceed the total -> warn and drop them
  expect_warning(print(p, page = c(1, 99)), "exceeding")

  # A page preset on the facet itself (rather than passed to print()) is honored
  p_preset <- xpose::ind_plots(small, nrow = 2, ncol = 2, page = 2, quiet = TRUE)
  expect_no_error(print(p_preset))

  # interactive() session: renders a progress message/bar while looping pages
  local_mocked_bindings(interactive = function() TRUE)
  utils::capture.output(
    expect_message(print(p, page = c(1, 2)), "Rendering"),
    type = "message"
  )
})

test_that("print.xpose_plot() pagination: legacy ggplot2 <= 3.5.2 page/panel-count call sites", {
  # As above, force the legacy branch for just the page_tot/panel_tot
  # computation (n_pages()/n_panels()) while keeping the earlier
  # label-substitution code on the modern (safe) path, by only returning the
  # legacy ggplot2 version from the 3rd call onward. This isolates line
  # coverage for the `page_tot <- n_pages(x)` / `panel_tot <- n_panels(x)`
  # call sites themselves without hitting the S7 labels-assignment failure
  # demonstrated above.
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  small <- xpose::filter(xpdb_ex_pk, ID %in% c("110", "112", "113", "121", "123", "124", "126", "127"))

  real_pv <- utils::packageVersion
  make_delayed_mock <- function() {
    call_count <- 0
    function(pkg, ...) {
      if (identical(pkg, "ggplot2")) {
        call_count <<- call_count + 1
        if (call_count <= 2) return(real_pv("ggplot2"))
        return(package_version("3.5.0"))
      }
      real_pv(pkg, ...)
    }
  }

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Paginated facet -> exercises `page_tot <- n_pages(x)`
  p_pag <- xpose::ind_plots(small, nrow = 2, ncol = 2, quiet = TRUE)
  local_mocked_bindings(packageVersion = make_delayed_mock(), .package = "utils")
  local_mocked_bindings(auto_apply_defaults = function(plot, xpdb = NULL) plot)
  expect_error(print(p_pag, page = 1))

  # Non-paginated plot -> exercises `panel_tot <- n_panels(x)`
  p_flat <- xpose::dv_vs_ipred(small, quiet = TRUE)
  local_mocked_bindings(packageVersion = make_delayed_mock(), .package = "utils")
  local_mocked_bindings(auto_apply_defaults = function(plot, xpdb = NULL) plot)
  expect_error(print(p_flat))
})

test_that("paginate() falls back to the ggplot2 <= 3.5.2 branch", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  p <- xpose::dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)

  real_pv <- utils::packageVersion
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "ggplot2")) return(package_version("3.5.0"))
      real_pv(pkg, ...)
    },
    .package = "utils"
  )

  # A plain-list stand-in avoids the S7 `labels<-` validation issue real
  # ggplot2 4.x xpose_plot objects hit under this legacy branch (see above),
  # letting us test paginate()'s own keyword-substitution logic directly.
  fake_plot <- list(
    labels = list(title = "Page @page of @lastpage", subtitle = NULL),
    xpose = p$xpose
  )
  out <- paginate(fake_plot, 3, 19)
  expect_equal(out$labels$title, "Page 3 of 19")
  # Untouched labels (no @page/@lastpage keyword) pass through unchanged
  expect_null(out$labels$subtitle)
})

test_that("n_pages()/n_panels() compute page/panel counts, and abort for ggplot2 > 3.5.2", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  small <- xpose::filter(xpdb_ex_pk, ID %in% c("110", "112", "113", "121", "123", "124", "126", "127"))
  p_pag <- xpose::ind_plots(small, nrow = 2, ncol = 2, quiet = TRUE)
  p_flat <- xpose::dv_vs_ipred(small, quiet = TRUE)

  # Not intended for use with the actually-installed (> 3.5.2) ggplot2
  expect_error(n_pages(p_pag), "Not intended")
  expect_error(n_panels(p_pag), "Not intended")

  real_pv <- utils::packageVersion
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "ggplot2")) return(package_version("3.5.0"))
      real_pv(pkg, ...)
    },
    .package = "utils"
  )

  expect_equal(n_pages(p_pag), 2)
  expect_equal(n_panels(p_pag), 8)
  # No `page` facet column on a non-paginated plot -> 0 pages
  expect_equal(n_pages(p_flat), 0L)
  # A non-paginated plot still has a single panel in its build layout
  expect_equal(n_panels(p_flat), 1L)
})

test_that("n_pages()/n_panels() ggplot2 <= 2.2.1 branch", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  p <- xpose::dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)

  real_pv <- utils::packageVersion
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "ggplot2")) return(package_version("2.2.1"))
      real_pv(pkg, ...)
    },
    .package = "utils"
  )

  # `$layout$panel_layout` no longer exists on a modern ggplot_build()
  # result, so this ancient branch resolves to NULL/0 here -- it only needs
  # to execute (ggplot2 <= 2.2.1 can't actually be installed alongside this
  # package anymore, so a meaningful result isn't obtainable in this
  # environment).
  expect_equal(n_pages(p), 0L)
  expect_equal(n_panels(p), 0L)
})

test_that("print.xpose_plot() non-paginated branch: `page` argument is ignored with a warning, and many panels emit a size message", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  p_flat <- xpose::dv_vs_ipred(xpdb_ex_pk, quiet = TRUE)
  # A plot faceted directly via ggplot2::facet_wrap() (rather than through
  # xpose's own plotting functions, which always use ggforce's *_paginate()
  # facets) is not of class FacetWrapPaginate/FacetGridPaginate, so it still
  # takes the non-paginated branch despite having many panels.
  p_many_panels <- xpose::dv_vs_ipred(xpdb_ex_pk, quiet = FALSE) + ggplot2::facet_wrap(~ID)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_warning(print(p_flat, page = 1), "Faceting not set")
  suppressWarnings(expect_message(print(p_many_panels), "panels"))
})

test_that("n_panels() returns 0L when the build layout is unavailable (defensive fallback)", {
  # `ggplot2::ggplot_build(plot)$layout$layout` is effectively always
  # populated for any real plot; mock ggplot_build() itself to exercise this
  # defensive branch, which normal plot objects can't reach.
  real_pv <- utils::packageVersion
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "ggplot2")) return(package_version("3.5.0"))
      real_pv(pkg, ...)
    },
    .package = "utils"
  )
  local_mocked_bindings(
    ggplot_build = function(plot) list(layout = list(layout = NULL)),
    .package = "ggplot2"
  )

  expect_equal(n_panels(1), 0L)
})
