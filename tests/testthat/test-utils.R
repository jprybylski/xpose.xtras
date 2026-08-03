test_that("get_* functions work", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_error(get_prop(xpdb_ex_pk, c("descr", "etashk")), regexp = "one property")
  expect_error(get_prop(xpdb_ex_pk, "fakeprop"), regexp = "fakeprop")

  expect_identical(
    get_prop(xpdb_ex_pk, "descr"),
    xpdb_ex_pk %>% xpose::get_summary() %>% dplyr::filter(label=="descr") %>% dplyr::pull(value)
  )
  # backwards approach to ensure shrinkage is same format in current version of xpose
  expect_identical(
    get_shk(xpdb_ex_pk) %>% sprintf("%s [%i]", ., seq_along(.)) %>% paste(collapse=", "),
    get_prop(xpdb_ex_pk, "etashk")
  )
  expect_identical(
    get_shk(xpdb_ex_pk, wh="eps") %>% sprintf("%s [%i]", ., seq_along(.)) %>% paste(collapse=", "),
    get_prop(xpdb_ex_pk, "epsshk")
  )

})

test_that("get_prop handles multi-row properties", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # multiple rows but all problem==0/subprob==0 (eg a duplicated run-level
  # property) skips the .problem/.subprob filter and returns every value
  xpdb_dup <- xpdb_ex_pk
  descr_row <- xpdb_dup$summary[xpdb_dup$summary$label == "descr", ][1, ]
  descr_row$value <- "second description"
  xpdb_dup$summary <- dplyr::bind_rows(xpdb_dup$summary, descr_row)
  expect_equal(
    get_prop(xpdb_dup, "descr"),
    c("NONMEM PK example for xpose", "second description")
  )

  # multi-row property, filtered to a .problem with no matching rows errors
  # informatively
  expect_error(
    get_prop(xpdb_ex_pk, "label", .problem = 99),
    "No summary item matching"
  )
})

test_that("set_* functions works", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_error(set_prop(xpdb_ex_pk, repeat_name="", repeat_name=""), regexp = "have unique")
  expect_error(set_prop(xpdb_ex_pk, fake_prop=""), regexp = "fake_prop")
  expect_error(set_prop(xpdb_ex_pk, descr=c("multiple","values")), regexp = "set to one value")

  rand_desc <- paste(sample(letters, 5), collapse="")
  new_desc <- set_prop(xpdb_ex_pk, descr = rand_desc)
  expect_equal(
    get_prop(new_desc, "descr"), rand_desc
  )

  rand_desc <- paste(sample(letters, 5), collapse="")
  new_desc <- set_prop(xpdb_ex_pk, descr = rand_desc, .problem = 1)
  expect_failure(expect_equal(
    get_prop(new_desc, "descr"), rand_desc
  ))

  # expect check for string
  expect_error(
    set_prop(xpdb_x, descr = list(one="item")),
    "to character/string values"
  )
  expect_error(
    set_prop(xpdb_x, descr = Sys.Date()),
    "to character/string values"
  )
  # expect for length 1 numbers and factors to be gracefully converted
  expect_no_error(
    set_prop(xpdb_x, nsig=4)
  )
  expect_no_error(
    set_prop(xpdb_x, descr = factor("for some reason this is a factor"))
  )



  expect_failure(expect_equal(
    get_prop(xpdb_ex_pk, "label", .problem = 1),
    get_prop(xpdb_ex_pk, "label", .problem = 2)
  ))
  rand_label <- paste(sample(letters, 5), collapse="")
  new_label <- set_prop(xpdb_ex_pk, label = rand_label)
  expect_equal(
    get_prop(new_label, "label", .problem = 1),
    get_prop(new_label, "label", .problem = 2)
  )
  expect_equal(
    get_prop(new_label, "label", .problem = 1),
    rand_label
  )
  expect_equal(
    get_prop(new_label, "label", .problem = 2),
    rand_label
  )


  rand_label <- paste(sample(letters, 5), collapse="")
  new_label <- set_prop(xpdb_ex_pk, label = rand_label, .problem = 1)
  expect_failure(expect_equal(
    get_prop(new_label, "label", .problem = 1),
    get_prop(new_label, "label", .problem = 2)
  ))
  expect_equal(
    get_prop(new_label, "label", .problem = 1),
    rand_label
  )
  expect_failure(expect_equal(
    get_prop(new_label, "label", .problem = 2),
    rand_label
  ))

  # set_option
  current_quiet <- xpdb_ex_pk$options$quiet
  expect_equal(
    set_option(xpdb_ex_pk, quiet = !current_quiet)$options$quiet,
    !current_quiet
  )
  expect_error(
    set_option(xpdb_x, cvtype = "log"),
    regexp = "exact.*sqrt.*log"
  )


  expect_error(
    set_prop(pheno_saem,descr="good description", .subprob = 2),
    "\\.problem.*is needed if.*subprob"
  )
  expect_error(
    set_prop(pheno_saem,descr="good description", .problem = 1:3, .subprob = 1:2),
    "subprob.*should be recyclable"
  )
  expect_error(
    set_prop(pheno_saem,descr="good description", .problem = 1:2, .subprob = 1:3),
    "problem.*should be recyclable"
  )
  expect_identical(
    set_prop(pheno_saem,method="different method label", .problem=1, .subprob = 0:1)$summary,
    set_prop(pheno_saem,method="different method label", .problem=1)$summary
  )
  expect_failure(expect_identical(
    set_prop(pheno_saem,method="different method label", .problem=1, .subprob = 1)$summary,
    set_prop(pheno_saem,method="different method label", .problem=1, .subprob = 0)$summary
  ))

})

test_that("get-set index works", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_error(
    get_index(c())
  )
  expect_error(
    get_index(xpdb_ex_pk, NULL, sddd=1),
    regexp = "sddd"
  )

  expect_s3_class(
    get_index(xpdb_ex_pk),
    "data.frame"
  )

  expect_setequal(
    xpdb_ex_pk$data$problem,
    get_index(xpdb_ex_pk)$problem
  )
  expect_setequal(
    1,
    get_index(xpdb_ex_pk, .problem = 1)$problem
  )
  expect_setequal(
    2,
    get_index(xpdb_ex_pk, .problem = 2)$problem
  )


  expect_failure(expect_identical(
    get_index(xpdb_ex_pk),
    get_index(set_index(xpdb_ex_pk, get_index(xpdb_ex_pk)))
  ))
  expect_s3_class(
    set_index(xpdb_ex_pk, get_index(xpdb_ex_pk)),
    "xp_xtras"
  )
  expect_identical(
    get_index(as_xpdb_x(xpdb_ex_pk)),
    get_index(set_index(xpdb_ex_pk, get_index(xpdb_ex_pk)))
  )


})

test_that("convenience functions return expected", {
  expect_false(
    is_formula_list(list())
  )
  expect_false(
    is_formula_list(a~b)
  )
  expect_true(
    is_formula_list(c(a~b))
  )
  expect_true(
    is_formula_list(list(a~b))
  )
  expect_false(
    is_formula_list(rlang::quos(1+1,1+2,1+3))
  )
})

test_that("reportable digits works", {
  # cross-compatible
  expect_identical(
    reportable_digits(xpose::xpdb_ex_pk),
    reportable_digits(as_xpdb_x(xpose::xpdb_ex_pk))
  )

  # gets new nsig
  new_digs <- sample(4:9,1)
  expect_equal(
    set_prop(xpdb_x, nsig=new_digs) %>% reportable_digits(),
    new_digs
  )


  # doesn't return error if nsig not in summary
  no_sig <- xpdb_x
  no_sig$summary <- no_sig$summary %>%
    dplyr::filter(label!="nsig")
  no_sig <- as_xpdb_x(no_sig)
  expect_no_error(
    reportable_digits(no_sig)
  )
  new_digs <- sample(4:9,1)
  expect_equal(
    reportable_digits(no_sig, .default = new_digs),
    new_digs
  )

  # expect error if not even talking about xpdb
  expect_error(
    reportable_digits(Sys.Date()),
    "Bad input"
  )

  # doesn't return non-numeric
  new_digs <- sample(4:9,1)
  expect_equal(
    set_prop(xpdb_x, nsig="not a number") %>% reportable_digits(.default = new_digs),
    new_digs
  )

  # doesn't return other NA
  new_digs <- sample(4:9,1)
  expect_equal(
    set_prop(xpdb_x, nsig=NA_character_) %>% reportable_digits(.default = new_digs),
    new_digs
  )

})

test_that("description can be pulled from commments generically", {
  expect_false(identical(
    get_prop(pheno_base, "descr"),
    get_prop(pheno_base %>% desc_from_comments(), "descr")
  ))
  # Weird code example
  pkpd_m3x <- pkpd_m3b <- pkpd_m3a <- pkpd_m3
  pkpd_m3b$code$comment[6] <- "; Description: late description in file"
  pkpd_m3b <- as_xp_xtras(pkpd_m3b)
  pkpd_m3a$code$comment[1] <- "; Description: correct description in file"
  pkpd_m3a <- as_xp_xtras(pkpd_m3a)
  pkpd_m3x$code$comment[1] <- "; Description:" # empty
  pkpd_m3x <- as_xp_xtras(pkpd_m3x)
  expect_warning(
    desc_from_comments(pkpd_m3),
    "Cannot find a valid"
  )
  expect_warning(
    desc_from_comments(pkpd_m3b),
    "Cannot find a valid"
  )
  expect_warning(
    desc_from_comments(pkpd_m3x),
    "Cannot find a valid"
  )
  expect_no_warning(
    desc_from_comments(pkpd_m3a),
    message="Cannot find a valid"
  )

  expect_false(
    desc_from_comments(pkpd_m3a) %>%
      get_prop("descr") %>%
      grepl(";")
  )
  expect_false(
    desc_from_comments(pkpd_m3a) %>%
      get_prop("descr") %>%
      grepl("^description",.,ignore.case = TRUE)
  )
  expect_true(
    desc_from_comments(pkpd_m3a, remove="^\\W") %>%
      get_prop("descr") %>%
      grepl("^description",.,ignore.case = TRUE)
  )
  expect_true(
    desc_from_comments(pkpd_m3a, extra_proc = toupper) %>%
      get_prop("descr") %>%
      grepl("CORRECT",.,ignore.case = FALSE)
  )
  expect_error(
    desc_from_comments(pkpd_m3a, extra_proc = ""),
    regexp = "character"
  )
})

test_that("extra fill tests pass", {
  fill_test <- function(...,xpdb=pheno_saem) {
    fill_prob_subprob_method(xpdb, ...)
    list(
      .problem = .problem,
      .subprob = .subprob,
      .method = .method
    )
  }

  expect_no_error(
    fill_test()
  )
  expect_identical(
    fill_test(),
    list(
      .problem = 1,
      .subprob = 2,
      .method = "imp"
    )
  )
  expect_identical(
    fill_test(for_summary = TRUE),
    list(
      .problem = 1,
      .subprob = 1,
      .method = "imp"
    )
  )
  expect_no_error(
    fill_test(.problem = 1)
  )
  expect_no_error(
    fill_test(.problem = 1, .subprob=1)
  )
  expect_no_error(
    fill_test(.problem = 1, .subprob=1, .method="saem")
  )
  expect_identical(
    fill_test(.method="saem"),
    fill_test(.problem = 1, .subprob=1, .method="saem")
  )

  no_ext <- xpdb_x
  no_ext$files <- dplyr::filter(no_ext$files, extension != "ext")
  no_ext <- as_xp_xtras(no_ext)
  expect_error(
    fill_test(xpdb=no_ext),
    regexp = "extension.*ext.*missing"
  )

})


test_that("check xpdb with logical return", {
  expect_no_error(
    test_xpdb(xpdb_x)
  )
  expect_true(
    test_xpdb(xpdb_x)
  )
  not_xpdb <- "hi"
  expect_no_error(
    test_xpdb(not_xpdb)
  )
  expect_false(
    test_xpdb(not_xpdb)
  )
})

test_that("files df can be mutated", {
  expect_no_error(
    mutate_files(xpdb_x)
  )
  if (rlang::is_installed(c("nlmixr2est", "nlmixr2data", "xpose.nlmixr2"))) {
    expect_no_error(
      mutate_files(cached_nlmixr_example("nlmixr2_m3"))
    )
  }
  expect_identical(
    class(mutate_files(xpdb_x)),
    class(xpdb_x)
  )
  expect_identical(
    class(mutate_files(xpose::xpdb_ex_pk)),
    class(xpose::xpdb_ex_pk)
  )
  expect_identical(
    mutate_files(xpdb_x,name=toupper(name))$files,
    dplyr::mutate(xpdb_x$files,name=toupper(name))
  )
})

test_that("recalc_shk recalculates shrinkage from individual etas", {

  all_etas <- recalc_shk(xpdb_x, quiet = TRUE)
  expect_equal(all_etas$eta, c("ETA1", "ETA2", "ETA3"))
  expect_equal(all_etas$n, rep(74L, 3))
  expect_equal(all_etas$n_excluded, rep(0L, 3))
  # hand-computed via the standard 100*(1-SD(eta)/omega) formula
  expect_equal(
    all_etas$shrinkage,
    c(52.9, 68.5, 10.3),
    tolerance = 0.05
  )

  # tidyselect subsets the etas used
  expect_identical(
    recalc_shk(xpdb_x, ETA1, quiet = TRUE)$eta,
    "ETA1"
  )
  expect_error(
    recalc_shk(xpdb_x, ID, quiet = TRUE),
    regexp = "should only select"
  )

  expect_error(
    recalc_shk(xpdb_x, .etastype = 2, quiet = TRUE),
    regexp = "etastype"
  )

  # .etastype governs whether "true zero" etas are excluded
  xpdb_zeroes <- xpdb_x
  raw_data <- xpdb_zeroes$data$data[[1]]
  zero_ids <- unique(raw_data$ID)[1:5]
  raw_data$ETA1[raw_data$ID %in% zero_ids] <- 0
  xpdb_zeroes$data$data[[1]] <- raw_data

  excl <- recalc_shk(xpdb_zeroes, ETA1, .etastype = 1, quiet = TRUE)
  incl <- recalc_shk(xpdb_zeroes, ETA1, .etastype = 0, quiet = TRUE)
  expect_equal(excl$n_excluded, 5L)
  expect_equal(incl$n_excluded, 0L)
  expect_false(isTRUE(all.equal(excl$shrinkage, incl$shrinkage)))

  # eta columns with no embedded number, and no direct name match, error
  # informatively rather than guessing
  no_num_xpdb <- xpdb_x
  no_num_xpdb$data$data[[1]]$WEIRDETA <- no_num_xpdb$data$data[[1]]$ETA1
  no_num_xpdb$data <- xpose::xpdb_index_update(xpdb = no_num_xpdb, .problem = 1)
  no_num_xpdb <- set_var_types_x(no_num_xpdb, .problem = 1, eta = WEIRDETA)
  expect_error(
    recalc_shk(no_num_xpdb, WEIRDETA, quiet = TRUE),
    regexp = "Could not associate"
  )
})

test_that("recalc_shk matches etas to omegas by name for nlmixr2 models", {
  skip_if_not_installed("rxode2")
  skip_if(utils::packageVersion("rxode2") < "5.0",
          "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)")
  skip_if_not_installed("nlmixr2est")

  # nlmixr2 eta columns (eg `eta.cl`) aren't numbered, and don't relate to
  # `m`/`n` matrix position at all -- recalc_shk() has to fall back to
  # matching by name against get_prm()'s `name` column for these
  xp1 <- cached_nlmixr_example("xpdb_nlmixr2")
  eta_cols <- xp_var(xp1, .problem = 1, type = "eta")$col
  expect_false(any(grepl("\\d", eta_cols)))

  shk <- recalc_shk(xp1, quiet = TRUE)
  expect_setequal(shk$eta, eta_cols)
  expect_true(all(shk$omega > 0))
  expect_true(all(is.finite(shk$shrinkage)))
})

test_that("normalize_etas sets a per-eta normalization factor from sqrt(omega)", {
  xpdb_n <- normalize_etas(xpdb_x, quiet = TRUE)
  factors <- xpdb_n$options$normalize_etas
  expect_setequal(names(factors), c("ETA1", "ETA2", "ETA3"))
  # hand-computed against the same omegas recalc_shk() reports
  om <- recalc_shk(xpdb_x, quiet = TRUE)
  expect_equal(
    unlist(factors)[om$eta],
    sqrt(om$omega),
    ignore_attr = TRUE
  )

  # tidyselect subsets which etas get (re)computed
  xpdb_n1 <- normalize_etas(xpdb_x, ETA1, quiet = TRUE)
  expect_named(xpdb_n1$options$normalize_etas, "ETA1")

  expect_error(
    normalize_etas(xpdb_x, ID, quiet = TRUE),
    regexp = "should only select"
  )

  # normalize_etas() never touches the underlying data
  expect_identical(
    xpose::get_data(xpdb_n, .problem = 1, quiet = TRUE),
    xpose::get_data(xpdb_x, .problem = 1, quiet = TRUE)
  )

  # calling again merges (via set_option()) rather than replacing
  xpdb_merged <- normalize_etas(xpdb_n, ETA1, .use_sd = TRUE, quiet = TRUE)
  expect_setequal(names(xpdb_merged$options$normalize_etas), c("ETA1", "ETA2", "ETA3"))
  expect_false(isTRUE(all.equal(
    xpdb_merged$options$normalize_etas$ETA1,
    xpdb_n$options$normalize_etas$ETA1
  )))
  expect_equal(
    xpdb_merged$options$normalize_etas$ETA2,
    xpdb_n$options$normalize_etas$ETA2
  )

  # normalise_etas() is a plain alias
  expect_identical(
    normalise_etas(xpdb_x, quiet = TRUE)$options$normalize_etas,
    normalize_etas(xpdb_x, quiet = TRUE)$options$normalize_etas
  )
})

test_that("normalize_etas .use_sd normalizes by empirical SD instead of omega", {
  xpdb_sd <- normalize_etas(xpdb_x, ETA1, .use_sd = TRUE, quiet = TRUE)
  eta1_vals <- xpose::get_data(xpdb_x, .problem = 1, quiet = TRUE) %>%
    dplyr::distinct(ID, .keep_all = TRUE) %>%
    dplyr::pull(ETA1)
  expect_equal(xpdb_sd$options$normalize_etas$ETA1, stats::sd(eta1_vals))

  # .use_sd sidesteps the eta-omega matching entirely, so it works even
  # when that match would fail (see the "Could not associate" test above)
  no_num_xpdb <- xpdb_x
  no_num_xpdb$data$data[[1]]$WEIRDETA <- no_num_xpdb$data$data[[1]]$ETA1
  no_num_xpdb$data <- xpose::xpdb_index_update(xpdb = no_num_xpdb, .problem = 1)
  no_num_xpdb <- set_var_types_x(no_num_xpdb, .problem = 1, eta = WEIRDETA)

  expect_error(
    normalize_etas(no_num_xpdb, WEIRDETA, quiet = TRUE),
    regexp = "Could not associate"
  )
  expect_error(
    normalize_etas(no_num_xpdb, WEIRDETA, quiet = TRUE),
    regexp = "\\.use_sd"
  )
  expect_no_error(
    normalize_etas(no_num_xpdb, WEIRDETA, .use_sd = TRUE, quiet = TRUE)
  )
})

test_that("derive_shk/backfill_shk compute per-individual shrinkage contribution", {

  orig <- xpose::get_data(xpdb_x, .problem = 1, quiet = TRUE)
  derived <- derive_shk(xpdb_x, quiet = TRUE)
  expect_setequal(
    setdiff(names(derived), names(orig)),
    c("ETA1_SHK", "ETA2_SHK", "ETA3_SHK")
  )
  # hand-computed via log((eta - mean(eta))^2)
  expect_equal(
    derived$ETA1_SHK,
    log((orig$ETA1 - mean(unique(orig$ETA1)))^2)
  )

  # tidyselect subsets which etas get a `_SHK` column
  expect_identical(
    setdiff(names(derive_shk(xpdb_x, ETA1, quiet = TRUE)), names(orig)),
    "ETA1_SHK"
  )
  expect_error(
    derive_shk(xpdb_x, ID, quiet = TRUE),
    regexp = "should only select"
  )

  # backfill_shk joins the column(s) in and tags them with the `shk` type
  xp2 <- backfill_shk(xpdb_x, ETA1, quiet = TRUE)
  expect_identical(
    xp_var(xp2, .problem = 1, type = "shk")$col,
    "ETA1_SHK"
  )
  expect_true("ETA1_SHK" %in% names(xpose::get_data(xp2, .problem = 1, quiet = TRUE)))

  # refuses to silently overwrite an existing `_SHK` column
  expect_error(
    backfill_shk(xp2, ETA1, quiet = TRUE),
    regexp = "already present"
  )
})

