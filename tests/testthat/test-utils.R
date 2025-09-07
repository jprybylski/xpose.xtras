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
  expect_no_error(
    mutate_files(nlmixr2_m3)
  )
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

