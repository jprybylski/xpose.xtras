test_that("two dot sets are checked and set appropriately", {
  two_dot_env <- new.env()
  expect_length(
    ls(envir = two_dot_env),
    0
  )
  # simple working test (no parent, no base, two items in dots)
  two_set_dots(pheno_set,run3,run7,envir = two_dot_env)
  expect_setequal(
    ls(envir = two_dot_env),
    c("mod1","mod2")
  )
  expect_identical(
    two_dot_env$mod1,
    pheno_set$run3
  )
  expect_identical(
    two_dot_env$mod2,
    pheno_set$run7
  )
  two_dot_env <- new.env() # clean up


  expect_error(
    pheno_set %>%
      select(1) %>%
      two_set_dots(),
    "Need at least two"
  )
  expect_error(
    pheno_set %>%
      select(1:3) %>%
      two_set_dots(),
    "Need to select at least two.*use.*length 2"
  )
  expect_error(
    pheno_set %>%
      two_set_dots(run3,run7,run14),
    "Need.*exactly two"
  )

  # Sample 2 random in set
  two_random <- sample(length(pheno_set),2)
  expect_no_error(
    pheno_set %>%
      select(!!two_random) %>%
      two_set_dots(envir = two_dot_env)
  )
  if (pheno_set[[two_random[1]]]$parent==pheno_set[[two_random[2]]]$label) {
    # Because random, test depends on parentage
    expect_identical(
      two_dot_env$mod1,
      pheno_set[[two_random[2]]]
    )
    expect_identical(
      two_dot_env$mod2,
      pheno_set[[two_random[1]]]
    )
  } else {
    expect_identical(
      two_dot_env$mod1,
      pheno_set[[two_random[1]]]
    )
    expect_identical(
      two_dot_env$mod2,
      pheno_set[[two_random[2]]]
    )
  }
  two_dot_env <- new.env() # clean up

  # Parent and child in set (speaking of...)
  expect_no_error(
    pheno_set %>%
      two_set_dots(run14, run9, envir = two_dot_env)
  )
  expect_identical(
    two_dot_env$mod1,
    pheno_set$run9
  )
  expect_identical(
    two_dot_env$mod2,
    pheno_set$run14
  )
  two_dot_env <- new.env() # clean up

  # Parents don't matter if base model set
  with_base_model <- set_base_model(pheno_set,run14)
  with_base_model %>%
    two_set_dots(run14, run9, envir = two_dot_env)
  expect_identical(
    two_dot_env$mod1,
    with_base_model$run14
  )
  expect_identical(
    two_dot_env$mod2,
    with_base_model$run9
  )


})

test_that("two or more xpose objects can be combined with franken_xpdb", {
  expect_no_error(
    franken_xpdb(pheno_base, pheno_final, .types="catcov")
  )
  expect_in(
    c("APGR_1","APGR_2"),
    franken_xpdb(pheno_base, pheno_final, .types="catcov") %>%
      xpose::get_data(quiet=TRUE) %>%
      names()
  )

  expect_error(
    franken_xpdb(pheno_base, .types="catcov"),
    "Need at least two"
  )
  expect_error(
    franken_xpdb(pheno_base, pheno_final),
    "Need .*cols.*and.*or.*types"
  )

  expect_error(
    franken_xpdb(xpdb_x, pheno_final, .types="idv", problem=2),
    "No prob.*2.*in.*run16"
  )
  expect_error(
    franken_xpdb(xpdb_x, pheno_final, .types="idv", problem=99),
    "No prob.*99.*in.*run001"
  )

  expect_error(
    franken_xpdb(xpdb_x, pheno_final, .types="catcov", problem=2),
    "Error with.*prob.*2.*in.*run001.*catcov"
  )

  expect_error(
    franken_xpdb(xpdb_x, pheno_final, .types="catcov", problem = 1),
    "run16.*744 rows.*previous.*550"
  )
  expect_error(
    franken_xpdb(pheno_final, xpdb_x, .types="catcov", problem = 1),
    "run001.*550 rows.*previous.*744"
  )


  expect_error(
    pheno_base %>%
      mutate_x(ID = as.numeric(as.character(ID))) %>%
      franken_xpdb(pheno_final, .types="catcov"),
    "run16.*IDs do not match.*previous.*be identical"
  )

  expect_error(
    pheno_base %>%
      rename_x(DV2=DV) %>%
      franken_xpdb(pheno_final, .cols=DV),
    "Error.*prob.*run6.*.DV.*exist"
  )

})

test_that("franken_prop reasonably combines properties", {
  xpdb_f <- franken_xpdb(pheno_base, pheno_final, .types="catcov")
  xpdb_list <- list(pheno_base, pheno_final)
  xpdb_f3 <- franken_xpdb(pheno_base,pheno_set$run3$xpdb, pheno_final, .types="catcov")
  xpdb_list3 <- list(pheno_base,pheno_set$run3$xpdb, pheno_final)

  expect_equal(
    xpdb_f %>%
      franken_prop(xpdb_list, "run",
                   glue_cmd = function(x) paste(x, collapse="&also&")) %>%
      get_prop("run"),
    paste0(get_prop(pheno_base,"run"),"&also&",get_prop(pheno_final,"run"))
  )
  expect_equal(
    xpdb_f %>%
      franken_prop(xpdb_list, "run") %>%
      get_prop("run"),
    paste0(get_prop(pheno_base,"run")," and ",get_prop(pheno_final,"run"))
  )
  expect_equal(
    xpdb_f3 %>%
      franken_prop(xpdb_list3, "run") %>%
      get_prop("run"),
    paste0(get_prop(pheno_base,"run"),", ",get_prop(pheno_set$run3$xpdb,"run")," and ",get_prop(pheno_final,"run"))
  )
  #mix indices
  expect_equal(
    xpdb_f3 %>%
      franken_prop(xpdb_list3, "run", indices = c(3,1,2)) %>%
      get_prop("run"),
    paste0(get_prop(pheno_final,"run"),", ",get_prop(pheno_base,"run")," and ",get_prop(pheno_set$run3$xpdb,"run"))
  )
  expect_equal(
    xpdb_f3 %>%
      franken_prop(xpdb_list3, "run", indices = c(1,3)) %>%
      get_prop("run"),
    paste0(get_prop(pheno_base,"run")," and ",get_prop(pheno_final,"run"))
  )

  # numprop
  expect_equal(
    xpdb_f %>%
      franken_prop(xpdb_list, "ofv",
                   glue_cmd = franken_numprop) %>%
      get_prop("ofv"),
    paste0(get_prop(pheno_base,"ofv")," [1]; ",get_prop(pheno_final,"ofv")," [2]")
  )
  expect_match(
    xpdb_f %>%
      franken_prop(xpdb_list, "etashk",
                   glue_cmd = franken_numprop) %>%
      get_prop("etashk"),
    "\\(1\\): .*; \\(2\\): .*"
  )

})
