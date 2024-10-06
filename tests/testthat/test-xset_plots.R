test_that("two dot sets are checked and set appropriately", {
  two_dot_env <- new.env()
  expect_length(
    ls(envir = two_dot_env),
    0
  )
  # simple working test (no parent, no base, two items in dots)
  two_set_dots(pheno_set, run3, run7, envir = two_dot_env)
  expect_setequal(
    ls(envir = two_dot_env),
    c("mod1", "mod2")
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
      two_set_dots(run3, run7, run14),
    "Need.*exactly two"
  )

  # Sample 2 random in set
  two_random <- sample(length(pheno_set), 2)
  expect_no_error(
    pheno_set %>%
      select(!!two_random) %>%
      two_set_dots(envir = two_dot_env)
  )
  if (!is.na(pheno_set[[two_random[1]]]$parent) &&
    pheno_set[[two_random[1]]]$parent == pheno_set[[two_random[2]]]$label) {
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
  with_base_model <- set_base_model(pheno_set, run14)
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
    franken_xpdb(pheno_base, pheno_final, .types = "catcov")
  )
  expect_in(
    c("APGR_1", "APGR_2"),
    franken_xpdb(pheno_base, pheno_final, .types = "catcov") %>%
      xpose::get_data(quiet = TRUE) %>%
      names()
  )

  expect_error(
    franken_xpdb(pheno_base, .types = "catcov"),
    "Need at least two"
  )
  expect_error(
    franken_xpdb(pheno_base, pheno_final),
    "Need .*cols.*and.*or.*types"
  )

  expect_error(
    franken_xpdb(xpdb_x, pheno_final, .types = "idv", problem = 2),
    "No prob.*2.*in.*run16"
  )
  expect_error(
    franken_xpdb(xpdb_x, pheno_final, .types = "idv", problem = 99),
    "No prob.*99.*in.*run001"
  )

  expect_error(
    franken_xpdb(xpdb_x, pheno_final, .types = "catcov", problem = 2),
    "Error with.*prob.*2.*in.*run001.*catcov"
  )

  expect_error(
    franken_xpdb(xpdb_x, pheno_final, .types = "catcov", problem = 1),
    "run16.*744 rows.*previous.*550"
  )
  expect_error(
    franken_xpdb(pheno_final, xpdb_x, .types = "catcov", problem = 1),
    "run001.*550 rows.*previous.*744"
  )


  expect_error(
    pheno_base %>%
      mutate_x(ID = as.numeric(as.character(ID))) %>%
      franken_xpdb(pheno_final, .types = "catcov"),
    "run16.*IDs do not match.*previous.*be identical"
  )

  expect_error(
    pheno_base %>%
      rename_x(DV2 = DV) %>%
      franken_xpdb(pheno_final, .cols = DV),
    "Error.*prob.*run6.*.DV.*exist"
  )
})

test_that("franken_prop reasonably combines properties", {
  xpdb_f <- franken_xpdb(pheno_base, pheno_final, .types = "catcov")
  xpdb_list <- list(pheno_base, pheno_final)
  xpdb_f3 <- franken_xpdb(pheno_base, pheno_set$run3$xpdb, pheno_final, .types = "catcov")
  xpdb_list3 <- list(pheno_base, pheno_set$run3$xpdb, pheno_final)

  expect_equal(
    xpdb_f %>%
      franken_prop(xpdb_list, "run",
        glue_cmd = function(x) paste(x, collapse = "&also&")
      ) %>%
      get_prop("run"),
    paste0(get_prop(pheno_base, "run"), "&also&", get_prop(pheno_final, "run"))
  )
  expect_equal(
    xpdb_f %>%
      franken_prop(xpdb_list, "run") %>%
      get_prop("run"),
    paste0(get_prop(pheno_base, "run"), " and ", get_prop(pheno_final, "run"))
  )
  expect_equal(
    xpdb_f3 %>%
      franken_prop(xpdb_list3, "run") %>%
      get_prop("run"),
    paste0(get_prop(pheno_base, "run"), ", ", get_prop(pheno_set$run3$xpdb, "run"), " and ", get_prop(pheno_final, "run"))
  )
  # mix indices
  expect_equal(
    xpdb_f3 %>%
      franken_prop(xpdb_list3, "run", indices = c(3, 1, 2)) %>%
      get_prop("run"),
    paste0(get_prop(pheno_final, "run"), ", ", get_prop(pheno_base, "run"), " and ", get_prop(pheno_set$run3$xpdb, "run"))
  )
  expect_equal(
    xpdb_f3 %>%
      franken_prop(xpdb_list3, "run", indices = c(1, 3)) %>%
      get_prop("run"),
    paste0(get_prop(pheno_base, "run"), " and ", get_prop(pheno_final, "run"))
  )

  # numprop
  expect_equal(
    xpdb_f %>%
      franken_prop(xpdb_list, "ofv",
        glue_cmd = franken_numprop
      ) %>%
      get_prop("ofv"),
    paste0(get_prop(pheno_base, "ofv"), " [1]; ", get_prop(pheno_final, "ofv"), " [2]")
  )
  expect_match(
    xpdb_f %>%
      franken_prop(xpdb_list, "etashk",
        glue_cmd = franken_numprop
      ) %>%
      get_prop("etashk"),
    "\\(1\\): .*; \\(2\\): .*"
  )
})

test_that("model averaged plots are consistent with manually-implemented", {
  # model averaging function is assessed in a separate test script
  # this just ensures plot objects are as expected if manually coded

  # test with random variation
  roll_opts <- function(envir = parent.frame(), seed = 1) {
    set.seed(seed)
    assign("algo", sample(c("maa", "msa"), 1), envir = envir)
    assign("wtype", sample(c("individual", "population"), 1), envir = envir)
    assign("wbase", sample(c("ofv", "aic", "res"), 1), envir = envir)
  }
  test_modavg <- function(..., .lineage = FALSE, avg_cols = NULL, avg_by_type = NULL, seed = 1) {
    roll_opts(seed = seed)
    if (rlang::is_interactive()) cli::cli_inform("{c(algo, wtype, wbase)}")
    modavg_xpdb(
      xpdb_s = pheno_set,
      ...,
      .lineage = .lineage,
      avg_cols = avg_cols,
      avg_by_type = avg_by_type,
      algorithm = algo,
      weight_type = wtype,
      auto_backfill = TRUE,
      weight_basis = wbase,
      res_col = "RES",
      quiet = TRUE
    )
  }

  random_set_elements <- sample(names(pheno_set), 4)

  expect_no_error(
    plotfun_modavg(pheno_set, dplyr::all_of(random_set_elements), quiet = TRUE, auto_backfill = TRUE, .fun = xpose::eta_distrib)
  )
  expect_error(
    plotfun_modavg(pheno_set, dplyr::all_of(random_set_elements), quiet = TRUE, auto_backfill = FALSE, .fun = xpose::eta_distrib),
    "Indiv.*OFV.*required.*Set.*auto_backfill"
  )
  expect_error(
    plotfun_modavg(pheno_set, dplyr::all_of(random_set_elements), quiet = TRUE, auto_backfill = FALSE, .fun = "hey"),
    "must be a function.*not.*character"
  )
  expect_error(
    plotfun_modavg(xpdb = pheno_base, dplyr::all_of(random_set_elements), quiet = TRUE, auto_backfill = FALSE, .fun = xpose::eta_distrib),
    "xpose_set.*required.*not.*xpose_data"
  )

  # compare
  compare_on <- c("data", "labels", "theme") # limit comparison to those without embedded environments
  test_funs <- list(eta_vs_contcov, xpose::eta_distrib, xpose::dv_vs_ipred)
  for (tfun in test_funs) {
    set.seed(NULL)
    roll_samp <- sample(1000, 1)
    roll_opts(seed = roll_samp)
    random_set_elements <- sample(names(pheno_set), 4)
    expect_identical(
      plotfun_modavg(
        pheno_set,
        dplyr::all_of(random_set_elements),
        quiet = TRUE,
        auto_backfill = TRUE,
        .fun = tfun,
        avg_by_type = c("eta", "ipred"),
        algorithm = algo,
        weight_type = wtype,
        weight_basis = wbase,
        title = "overwrite"
      ) %>%
        `if`(xpose::is.xpose.plot(.), ., .[[1]]) %>% # catch list created by eta_vs_contcov
        .[names(.) %in% compare_on],
      test_modavg(
        dplyr::all_of(random_set_elements),
        avg_by_type = c("eta", "ipred"),
        seed = roll_samp
      ) %>%
        tfun(quiet = TRUE, title = "overwrite") %>%
        `if`(xpose::is.xpose.plot(.), ., .[[1]]) %>% # catch list created by eta_vs_contcov
        .[names(.) %in% compare_on]
    )
  }

  # specific built-ins
  test_tbl <- purrr::map2_dfr(
    # odds should be ipreds (not great, but this is already quite complicated)
    list(xpose::dv_vs_ipred, xpose::dv_vs_pred, xpose::ipred_vs_idv, xpose::pred_vs_idv),
    list(dv_vs_ipred_modavg, dv_vs_pred_modavg, ipred_vs_idv_modavg, pred_vs_idv_modavg),
    ~ tibble::tibble(
      tst = list(.x),
      pkg = list(.y)
    )
  )
  for (rown in 1:nrow(test_tbl)) {
    tfun <- test_tbl$tst[[rown]]
    pfun <- test_tbl$pkg[[rown]]
    predvar <- ifelse(rown %% 2 == 0, "pred", "ipred")
    set.seed(NULL)
    roll_samp <- sample(1000, 1)
    roll_opts(seed = roll_samp)
    random_set_elements <- sample(names(pheno_set), 4)
    expect_identical(
      plotfun_modavg(
        pheno_set,
        dplyr::all_of(random_set_elements),
        quiet = TRUE,
        auto_backfill = TRUE,
        .fun = tfun,
        avg_by_type = predvar,
        algorithm = algo,
        weight_type = wtype,
        weight_basis = wbase
      ) %>%
        `if`(xpose::is.xpose.plot(.), ., .[[1]]) %>% # catch list created by eta_vs_contcov
        .[names(.) %in% compare_on],
      pfun(
        pheno_set,
        dplyr::all_of(random_set_elements),
        auto_backfill = TRUE,
        quiet = TRUE,
        algorithm = algo,
        weight_type = wtype,
        weight_basis = wbase
      ) %>%
        `if`(xpose::is.xpose.plot(.), ., .[[1]]) %>% # catch list created by eta_vs_contcov
        .[names(.) %in% compare_on]
    )
  }
})

test_that("pred comparison plots work", {
  expect_no_error(
    xpose_set(pheno_base,pheno_final) %>%
      ipred_vs_ipred(quiet=TRUE)
  )
  expect_no_error(
    xpose_set(pheno_base,pheno_final) %>%
      pred_vs_pred(quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      ipred_vs_ipred(run6,run8,quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      pred_vs_pred(run6,run8,quiet=TRUE)
  )

  example_xpdbs <- list(a=pheno_base,b=pheno_final)
  test_iplot <- xpose_set(!!!example_xpdbs) %>%
    ipred_vs_ipred(quiet=TRUE)
  test_plot <- xpose_set(!!!example_xpdbs) %>%
    pred_vs_pred(quiet=TRUE)
  test_data <- purrr::map(example_xpdbs,
                          xpose::get_data, quiet=TRUE)
  ipred_labs <- sprintf("%s (%s)",
                        purrr::map_chr(
                          example_xpdbs,
                          ~ xp_var(.x,.problem = 1,type="ipred")$col
                        ),
                        purrr::map_chr(
                          example_xpdbs,
                          ~ get_prop(.x, "run")
                        ))
  pred_labs <- sprintf("%s (%s)",
                        purrr::map_chr(
                          example_xpdbs,
                          ~ xp_var(.x,.problem = 1,type="pred")$col
                        ),
                        purrr::map_chr(
                          example_xpdbs,
                          ~ get_prop(.x, "run")
                        ))
  for (i in seq_along(example_xpdbs)) {
    expect_in( # in because plot only contains obs data
      test_iplot$data[[ipred_labs[[i]]]],
      test_data[[i]][[xp_var(example_xpdbs[[i]],.problem = 1,type="ipred")$col]]
    )
    expect_in(
      test_plot$data[[pred_labs[[i]]]],
      test_data[[i]][[xp_var(example_xpdbs[[i]],.problem = 1,type="pred")$col]]
    )
  }

})

test_that("waterfall plots produce expected errors", {
  # most core parts of waterfall plots are a separate script

  expect_error(
    pheno_set %>%
      iofv_waterfall(run8,run13),
    "No.*iOFV.*set_var_type.*iofv"
  )
  expect_no_error(
    pheno_set %>%
      focus_qapply(backfill_iofv) %>%
      iofv_waterfall(run8,run13,quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      eta_waterfall(run8,run13,quiet=TRUE)
  )
  expect_error(
    pheno_set %>%
      prm_waterfall(run8,run13,quiet=TRUE),
    "No.*parameters.*set_var_type.*param"
  )
  expect_no_error(
    pheno_set %>%
      focus_qapply(set_var_types,param=c(CL,V)) %>%
      prm_waterfall(run8,run13,quiet=TRUE)
  )
})

test_that("iofv trends can be shown in a boxplot", {
  expect_error(
    pheno_set %>%
      iofv_vs_mod(),
    "auto_backfill"
  )
  expect_no_error(
    pheno_set %>%
      focus_qapply(backfill_iofv) %>%
      iofv_vs_mod(quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      iofv_vs_mod(auto_backfill = TRUE, quiet=TRUE)
  )


  expect_no_error(
    pheno_set %>%
      iofv_vs_mod(run3, .lineage = TRUE, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_error(
    pheno_set %>%
      iofv_vs_mod(run3, run6, .lineage = TRUE, auto_backfill = TRUE, quiet=TRUE),
    "list.*lineage.*multiple.*empty.*single.*"
  )

  expect_no_error(
    pheno_set %>%
      iofv_vs_mod(run15~run7+run6, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      iofv_vs_mod(run5:run8, auto_backfill = TRUE, quiet=TRUE)
  )
})
