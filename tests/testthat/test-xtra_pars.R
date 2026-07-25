test_that("parameter associations can be added", {
  # Simple tests
  expect_no_error(
    pheno_base %>%
      add_prm_association(the1~log(IIVCL),V~log(IIVV))
  )
  expect_no_error(
    pheno_base %>%
      add_prm_association(the1~log(ome1),THETA2~log(ome2))
  )
  expect_error(
    pheno_base %>%
      add_prm_association(THETA2~log(OMEGA(2,2)))
  )
  expect_no_error(
    pheno_base %>%
      add_prm_association(THETA2~log(`OMEGA(2,2)`))
  )
  expect_error(
    pheno_base %>%
      add_prm_association(THETA2~log(ome2),the2~log(ome1))
  )
  expect_no_error(
    pheno_base %>%
      add_prm_association(the1~log(ome1),the2~log(ome1))
  )
  expect_identical(
    pheno_base %>%
      add_prm_association(),
    pheno_base
  )
  expect_no_error(
    pheno_base %>%
      add_prm_association(the1~logit(IIVCL),V~nmboxcox(IIVV, 0.1))
  )
  expect_no_error(
    pheno_base %>%
      add_prm_association(the1~logexp(IIVCL),V~custom(ome2,qdist=qnorm,pdist=pnorm))
  )
  expect_error(
    pheno_base %>%
      add_prm_association(the1~hhh(IIVCL)),
    "hhh"
  )

  # Targeted errors
  suppressWarnings(suppressMessages(expect_error(
    add_prm_association(xpose::xpdb_ex_pk, the1~log(ome1)),
    "xp_xtras.*required"
  )))
  suppressWarnings(suppressMessages(expect_message(
    tryCatch(add_prm_association(xpose::xpdb_ex_pk, the1~log(ome1)),
             error=function(s) NULL),
    "as_xpdb_x"
  )))
  expect_warning(
    pheno_base %>% # would throw error, but only the first list is used
      add_prm_association(c(THETA2~log(ome2)),c(the2~log(ome1))),
    "List.*should not.*"
  )

  # updating
   with_assoc <- pheno_base %>%
     add_prm_association(the1~log(IIVCL),V~log(IIVV))
   expect_equal(
     with_assoc$pars$assoc[2],
     "log"
   )
   updated_assoc <- with_assoc %>%
     add_prm_association(V~logit(IIVV))
   expect_equal(
     updated_assoc$pars$assoc[2],
     "logit"
   )
})

test_that("parameter associations can be dropped", {
  has_assoc <- pheno_base %>%
        add_prm_association(the1~log(ome1),the2~log(ome2))

  expect_identical(
    drop_prm_association(
      has_assoc
    ),
    has_assoc
  )
  expect_identical(
    drop_prm_association(
      has_assoc, RUVADD
    ),
    has_assoc
  )


  expect_identical(
    drop_prm_association(
      has_assoc, CL
    ),
    pheno_base %>%
      add_prm_association(the2~log(ome2))
  )
  expect_identical(
    drop_prm_association(
      has_assoc, "CL"
    ),
    drop_prm_association(
      has_assoc, CL
    )
  )
})

test_that("drop_prm_association handles edge cases", {
  suppressWarnings(suppressMessages(expect_error(
    drop_prm_association(xpose::xpdb_ex_pk, CL),
    "xp_xtras.*required"
  )))
  # No associations ever declared -> no-op, returned unchanged
  expect_identical(
    drop_prm_association(pheno_base, CL),
    pheno_base
  )
})

test_that("association checks are thorough", {
  expect_error(
    check_associations(
      seems~valid,
      pheno_base
    ),
    "list of formulas"
  )
  expect_error(
    check_associations(
      c(~missing_lhs),
      pheno_base
    ),
    "LHS.*empty"
  )
  expect_error(
    check_associations(
      c(lhs~not_function),
      pheno_base
    ),
    "RHS.*function call"
  )
  expect_error(
    check_associations(
      c(lhs~nmboxcox(ome)),
      pheno_base
    ),
    "nmboxcox.*second.*lambda"
  )
  expect_error(
    check_associations(
      c(lhs~custom(ome)),
      pheno_base
    ),
    "custom.*more.*qdist.*pdist"
  )
  expect_error(
    check_associations(
      c(lhs~custom(ome,onearg)),
      pheno_base
    ),
    "custom.*more.*qdist.*pdist"
  )
  infoEnv <- new.env()
  fill_prob_subprob_method(pheno_base, envir = infoEnv)
  expect_error(
    check_associations(
      c(lhs~log(ome)),
      pheno_base,
      .problem = infoEnv$.problem,.subprob = infoEnv$.subprob,.method = infoEnv$.method
    ),
    "Non.*valid.*selectors"
  )
})

test_that("no other issues with param selector", {
  pheno_prms <- get_prm(pheno_base,quiet = TRUE)
  expect_error(
    param_selector("the2"),
    "requires.*parameter.*table"
  )
  expect_no_error(
    param_selector("the2", pheno_prms)
  )
  expect_error(
    param_selector(1, pheno_prms),
    "non.*empty.*character.*vector"
  )
  expect_error(
    param_selector("", pheno_prms),
    "non.*empty.*character.*vector"
  )

  expect_error(
    param_selector("the_k", pheno_prms),
    "does not match.*any.*valid"
  )
  expect_error(
    pheno_prms %>%
      mutate(label=ifelse(label=="IIVCL", "CL", label)) %>%
      param_selector("CL", .),
    "does not .*unambiguously"
  )
  # Selector matches only an off-diagonal (covariance) row by name
  expect_error(
    param_selector("OMEGA(2,1)", pheno_prms),
    "does not match.*non-diagonal"
  )
})

test_that("get_prm works as expected", {
  suppressMessages(expect_error(
    xpose::xpdb_ex_pk %>%
      get_prm.xp_xtras(quiet = TRUE),
    "xp_xtras"
  ))
  expect_identical(
    xpose::xpdb_ex_pk %>%
      get_prm(quiet = TRUE),
    xpose::xpdb_ex_pk %>%
      xpose::get_prm(quiet = TRUE)
  )
  expect_identical(
    pheno_base %>%
      get_prm(quiet = TRUE),
    pheno_base %>%
      xpose::set_var_units(WT="kg") %>% # turns to a simple xpose_data
      get_prm(quiet = TRUE)
  )
  expect_setequal(
    pheno_base %>%
      get_prm(quiet = TRUE) %>%
      names(),
    xpose::xpdb_ex_pk %>%
      get_prm(quiet = TRUE) %>%
      names() %>%
      c("cv","shk")
  )


  expect_setequal(
    pheno_base %>%
      get_prm(quiet = TRUE) %>%
      dplyr::pull(shk) %>%
      stats::na.omit() %>%
      as.numeric(),
    c(get_shk(pheno_base),get_shk(pheno_base, wh="eps"))
  )

  expect_equal(
    pheno_base %>%
      add_prm_association(the1~log(ome1),the2~log(ome2)) %>%
      get_prm(quiet=TRUE),
    pheno_base %>%
      get_prm(quiet=TRUE),
    ignore_attr =TRUE
  )
  expect_false(identical(
    pheno_final %>%
      add_prm_association(the1~logit(ome1),the2~logit(ome2)) %>%
      get_prm(quiet=TRUE),
    pheno_final %>%
      get_prm(quiet=TRUE)
  ))
  expect_false(identical(
    pheno_final %>%
      add_prm_association(the1~logit(ome1),the2~logit(ome1)) %>%
      get_prm(quiet=TRUE),
    pheno_final %>%
      get_prm(quiet=TRUE)
  ))
  capture.output(expect_message(
    print(pheno_final %>%
        add_prm_association(the1~logit(ome1),the2~logit(ome1)) %>%
        get_prm(quiet=TRUE)),
    "logit"
  ))

  # nmboxcox and custom associations exercise the CV calculation branches
  # dedicated to those distributions (not just log)
  suppressWarnings(expect_no_error(
    pheno_base %>%
      add_prm_association(V~nmboxcox(IIVV, lambda=0.1)) %>%
      get_prm(quiet=TRUE)
  ))
  suppressWarnings(expect_no_error(
    pheno_base %>%
      add_prm_association(V~custom(IIVV,qdist=qnorm,pdist=pnorm)) %>%
      get_prm(quiet=TRUE)
  ))
})

test_that("get_prm.default routes real xp_xtras objects to get_prm.xp_xtras", {
  expect_identical(
    get_prm.default(pheno_base, quiet = TRUE),
    get_prm(pheno_base, quiet = TRUE)
  )
})

test_that("get_prm errors informatively for unsupported/unimplemented software", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # get_prm.default: plain (non-xp_xtras) object, software == "nlmixr2"
  # -> tells the user to convert to an xp_xtras object first
  nlmixr2_like <- xpdb_ex_pk
  nlmixr2_like$summary$value[nlmixr2_like$summary$label == "software"] <- "nlmixr2"
  expect_error(
    get_prm(nlmixr2_like, quiet = TRUE),
    "nlmixr2.*convert"
  )

  # get_prm.default: plain object, software neither nonmem nor nlmixr2
  other_sw_plain <- xpdb_ex_pk
  other_sw_plain$summary$value[other_sw_plain$summary$label == "software"] <- "monolix"
  expect_error(
    get_prm(other_sw_plain, quiet = TRUE),
    "monolix.*not implemented"
  )

  # get_prm.xp_xtras: real xp_xtras object, software neither nonmem nor nlmixr2
  other_sw_xtras <- pheno_base
  other_sw_xtras$summary$value[other_sw_xtras$summary$label == "software"] <- "monolix"
  expect_error(
    get_prm(other_sw_xtras, quiet = TRUE),
    "only works for nonmem and nlmixr2"
  )
})


test_that("mutations to parameters are applied as expected", {
  suppressWarnings(expect_error(
    vismo_pomod %>%
      mutate_prm(THETA11=exp),
    "Only formula.*=.*~"
  ))
  suppressWarnings(expect_no_error(
    vismo_pomod %>%
      mutate_prm(THETA11~exp)
  ))
  expect_warning(
    pheno_base %>%
      mutate_prm(c(THETA2~log),c(the2~log)),
    "List.*should not.*"
  )
  expect_error(
    pheno_base %>%
      mutate_prm(the1+THETA2~log(the2)),
    "complex.*RHS.*\\+"
  )
  expect_error(
    pheno_base %>%
      mutate_prm(~log(the2)),
    "LHS.*empty"
  )
  expect_error(
    pheno_base %>%
      mutate_prm(the2~log(ome2)),
    "Cannot.*omega.*sigma"
  )
  expect_error(
    pheno_base %>%
      mutate_prm(sle(the2)~10),
    "LHS.*formula.*se.*selector"
  )
  expect_no_error(
    pheno_base %>%
      mutate_prm(se(the2)~10)
  )
  expect_error(
    pheno_base %>%
      mutate_prm(ome2~log(ome2)),
    "nappropriate.*selector.*theta"
  )
  expect_error(
    pheno_base %>%
      mutate_prm(THETA2~log(1:2)),
    "RHS.*length.*"
  )
  suppressWarnings(normal <- vismo_pomod %>%
                     get_prm(quiet = TRUE))
  suppressWarnings(with_function <- vismo_pomod %>%
                     mutate_prm(the12~plogis) %>%
                     get_prm(quiet = TRUE))
  suppressWarnings(with_call <- vismo_pomod %>%
    mutate_prm(the12~plogis(the12)) %>%
    get_prm(quiet = TRUE))
  expect_equal(
    with_function$value[12],
    with_call$value[12]
  )
  expect_equal(
    with_function$value[12],
    plogis(normal$value[12]),
    tolerance = 0.01
  )
  expect_failure(expect_equal(
    normal$value[12],
    with_call$value[12]
  ))
  suppressWarnings(expect_equal(
    vismo_pomod %>%
      get_prm(quiet = TRUE) %>%
      {.$se[12]},
    vismo_pomod %>%
      mutate_prm(the12~plogis, .autose = FALSE) %>%
      get_prm(quiet = TRUE) %>%
      {.$se[12]}
  ))
  suppressWarnings(expect_equal(
    vismo_pomod %>%
      mutate_prm(se(the12)~60*se(the12), .autose = FALSE) %>%
      get_prm(quiet = TRUE) %>%
      {.$se[12]},
    vismo_pomod %>%
      get_prm(quiet = TRUE) %>%
      {.$se[12]*60},
    tolerance = 0.01
  ))

  # tests of off-diagonal changes to cov and cor
  vismo_test <- vismo_pomod %>%
    mutate_prm(the12~plogis)
  vismo_cov <- vismo_test %>% xpose::get_file(ext="cov",quiet = TRUE) %>%
    dplyr::select(-1) %>%
    as.matrix()
  vismo_cov <- vismo_cov[rowSums(vismo_cov)!=0,colSums(vismo_cov)!=0]
  vismo_cor <- vismo_test %>% xpose::get_file(ext="cor",quiet = TRUE) %>%
    dplyr::select(-1) %>%
    as.matrix()
  vismo_cor <- vismo_cor[rowSums(vismo_cor)!=0,colSums(vismo_cor)!=0]
  expect_equal(
    cov2cor(vismo_cov)[lower.tri(vismo_cov)],
    vismo_cor[lower.tri(vismo_cor)],
    tolerance = 0.01
  )

  vismo_cov_normal <- vismo_pomod %>% xpose::get_file(ext="cov",quiet = TRUE) %>%
    dplyr::select(-1) %>%
    as.matrix()
  vismo_cov_normal <- vismo_cov_normal[rowSums(vismo_cov_normal)!=0,colSums(vismo_cov_normal)!=0]
  expect_equal(
      cov2cor(vismo_cov_normal)[lower.tri(vismo_cov_normal)],
      vismo_cor[lower.tri(vismo_cor)],
      tolerance = 0.01
    ) # correlation is invariant
  expect_failure(expect_equal(
    vismo_cov_normal[lower.tri(vismo_cov_normal)],
    vismo_cov[lower.tri(vismo_cov)],
    tolerance = 0.01
  ))
})

test_that("mutate_prm with no mutations is a no-op", {
  expect_identical(
    mutate_prm(pheno_base),
    pheno_base
  )
})

test_that("mutate_prm_check rejects non-formula input directly", {
  expect_error(
    mutate_prm_check(seems~valid, pheno_base),
    "Mutations must be formulas"
  )
})

test_that("mutate_prm warns and skips SE updates when covariance is unavailable", {
  # Strip the SE row from `ext` and drop `cov`/`cor` files entirely, so that
  # every parameter's SE is NA (mirrors a model run without a covariance step)
  no_cov <- pheno_final
  extrow <- which(no_cov$files$extension == "ext")
  no_cov$files$data[[extrow]] <- no_cov$files$data[[extrow]] %>%
    dplyr::filter(ITERATION != -1000000001)
  no_cov$files <- no_cov$files %>%
    dplyr::filter(!extension %in% c("cov", "cor"))
  no_cov <- as_xp_xtras(no_cov)

  expect_warning(
    updated <- no_cov %>% mutate_prm(the1~99),
    "Covariance matrix is not available"
  )
  # Value change still applies; SE update is silently skipped (no error)
  expect_equal(
    as.numeric(get_prm(updated, quiet=TRUE)$value[1]),
    99
  )
})

test_that("mutate_prm warns when autose falls back to RSE scaling for plain values", {
  expect_warning(
    pheno_final %>% mutate_prm(the1~99, quiet = FALSE),
    "function was not provided"
  )
})

test_that("mutate_prm enforces length-1 results for SE mutations too", {
  expect_error(
    pheno_final %>% mutate_prm(se(the1)~c(1,2)),
    "RHS.*length.*1"
  )
})

test_that("mutate_prm reports unimplemented software for value and SE changes", {
  other_sw <- pheno_final
  other_sw$summary$value[other_sw$summary$label == "software"] <- "monolix"

  testthat::local_mocked_bindings(
    hot_swap_base_get_prm = function(xpdb, ...) xpose::get_prm(xpdb, ...),
    .package = "xpose.xtras"
  )

  # Fixed-effect value change: not implemented for this software
  expect_error(
    other_sw %>% mutate_prm(the1~99, .problem = 1, .subprob = 1, .method = "foce"),
    "not yet implemented for.*monolix"
  )

  # SE-only change: silently returns unmodified (early-return branch, since
  # other software would need SE handled differently)
  expect_identical(
    other_sw %>% mutate_prm(se(the1)~10, .problem = 1, .subprob = 1, .method = "foce"),
    other_sw
  )
})

test_that("covariate associations can be added", {
  # Successful builtins
  expect_no_error(
    xpdb_x %>% add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64))
  )
  expect_no_error(
    xpdb_x %>% add_cov_association(TVCL ~ linear(CLCR, THETA7, ref = 64))
  )
  expect_no_error(
    xpdb_x %>% add_cov_association(TVCL ~ exponential(CLCR, THETA7, ref = 64))
  )
  expect_no_error(
    xpdb_x %>% add_cov_association(TVCL ~ hockey(CLCR, THETA7, THETA4, ref = 64))
  )
  expect_no_error(
    xpdb_x %>% add_cov_association(TVCL ~ catshift(SEX, THETA4, ref = 1))
  )
  expect_no_error(
    xpdb_x %>% add_cov_association(
      TVCL ~ custom(CLCR, THETA7, ref = 64,
                    fun = function(cov, ref, theta) (cov/ref)^theta)
    )
  )

  # Sharing one association across multiple parameters (LHS with `+`)
  expect_no_error(
    xpdb_x %>% add_cov_association(TVCL + TVV ~ power(CLCR, THETA7, ref = 64))
  )

  # Empty dots is a no-op, identical object returned
  expect_identical(
    xpdb_x %>% add_cov_association(),
    xpdb_x
  )

  # `ref` is always required, no implicit default
  expect_error(
    xpdb_x %>% add_cov_association(TVCL ~ power(CLCR, THETA7)),
    "ref"
  )

  # Wrong theta count for a given builtin
  expect_error(
    xpdb_x %>% add_cov_association(TVCL ~ power(CLCR, THETA7, THETA4, ref = 64)),
    "exactly one theta"
  )
  expect_error(
    xpdb_x %>% add_cov_association(TVCL ~ hockey(CLCR, THETA7, ref = 64)),
    "exactly two theta"
  )
  expect_error(
    xpdb_x %>% add_cov_association(TVCL ~ catshift(SEX, THETA4, THETA6, ref = 1)),
    "one theta per non-reference level"
  )

  # covtype/assoc mismatches
  expect_error(
    xpdb_x %>% add_cov_association(TVCL ~ catshift(CLCR, THETA4, ref = 1)),
    "continuous"
  )
  expect_error(
    xpdb_x %>% add_cov_association(TVCL ~ power(SEX, THETA4, ref = 1)),
    "categorical"
  )

  # Unknown covariate / bad selectors
  expect_error(
    xpdb_x %>% add_cov_association(TVCL ~ power(NOTACOL, THETA7, ref = 64)),
    "contcov"
  )
  expect_error(
    xpdb_x %>% add_cov_association(TVCL ~ power(CLCR, ome1, ref = 64)),
    "fixed-effect"
  )
  expect_error(
    xpdb_x %>% add_cov_association(NOTAPARAM ~ power(CLCR, THETA7, ref = 64))
  )

  # Duplicate (param, covariate) pair in one call
  expect_error(
    xpdb_x %>% add_cov_association(
      TVCL ~ power(CLCR, THETA7, ref = 64),
      TVCL ~ linear(CLCR, THETA7, ref = 64)
    ),
    "same \\(parameter, covariate\\)"
  )

  # custom() must satisfy fun(ref, ref, theta) == 1 for any theta
  expect_error(
    xpdb_x %>% add_cov_association(
      TVCL ~ custom(CLCR, THETA7, ref = 64,
                    fun = function(cov, ref, theta) cov/ref + theta)
    ),
    "fun\\(ref, ref, theta\\)"
  )

  # Redeclaring an association for the same (param, covariate) replaces it,
  # regardless of which valid selector form is used for the parameter
  replaced <- xpdb_x %>%
    add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64)) %>%
    add_cov_association(THETA1 ~ linear(CLCR, THETA7, ref = 50))
  expect_equal(nrow(replaced$covs), 1)
  expect_equal(replaced$covs$assoc, "linear")
  expect_equal(replaced$covs$ref[[1]], 50)
})

test_that("add_cov_association requires an xp_xtras object and warns on list dots", {
  suppressWarnings(suppressMessages(expect_error(
    add_cov_association(xpose::xpdb_ex_pk, TVCL ~ power(CLCR, THETA7, ref = 64)),
    "xp_xtras.*required"
  )))
  expect_warning(
    xpdb_x %>% add_cov_association(list(TVCL ~ power(CLCR, THETA7, ref = 64))),
    "List.*should not.*"
  )
})

test_that("check_cov_associations catches every malformed-formula shape", {
  expect_error(
    check_cov_associations(seems~valid, xpdb_x),
    "list of formulas"
  )
  expect_error(
    check_cov_associations(c(~missing_lhs), xpdb_x),
    "LHS.*empty"
  )
  expect_error(
    check_cov_associations(c(lhs~not_function), xpdb_x),
    "RHS.*function call"
  )
  expect_error(
    check_cov_associations(c(lhs~hhh(cov,ref=1)), xpdb_x),
    "RHS.*call to one of"
  )
  expect_error(
    check_cov_associations(c(TVCL~power(ref=64)), xpdb_x),
    "covariate as its first"
  )
  expect_error(
    check_cov_associations(c(TVCL~catshift(SEX, ref=1)), xpdb_x),
    "catshift.*at least one theta"
  )
  expect_error(
    check_cov_associations(c(TVCL~custom(CLCR, ref=64, fun=identity)), xpdb_x),
    "custom.*at least one theta"
  )
  expect_error(
    check_cov_associations(c(TVCL~custom(CLCR, THETA7, ref=64)), xpdb_x),
    "custom.*named.*fun"
  )

  infoEnv <- new.env()
  fill_prob_subprob_method(xpdb_x, envir = infoEnv)
  expect_error(
    check_cov_associations(
      c(ome1~power(CLCR, THETA7, ref=64)),
      xpdb_x,
      .problem = infoEnv$.problem, .subprob = infoEnv$.subprob, .method = infoEnv$.method
    ),
    "fixed-effect.*theta"
  )
  expect_error(
    check_cov_associations(
      c(TVCL~custom(CLCR, THETA7, ref=64, fun="notafun")),
      xpdb_x,
      .problem = infoEnv$.problem, .subprob = infoEnv$.subprob, .method = infoEnv$.method
    ),
    "fun.*argument must be a function"
  )
})

test_that("covariate associations can be dropped", {
  with_assoc <- xpdb_x %>%
    add_cov_association(
      TVCL ~ power(CLCR, THETA7, ref = 64),
      TVCL ~ catshift(SEX, THETA4, ref = 1)
    )
  expect_equal(nrow(with_assoc$covs), 2)

  dropped_one <- with_assoc %>% drop_cov_association(TVCL ~ CLCR)
  expect_equal(nrow(dropped_one$covs), 1)
  expect_equal(dropped_one$covs$covariate, "SEX")

  # Dropping something not present is a no-op
  expect_equal(
    nrow((with_assoc %>% drop_cov_association(TVV ~ CLCR))$covs),
    2
  )

  # Empty dots is a no-op
  expect_identical(
    with_assoc %>% drop_cov_association(),
    with_assoc
  )

  # Selector form for the parameter doesn't need to match how it was declared
  dropped_by_name <- with_assoc %>% drop_cov_association(THETA1 ~ CLCR)
  expect_equal(nrow(dropped_by_name$covs), 1)
})

test_that("drop_cov_association validates its inputs and its own edge cases", {
  suppressWarnings(suppressMessages(expect_error(
    drop_cov_association(xpose::xpdb_ex_pk, TVCL ~ CLCR),
    "xp_xtras.*required"
  )))
  expect_error(
    xpdb_x %>% drop_cov_association("TVCL~CLCR"),
    "must be formulas of the form"
  )
  expect_error(
    xpdb_x %>% drop_cov_association(TVCL ~ power(CLCR)),
    "bare covariate column name"
  )
  # No associations declared at all yet -> no-op
  expect_identical(
    xpdb_x %>% drop_cov_association(TVCL ~ CLCR),
    xpdb_x
  )
})

test_that("cov_effect_fun/cov_effect_ci internals behave as documented", {
  # Unsupported (eg catshift, which is handled separately in prm_catcov())
  expect_error(
    cov_effect_fun("catshift", list()),
    "Unsupported association type"
  )
  # se == NA with the delta method returns NA bounds rather than erroring
  ci <- cov_effect_ci(
    fun = function(cov, ref, theta) 1 + theta[1]*(cov-ref),
    cov = c(60, 64, 70), ref = 64, theta = 0.01, se = NA_real_,
    ci_method = "delta", level = 0.95, nsim = 100
  )
  expect_true(all(is.na(ci$low)))
  expect_true(all(is.na(ci$high)))
})

test_that("as_prm_tbl is idempotent", {
  once <- as_prm_tbl(tibble::tibble(a = 1))
  twice <- as_prm_tbl(once)
  expect_identical(once, twice)
})

test_that("prm_contcov/prm_catcov/prm_cov require xp_xtras objects", {
  suppressWarnings(suppressMessages(expect_error(
    prm_contcov(xpose::xpdb_ex_pk),
    "xp_xtras.*required"
  )))
  suppressWarnings(suppressMessages(expect_error(
    prm_catcov(xpose::xpdb_ex_pk),
    "xp_xtras.*required"
  )))
  suppressWarnings(suppressMessages(expect_error(
    prm_cov(xpose::xpdb_ex_pk),
    "xp_xtras.*required"
  )))
})

test_that("prm_contcov covers the linear/exponential/custom builtins and empty-after-filter", {
  x_linear <- xpdb_x %>% add_cov_association(TVCL ~ linear(CLCR, THETA7, ref = 64))
  out_linear <- x_linear %>% prm_contcov()
  expect_equal(out_linear$effect[out_linear$level == "ref"], 1)

  x_exp <- xpdb_x %>% add_cov_association(TVCL ~ exponential(CLCR, THETA7, ref = 64))
  out_exp <- x_exp %>% prm_contcov()
  expect_equal(out_exp$effect[out_exp$level == "ref"], 1)

  x_custom <- xpdb_x %>% add_cov_association(
    TVCL ~ custom(CLCR, THETA7, ref = 64, fun = function(cov, ref, theta) (cov/ref)^theta[1])
  )
  out_custom <- x_custom %>% prm_contcov()
  expect_equal(out_custom$effect[out_custom$level == "ref"], 1)

  # Selector doesn't match any declared cont association -> empty, not an error
  empty_out <- x_linear %>% prm_contcov(TVCL ~ WT)
  expect_equal(nrow(empty_out), 0)

  # Malformed selector (RHS not a bare name) is rejected
  expect_error(
    x_linear %>% prm_cov(TVCL ~ notaname(CLCR)),
    "Non-valid selector"
  )
})

test_that("prm_catcov covers the custom association branch", {
  x <- xpdb_x %>% add_cov_association(
    TVCL ~ custom(SEX, THETA4, ref = 1,
                  fun = function(cov, ref, theta) 1 + theta[1]*(as.numeric(cov)-as.numeric(ref)))
  )
  out <- x %>% prm_catcov()
  expect_equal(nrow(out), 2)
  expect_equal(out$effect[out$level == "1"], 1)
  expect_true(out$effect[out$level == "2"] != 1)

  # keep_draws is also supported for the custom() branch
  out_draws <- x %>% prm_catcov(keep_draws = TRUE, nsim = 50)
  expect_true("draws" %in% names(out_draws))
  expect_true(all(lengths(out_draws$draws) == 50))
})

test_that("prm_contcov computes continuous covariate effects", {
  prm <- xpose::get_prm(xpdb_x, .problem = 1, transform = FALSE, quiet = TRUE)
  theta7 <- prm$value[prm$label == "CRCL on CL"]
  cldata <- xpose::get_data(xpdb_x, .problem = 1, quiet = TRUE)$CLCR
  qs <- stats::quantile(cldata, probs = c(0.05, 0.95), na.rm = TRUE, names = FALSE)

  x <- xpdb_x %>% add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64))
  out <- x %>% prm_contcov()

  expect_equal(nrow(out), 3)
  expect_equal(out$level, c("low", "ref", "high"))
  expect_equal(out$effect[out$level == "ref"], 1)
  expect_equal(out$ci_low[out$level == "ref"], 1)
  expect_equal(out$ci_high[out$level == "ref"], 1)
  expect_equal(out$effect[out$level == "low"], (qs[1]/64)^theta7, tolerance = 1e-6)
  expect_equal(out$effect[out$level == "high"], (qs[2]/64)^theta7, tolerance = 1e-6)
  expect_true(all(out$ci_low <= out$effect + 1e-8))
  expect_true(all(out$effect <= out$ci_high + 1e-8))

  # Delta method gives the same point estimate, and a sane (bracketing) CI
  out_delta <- x %>% prm_contcov(ci_method = "delta")
  expect_equal(out_delta$effect, out$effect, tolerance = 1e-6)
  expect_true(all(out_delta$ci_low <= out_delta$effect + 1e-8))
  expect_true(all(out_delta$effect <= out_delta$ci_high + 1e-8))
})

test_that("prm_catcov computes categorical covariate effects", {
  prm <- xpose::get_prm(xpdb_x, .problem = 1, transform = FALSE, quiet = TRUE)
  theta4 <- prm$value[prm$label == "LAG"] # reused purely for illustration

  x <- xpdb_x %>% add_cov_association(TVCL ~ catshift(SEX, THETA4, ref = 1))
  out <- x %>% prm_catcov()

  expect_equal(nrow(out), 2)
  ref_row <- out[out$level == "1", ]
  other_row <- out[out$level == "2", ]
  expect_equal(ref_row$effect, 1)
  expect_equal(ref_row$ci_low, 1)
  expect_equal(ref_row$ci_high, 1)
  expect_equal(other_row$effect, 1 + theta4, tolerance = 1e-8)
  expect_true(other_row$ci_low <= other_row$effect)
  expect_true(other_row$effect <= other_row$ci_high)
})

test_that("prm_cov combines cont+cat and supports selector filtering", {
  x <- xpdb_x %>%
    add_cov_association(
      TVCL ~ power(CLCR, THETA7, ref = 64),
      TVCL ~ catshift(SEX, THETA4, ref = 1)
    )
  full <- x %>% prm_cov()
  expect_equal(nrow(full), 5)
  expect_setequal(unique(full$covariate), c("CLCR", "SEX"))

  filtered <- x %>% prm_cov(TVCL ~ CLCR)
  expect_equal(nrow(filtered), 3)
  expect_true(all(filtered$covariate == "CLCR"))

  # No associations declared -> empty tibble, not an error
  empty_out <- xpdb_x %>% prm_cov()
  expect_equal(nrow(empty_out), 0)
})

test_that("hockey and additive builtins compute distinct/expected effects", {
  prm <- xpose::get_prm(xpdb_x, .problem = 1, transform = FALSE, quiet = TRUE)
  theta7 <- prm$value[prm$label == "CRCL on CL"]
  theta4 <- prm$value[prm$label == "LAG"]
  cldata <- xpose::get_data(xpdb_x, .problem = 1, quiet = TRUE)$CLCR
  qs <- stats::quantile(cldata, probs = c(0.05, 0.95), na.rm = TRUE, names = FALSE)

  x <- xpdb_x %>% add_cov_association(TVCL ~ hockey(CLCR, THETA7, THETA4, ref = 64))
  out <- x %>% prm_contcov()
  expect_equal(out$effect[out$level == "low"], 1 + theta7*(qs[1]-64), tolerance = 1e-6)
  expect_equal(out$effect[out$level == "high"], 1 + theta4*(qs[2]-64), tolerance = 1e-6)

  x2 <- xpdb_x %>% add_cov_association(TVCL ~ additive(CLCR, THETA7, ref = 64))
  out2 <- x2 %>% prm_contcov()
  expect_equal(out2$effect[out2$level == "low"], (theta7+qs[1])/(theta7+64), tolerance = 1e-6)
  expect_equal(out2$effect[out2$level == "high"], (theta7+qs[2])/(theta7+64), tolerance = 1e-6)
})

test_that("prm_cov_tbl has a custom print method", {
  x <- xpdb_x %>% add_cov_association(TVCL ~ power(CLCR, THETA7, ref = 64))
  out <- x %>% prm_cov()
  expect_s3_class(out, "prm_cov_tbl")
  expect_message(print(out), "ratio to the parameter")

  empty_out <- xpdb_x %>% prm_cov()
  expect_s3_class(empty_out, "prm_cov_tbl")
  expect_no_message(print(empty_out))
})

test_that("keep_draws exposes raw simulation draws", {
  x <- xpdb_x %>%
    add_cov_association(
      TVCL ~ power(CLCR, THETA7, ref = 64),
      TVCL ~ catshift(SEX, THETA4, ref = 1)
    )

  out <- x %>% prm_cov(keep_draws = TRUE, nsim = 500)
  expect_true("draws" %in% names(out))
  expect_true(all(lengths(out$draws) == 500))

  # No uncertainty at the reference level/value, by construction
  expect_true(all(out$draws[[which(out$level == "ref")]] == 1))
  expect_true(all(out$draws[[which(out$level == "1")]] == 1))

  # Non-reference draws are centered near the point estimate
  other <- out$draws[[which(out$level == "2")]]
  expect_equal(mean(other), out$effect[out$level == "2"], tolerance = 0.05)

  # keep_draws requires simulation
  expect_error(
    x %>% prm_cov(ci_method = "delta", keep_draws = TRUE),
    "requires.*simulation"
  )

  # Default (keep_draws = FALSE) has no draws column
  expect_false("draws" %in% names(x %>% prm_cov()))
})
