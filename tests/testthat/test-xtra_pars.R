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
