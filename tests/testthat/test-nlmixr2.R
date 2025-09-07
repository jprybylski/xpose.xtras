test_that("nlmixr2 is compatible", {
  expect_no_error(
    as_xp_xtras(xpdb_nlmixr2)
  )
  expect_no_error(
    as_xp_xtras(xpdb_nlmixr2_saem)
  )
  fit_example <- nlmixr2_m3$fit
  expect_no_error(
    nlmixr2_as_xtra(fit_example)
  )
  expect_no_error(
    nlmixr2_as_xtra(fit_example, .skip_assoc = TRUE)
  )
  expect_failure(expect_identical(
    nlmixr2_as_xtra(fit_example),
    nlmixr2_as_xtra(fit_example, .skip_assoc = TRUE)
  ))
  # Another example
  expect_no_error(
    nlmixr2_as_xtra(nlmixr2_warfarin$fit, quiet = TRUE)
  )
  expect_no_error(
    nlmixr2_as_xtra(nlmixr2_warfarin$fit, .skip_assoc = TRUE, quiet = TRUE)
  )

  # Make sure properties can be found and manipulated
  # Including: get_prop, set_prop, backfill and options functions
  fill_test <- function(xpdb,...) {
    fill_prob_subprob_method(xpdb, ...)
    as.list(environment(), all.names = TRUE)
  }
  expect_identical(
    fill_test(xpdb_nlmixr2)$.method,
    "focei"
  )
  expect_identical(
    fill_test(xpdb_nlmixr2_saem)$.method,
    "saem"
  )
  no_summ_test <- xpdb_nlmixr2
  no_summ_test$summary <- dplyr::filter(no_summ_test$summary,label!="method")
  expect_warning(
    fill_test(as_xp_xtras(no_summ_test)),
    regexp = "may not be compatible"
  )
  expect_equal(
    as.numeric(get_prop(xpdb_nlmixr2, "condn")),
    xpdb_nlmixr2$fit$conditionNumberCov,
    ignore_attr = TRUE
  )
  random_string <- paste(sample(letters,12), collapse="")
  expect_identical(
    set_prop(xpdb_nlmixr2, descr = random_string) %>%
      get_prop("descr"),
    random_string
  )
  expect_no_error(
    backfill_iofv(xpdb_nlmixr2)
  )
  expect_message(
    backfill_iofv(set_option(xpdb_nlmixr2_saem, quiet=FALSE)),
    "Some iOFV values for problem.*are not finite"
  )
  expect_in(
    random_string,
    names(xpose::get_data(backfill_iofv(xpdb_nlmixr2, .label = random_string), quiet = TRUE))
  )
  expect_no_error(
    # pulling a typical problem 0 property is nonmem that is a problem 1 prop for nlmixr2
    get_prop(xpdb_nlmixr2, "file", .problem = 0)
  )

  # Make sure new single xpdb functions can be run without error
  expect_no_error(
    eta_grid(xpdb_nlmixr2, quiet=TRUE)
  )
  expect_no_error(
    eta_vs_contcov(xpdb_nlmixr2, quiet=TRUE)
  )
  expect_no_error(
    eta_vs_catcov(nlmixr2_warfarin, quiet=TRUE)
  )
  expect_no_error(
    eta_vs_cov_grid(nlmixr2_warfarin, quiet=TRUE)
  )
  expect_no_error({
    nlmixr2_m3 %>% # modified from catdv_vs_dvprobs example
      set_var_types(catdv=CENS,dvprobs=BLQLIKE) %>%
      set_dv_probs(1, 1~BLQLIKE, .dv_var = CENS) %>%
      set_var_levels(1, CENS = lvl_bin()) %>%
      catdv_vs_dvprobs(xlab = "basic", quiet = TRUE)
  })
  suppressMessages(expect_no_error(
    list_vars(nlmixr2_warfarin)
  ))

  # Make sure xpose_sets can be made (several iterations)
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      xpdb_nlmixr2_saem
    )
  )
  expect_no_warning(
    xpose_set(
      xpdb_nlmixr2,
      xpdb_nlmixr2_saem
    )
  )
  expect_length(
    xpose_set(
      xpdb_nlmixr2,
      xpdb_nlmixr2_saem,
      foo=xpdb_nlmixr2_saem,
      foo2=xpdb_nlmixr2_saem
    ),
    4
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3
    ) %>%
      focus_qapply(backfill_iofv)
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3, .relationships = nlmixr2_m3 ~ xpdb_nlmixr2
    )
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3,
      .as_ordered = TRUE
    )
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3
    ) %>%
      expose_param(tka)
  )
  expect_no_error(
    xpose_set(
      xpdb_nlmixr2,
      nlmixr2_m3
    ) %>%
      expose_property(file)
  )

  # Make sure model comparison plots can be created
  comparison <- xpose_set(
    xpdb_nlmixr2,
    nlmixr2_m3
  ) %>%
    focus_qapply(backfill_iofv)
  suppressMessages(expect_no_error(
    ipred_vs_ipred(comparison, quiet = TRUE)
  ))
  suppressMessages(expect_message(
    ipred_vs_ipred(comparison, quiet = TRUE),
    "Duplicate.*axis.text"
  ))
  suppressMessages(expect_message(
    ipred_vs_ipred(comparison, quiet = TRUE),
    "nlmixr2.*@file.*@run"
  ))
  expect_no_message(
    ipred_vs_ipred(comparison, quiet = TRUE, axis.text = "@file")
  )
  expect_no_error(
    pred_vs_pred(comparison, quiet = TRUE, axis.text = "@file")
  )
  expect_no_error(
    iofv_vs_mod(comparison, quiet = TRUE, axis.text = "@file")
  )
  expect_no_error(
    prm_waterfall(comparison, quiet = TRUE)
  )
  expect_no_error(
    eta_waterfall(comparison, quiet = TRUE)
  )
  expect_no_error(
    iofv_waterfall(comparison, quiet = TRUE)
  )
  expect_no_error(
    shark_plot(comparison, quiet = TRUE, df=1)
  )

  # Make sure model-averaging plots can be created
  expect_no_error(
    ipred_vs_idv_modavg(comparison, quiet = TRUE)
  )
  expect_no_error(
    pred_vs_idv_modavg(comparison, quiet = TRUE)
  )
  expect_no_error(
    dv_vs_ipred_modavg(comparison, quiet = TRUE)
  )
  expect_no_error(
    dv_vs_pred_modavg(comparison, quiet = TRUE)
  )


  # Make sure get_prm and prm associations work
  expect_no_error(
    get_prm(xpdb_nlmixr2, quiet = TRUE)
  )
  expect_message(
    get_prm(xpdb_nlmixr2, quiet = FALSE),
    regexp = "does not provide SE.*random effect"
  )
  suppressMessages(expect_message(
    nlmixr2_prm_associations(nlmixr2_warfarin, quiet = FALSE),
    regexp = "need to untransform thetas"
  ))
  suppressMessages(expect_message(
    nlmixr2_prm_associations(nlmixr2_warfarin, quiet = FALSE),
    regexp = "mutate_prm"
  ))
  expect_no_message(
    nlmixr2_prm_associations(xpdb_nlmixr2),
    message = "need to untransform thetas"
  )
  expect_equal(
    # all etas are log
    nlmixr2_prm_associations(set_option(xpdb_nlmixr2,quiet=TRUE)) %>%
      get_prm(),
    get_prm(xpdb_nlmixr2,quiet=TRUE),
    ignore_attr = TRUE
  )
  suppressWarnings(expect_failure(expect_equal(
    # warfarin model has logit exp
    nlmixr2_prm_associations(set_option(nlmixr2_warfarin,quiet=TRUE)) %>%
      get_prm(),
    get_prm(nlmixr2_warfarin,quiet=TRUE),
    ignore_attr = TRUE
  )))
  suppressWarnings(expect_warning(
    nlmixr2_prm_associations(set_option(nlmixr2_warfarin,quiet=TRUE)) %>%
      get_prm(),
    "NaNs produced"
  ))
  expect_no_warning(
    nlmixr2_prm_associations(set_option(nlmixr2_warfarin,quiet=TRUE)) %>%
      mutate_prm(temax~plogis) %>%
      get_prm(),
    message = "NaNs produced"
  )

})


test_that("pure LL fits can be used", {
  skip_if_not_installed("nlmixr2est")

  # From https://github.com/nlmixr2/nlmixr2est/issues/218#issue-1366433669
  markov_nlmixr <- function() {
    ini({
      logitp02 <- logit(0.2) ; label("Probablity of transition from 0 to 2")
      logitp20 <- logit(0.2) ; label("Probablity of transition from 2 to 0")
      eta.p02 ~ 0.1 # need IIV https://github.com/nlmixr2/xpose.nlmixr2/issues/8#issue-3304662799
    })
    model({
      tp02 <- expit(logitp02)
      tp00 <- 1 - tp02
      p02 <- expit(logitp02 + eta.p02)
      p00 <- 1 - p02
      p20 <- expit(logitp20)
      p22 <- 1 - p20

      current_p <-
        p02*(PDV == 0 & DV == 2) +
        p00*(PDV == 0 & DV == 0) +
        p20*(PDV == 2 & DV == 0) +
        p22*(PDV == 2 & DV == 2)
      ll(err) ~ log(current_p)

      # Need pred and res https://github.com/nlmixr2/xpose.nlmixr2/issues/7#issue-3304654465
      # user pop predicted
      pred  <-
        tp02*(PDV == 0 & DV == 2) +
        p00*(PDV == 0 & DV == 0) +
        p20*(PDV == 2 & DV == 0) +
        p22*(PDV == 2 & DV == 2)

      # User pwres
      p0 = p20 + p00
      p2 = p02 + p22
      pipred = 0*p0 + 2*p2
      sdpred = sqrt( p0*(0 - pipred)^2 + p2*(2 - pipred)^2  )
      pwres = (DV - pipred)/sdpred
    })
  }

  d_mod <-
    data.frame(
      ID=rep(1:10, each=11),
      CMT="markov"
    ) |>
    dplyr::group_by(ID) |>
    dplyr::mutate(
      DV=
        dplyr::case_when(
          (ID %% 2) == 1~c(rep(0, 6), rep(2, 5)),
          TRUE~c(rep(0, 3), rep(2, 3), rep(0, 5))
        ),
      PDV=dplyr::lag(DV, 1),
      TIME=seq_len(dplyr::n()) - 2
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(PDV))

  mmfit <- suppressMessages({
    nlmixr2est::nlmixr(object = markov_nlmixr, data = d_mod,
                       est = "focei", control = list(print = 0, outerOpt = "bobyqa"))
  })

  expect_no_error(
    xpose.nlmixr2::xpose_data_nlmixr(mmfit, pred = "pred", wres = "pwres")
  )
  expect_no_error(
    xpose.nlmixr2::xpose_data_nlmixr(mmfit, pred = "pred", wres = "pwres") %>%
      as_xp_xtras()
  )
  expect_no_error(
    xpose.nlmixr2::xpose_data_nlmixr(mmfit, pred = "pred", wres = "pwres") %>%
      attach_nlmixr2(mmfit)
  )
  mm_xpdb <- xpose.nlmixr2::xpose_data_nlmixr(mmfit, pred = "pred", wres = "pwres") %>%
    attach_nlmixr2(mmfit) %>%
    as_xp_xtras()
  expect_true(
    test_nlmixr2_has_fit(mm_xpdb)
  )
  expect_no_error(
    nlmixr2_as_xtra(mmfit, pred = "pred", wres = "pwres", .skip_assoc = TRUE)
  )
  expect_no_error(
    nlmixr2_as_xtra(mmfit, pred = "pred", wres = "pwres",
                    quiet=TRUE, .skip_assoc = FALSE)
  )
  expect_no_warning(
    nlmixr2_as_xtra(mmfit, pred = "pred", wres = "pwres",
                    quiet=TRUE, .skip_assoc = FALSE) %>%
      mutate_prm(the1~plogis,the2~plogis) %>%
      get_prm()
  )

})
