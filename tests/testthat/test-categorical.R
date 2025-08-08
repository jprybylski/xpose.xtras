test_that("DV probabilities can be set", {
  suppressMessages(expect_error(
    set_dv_probs(xpose::xpdb_ex_pk, 1~MED1),
    "xp_xtras.*required"
  ))
  suppressMessages(expect_error(
    set_dv_probs(pkpd_m3, 1~LIKE, .problem=99),
    "99.*not valid"
  ))

  xpx_w_types <- pkpd_m3 %>%
    set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE)
  expect_identical(
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ),
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE)
  )
  expect_error(
    pkpd_m3 %>%
      set_dv_probs(.problem=1, 1~LIKE)
  )
  expect_error(
    xpx_w_types %>%
      set_dv_probs(.problem=1, LIKE=1),
    "Only formula.*expected.* not assignment.*=.*~"
  )

  suppressWarnings(expect_warning(
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ, .handle_missing = "warn"),
    "values.*missing in probabilities"
  ))
  suppressWarnings(expect_error(
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ, .handle_missing = "error"),
    "values.*missing in probabilities"
  ))

  suppressWarnings(expect_warning(
    xpx_w_types %>%
      set_dv_probs(.problem=1, 99~LIKE, .dv_var = BLQ, .handle_missing = "warn"),
    "not in.*BLQ.*99"
  ))
  suppressWarnings(expect_error(
    xpx_w_types %>%
      set_dv_probs(.problem=1, 99~LIKE, .dv_var = BLQ, .handle_missing = "error"),
    "not in.*BLQ.*99"
  ))

  expect_identical(
    get_index(pkpd_m3) %>%
      dplyr::filter(col=="TIME") %>%
      dplyr::pull(probs) %>%
      .[[1]],
    get_index(pkpd_m3) %>%
      dplyr::filter(col=="BLQ") %>%
      dplyr::pull(probs) %>%
      .[[1]]
  )

  expect_identical(
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE) %>%
      get_index() %>%
      dplyr::filter(col=="BLQ") %>%
      dplyr::pull(probs) %>%
      .[[1]],
    proc_probs(c(1~LIKE))
  )
  expect_identical(
    pkpd_m3 %>%
      set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
      set_dv_probs(.problem=1, 1~LIKE) %>%
      get_index() %>%
      dplyr::filter(col=="BLQ") %>%
      dplyr::pull(probs) %>%
      .[[1]],
    pkpd_m3 %>%
      set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
      set_dv_probs(.problem=1, 1~LIKE) %>%
      list_dv_probs()
  )
  expect_identical(
    pkpd_m3 %>%
      set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
      set_dv_probs(.problem=1, 1~LIKE) %>%
      get_index() %>%
      dplyr::filter(col=="BLQ") %>%
      dplyr::pull(probs) %>%
      .[[1]],
    pkpd_m3 %>%
      set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
      set_dv_probs(.problem=1, 1~LIKE) %>%
      list_dv_probs(.dv_var = BLQ)
  )


})

test_that("errors in DV prob declarations can be caught", {
  xpx_w_types <- pkpd_m3 %>%
    set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE)
  defs <- list(
    prb_list = c(1~LIKE),
    index = get_index(xpx_w_types),
    dvcol = "BLQ"
  )

  expect_error(
    check_probs(c(LIKE~1),defs$index,defs$dvcol),
    "invalid syntax"
  )

  expect_error(
    check_probs(c(1~fakelike),defs$index,defs$dvcol),
    "not in data.*fakelike"
  )

  expect_error(
    check_probs(c(0~LIKE,1~LIKE),defs$index,defs$dvcol),
    "use same prob.*multiple.*new column.*pseudo"
  )

  expect_warning(
    check_probs(c(eq(1)~LIKE),defs$index,defs$dvcol),
    "avoid.*eq.*implied"
  )
  suppressWarnings(expect_identical(
    check_probs(c(eq(1)~LIKE),defs$index,defs$dvcol),
    check_probs(c(1~LIKE),defs$index,defs$dvcol)
  ))


  expect_error(
    check_probs(c(mmm(1)~LIKE),defs$index,defs$dvcol),
    "No available method.*mmm"
  )
  expect_no_error(
    check_probs(c(GT(1)~LIKE),defs$index,defs$dvcol),
    message="No available method.*GT"
  )
  expect_identical(
    check_probs(c(GT(1)~LIKE),defs$index,defs$dvcol),
    check_probs(c(gt(1)~LIKE),defs$index,defs$dvcol)
  )

  expect_warning(
    pkpd_m3 %>%
      set_var_types(.problem=1, catdv=BLQ) %>%
      set_dv_probs(.problem=1, 1~LIKE),
    "type.*not properly assigned.*dvprobs.*still.*applied.*LIKE"
  )

  expect_warning(
    pkpd_m3 %>%
      set_var_types(.problem=1, dvprobs=LIKE) %>%
      set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ),
    "type.*not properly assigned.*catdv.*still.*applied.*BLQ"
  )

})

test_that("catdv can be plot against dvprobs", {
  m3_test_dummy <- pkpd_m3 %>%
    set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
    set_dv_probs(.problem=1, 1~LIKE)

  expect_warning(
    m3_test_dummy %>%
      set_var_types(catdv=DOSE, quiet = TRUE) %>%
      catdv_vs_dvprobs(quiet=TRUE),
    "Only one.*cat.*DV.*used.*BLQ"
  )

  expect_error(
    pkpd_m3 %>%
      set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
      catdv_vs_dvprobs(quiet=TRUE),
    "Relationship between probabiliy column and at least one categorical DV level should be defined"
  )

  expect_error(
    m3_test_dummy %>%
      catdv_vs_dvprobs(cutpoint = 99, quiet=TRUE),
    "cutpoint.*is.*row number.*1.*99.*range"
  )

  test_plot <- m3_test_dummy %>%
    catdv_vs_dvprobs(quiet=TRUE)

  expect_equal(
    test_plot$mapping$x,
    quote(~.data[["LIKE"]]),
    ignore_attr = TRUE
  )
  expect_equal(
    test_plot$mapping$y,
    quote(~.data[["BLQ"]]),
    ignore_attr = TRUE
  )
  expect_setequal(
    test_plot$data$BLQ,
    c("NE(1)","EQ(1)")
  )
  expect_equal(
    test_plot$labels$x,
    "Probability BLQ EQ(1)"
  )
  expect_equal(
    m3_test_dummy %>%
      catdv_vs_dvprobs(quiet=TRUE, xlab = "basic") %>%
      {.$labels$x},
    "LIKE"
  )

  vismo_xpdb <- vismo_pomod  %>%
    set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
    set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)
  test_plot2 <- vismo_xpdb %>%
    catdv_vs_dvprobs(quiet=TRUE)
  expect_equal(
    test_plot2$mapping$x,
    quote(~.data[["P0"]]),
    ignore_attr = TRUE
  )
  expect_equal(
    test_plot2$mapping$y,
    quote(~.data[["DV"]]),
    ignore_attr = TRUE
  )
  expect_equal(
    vismo_xpdb %>%
      catdv_vs_dvprobs(cutpoint=2, quiet=TRUE) %>%
      {.$mapping$x},
    quote(~.data[["P1"]]),
    ignore_attr = TRUE
  )
  test_plot3 <- vismo_xpdb %>%
    catdv_vs_dvprobs(cutpoint=3,quiet=TRUE)
  expect_setequal(
    test_plot3$data$DV,
    c("GE(2)","LT(2)")
  )
})
