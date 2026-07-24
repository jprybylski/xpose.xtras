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

test_that(".problem can be omitted from set_dv_probs/list_dv_probs (#63)", {
  xpx_w_types <- pkpd_m3 %>%
    set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE)

  # .problem omitted, first dot is a formula: used to be swallowed by
  # .problem positionally and crash instead of being treated as a dot.
  expect_identical(
    xpx_w_types %>%
      set_dv_probs(1~LIKE, .dv_var = BLQ),
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ)
  )

  # .problem and .dv_var both omitted
  expect_identical(
    xpx_w_types %>%
      set_dv_probs(1~LIKE),
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ)
  )

  # .problem still works when named after the formula dots
  expect_identical(
    xpx_w_types %>%
      set_dv_probs(1~LIKE, .dv_var = BLQ, .problem = 1),
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ)
  )

  # positional .problem (legacy calling style) still works
  expect_identical(
    xpx_w_types %>%
      set_dv_probs(1, 1~LIKE, .dv_var = BLQ),
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ)
  )

  # an invalid problem number is still caught, whether .problem is
  # positional, named before the dots, or named after them
  suppressMessages(expect_error(
    xpx_w_types %>% set_dv_probs(1~LIKE, .dv_var = BLQ, .problem = 99),
    "99.*not valid"
  ))

  # list_dv_probs() shares the same .dv_var-inference codepath
  expect_identical(
    xpx_w_types %>%
      set_dv_probs(1~LIKE) %>%
      list_dv_probs(),
    xpx_w_types %>%
      set_dv_probs(.problem=1, 1~LIKE, .dv_var = BLQ) %>%
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

test_that("catdv can be plot as a binned calibration against dvprobs (catdv_vs_ipred)", {
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])

  m3_test_dummy <- pkpd_m3 %>%
    set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
    set_dv_probs(.problem=1, 1~LIKE)

  expect_warning(
    m3_test_dummy %>%
      set_var_types(catdv=DOSE, quiet = TRUE) %>%
      catdv_vs_ipred(quiet=TRUE),
    "Only one.*cat.*DV.*used.*BLQ"
  )

  expect_error(
    pkpd_m3 %>%
      set_var_types(.problem=1, catdv=BLQ, dvprobs=LIKE) %>%
      catdv_vs_ipred(quiet=TRUE),
    "Relationship between probabiliy column and at least one categorical DV level should be defined"
  )

  expect_error(
    m3_test_dummy %>%
      catdv_vs_ipred(cutpoint = 99, quiet=TRUE),
    "cutpoint.*is.*row number.*1.*99.*range"
  )

  expect_error(
    m3_test_dummy %>%
      catdv_vs_ipred(bins = 0, quiet=TRUE),
    "bins"
  )
  expect_error(
    m3_test_dummy %>%
      catdv_vs_ipred(bins = -1, quiet=TRUE),
    "bins"
  )
  expect_error(
    m3_test_dummy %>%
      catdv_vs_ipred(bins = 2.5, quiet=TRUE),
    "bins"
  )

  test_plot <- m3_test_dummy %>%
    catdv_vs_ipred(bins = 5, quiet=TRUE)

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
  expect_equal(
    test_plot$labels$x,
    "Probability BLQ EQ(1)"
  )
  expect_equal(
    test_plot$labels$y,
    "Observed frequency BLQ EQ(1)"
  )
  expect_equal(
    m3_test_dummy %>%
      catdv_vs_ipred(quiet=TRUE, xlab = "basic") %>%
      {.$labels$x},
    "LIKE"
  )

  # Exactly `bins` bins are produced (plenty of data to fill them all)
  expect_equal(nrow(test_plot$data), 5)
  # Every observation used is accounted for across bins
  expect_equal(sum(test_plot$data$n), nrow(xpose::only_obs(m3_test_dummy,1,TRUE)(xpose::get_data(m3_test_dummy,.problem=1,quiet=TRUE))))
  # Observed proportions and mean probabilities are valid probabilities
  expect_true(all(test_plot$data$LIKE >= 0 & test_plot$data$LIKE <= 1))
  expect_true(all(test_plot$data$BLQ >= 0 & test_plot$data$BLQ <= 1))
  # Bins are ordered from lowest to highest predicted probability
  expect_equal(test_plot$data$LIKE, sort(test_plot$data$LIKE))
  # A well-specified likelihood model should track closely with unity
  expect_equal(test_plot$data$LIKE, test_plot$data$BLQ, tolerance = 0.1)

  # Default type includes connecting line and points; guide is unity line
  expect_true("GeomLine" %in% geoms_lists(test_plot))
  expect_true("GeomPoint" %in% geoms_lists(test_plot))
  expect_true("GeomAbline" %in% geoms_lists(test_plot))
  expect_false(
    "GeomAbline" %in% geoms_lists(
      m3_test_dummy %>% catdv_vs_ipred(bins = 5, guide = FALSE, quiet=TRUE)
    )
  )
  expect_false(
    "GeomLine" %in% geoms_lists(
      m3_test_dummy %>% catdv_vs_ipred(bins = 5, type = "p", quiet=TRUE)
    )
  )

  # Binning can be stratified by a character facet
  facet_plot <- m3_test_dummy %>%
    catdv_vs_ipred(bins = 3, facets = "DOSE", quiet=TRUE)
  expect_equal(nrow(facet_plot$data), 3*length(unique(xpose::get_data(m3_test_dummy,.problem=1,quiet=TRUE)$DOSE)))
  expect_true("DOSE" %in% names(facet_plot$data))

  vismo_xpdb <- vismo_pomod  %>%
    set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
    set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)
  test_plot2 <- vismo_xpdb %>%
    catdv_vs_ipred(bins = 4, quiet=TRUE)
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
      catdv_vs_ipred(cutpoint=2, bins = 4, quiet=TRUE) %>%
      {.$mapping$x},
    quote(~.data[["P1"]]),
    ignore_attr = TRUE
  )
  test_plot3 <- vismo_xpdb %>%
    catdv_vs_ipred(cutpoint=3, bins = 4, quiet=TRUE)
  expect_equal(
    test_plot3$labels$x,
    "Probability DV GE(2)"
  )
})

test_that("catdv can be plotted longitudinally by occasion (catdv_vs_occ)", {
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])

  vismo_base <- vismo_pomod %>%
    set_var_types(.problem=1, catdv=DV, dvprobs=c(P0,P1,P23)) %>%
    set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)

  vismo_occ <- vismo_base %>%
    xpose::mutate(OCC = ceiling((TIME+1)/24), .problem=1) %>%
    set_var_types(.problem=1, occ=OCC) %>%
    set_var_levels(.problem=1, OCC = lvl_inord(paste0("Day", 1:12)))

  # No occ-typed column and no explicit `bin`
  expect_error(
    vismo_base %>% catdv_vs_occ(quiet=TRUE),
    "occ.*column found"
  )

  # More than one occ-typed column, none specified explicitly
  vismo_2occ <- vismo_occ %>%
    xpose::mutate(OCC2 = OCC, .problem=1) %>%
    set_var_types(.problem=1, occ=OCC2)
  expect_warning(
    vismo_2occ %>% catdv_vs_occ(quiet=TRUE),
    "Only one occasion.*used"
  )

  # Multiple catdv columns, none specified explicitly
  expect_warning(
    vismo_occ %>%
      set_var_types(catdv=ID, quiet=TRUE) %>%
      catdv_vs_occ(quiet=TRUE),
    "Only one.*cat.*DV.*used.*DV"
  )

  test_plot <- vismo_occ %>%
    catdv_vs_occ(quiet=TRUE)

  # x is the (ordered) occasion column, one row per bin per series
  expect_equal(test_plot$labels$x, "OCC")
  expect_true(is.ordered(test_plot$data$OCC))
  expect_setequal(test_plot$data$variable, c("Observed","Predicted"))
  expect_true(all(test_plot$data$value >= 0 & test_plot$data$value <= 1))
  expect_equal(nrow(test_plot$data) %% 2, 0)

  # y label reflects the cutpoint
  expect_equal(test_plot$labels$y, "Frequency/probability DV EQ(0)")
  test_plot_cp3 <- vismo_occ %>%
    catdv_vs_occ(cutpoint = 3, quiet=TRUE)
  expect_equal(test_plot_cp3$labels$y, "Frequency/probability DV GE(2)")

  expect_error(
    vismo_occ %>% catdv_vs_occ(cutpoint = 99, quiet=TRUE),
    "cutpoint.*is.*row number.*99.*range"
  )

  # Default type includes connecting line and points; "p" drops the line
  expect_true("GeomLine" %in% geoms_lists(test_plot))
  expect_true("GeomPoint" %in% geoms_lists(test_plot))
  expect_false(
    "GeomLine" %in% geoms_lists(
      vismo_occ %>% catdv_vs_occ(type = "p", quiet=TRUE)
    )
  )

  # `bin` can override the default occ column; an unleveled column falls
  # back to a plain (unordered) factor, with an informative message
  expect_message(
    unleveled_plot <- vismo_occ %>% catdv_vs_occ(bin = COHORT, quiet=FALSE),
    "no defined levels"
  )
  expect_false(is.ordered(unleveled_plot$data$COHORT))
  expect_equal(unleveled_plot$labels$x, "COHORT")

  # Binning can be stratified by a character facet
  facet_plot <- vismo_occ %>%
    catdv_vs_occ(facets = "COHORT", quiet=TRUE)
  expect_true("COHORT" %in% names(facet_plot$data))
  expect_equal(nrow(facet_plot$data) %% 2, 0)
})
