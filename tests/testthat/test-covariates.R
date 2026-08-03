test_that("grid plots appear as expected", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # test both xpdb_x xpdb_ex_pk produce the same plot
  wo_xpx <- xpdb_ex_pk %>% eta_grid(quiet = TRUE)
  w_xpx <- xpdb_ex_pk %>% as_xpdb_x() %>% eta_grid(quiet = TRUE)

  ## General tests
  xpdb_x <- set_option(xpdb_x, quiet=TRUE)
  eta1_grid <- eta_grid(xpdb_x, etavar = ETA1)
  eta2_grid <- eta_grid(xpdb_x, etavar = ETA2)
  eta12_grid <- eta_grid(xpdb_x, etavar = c(ETA1,ETA2))

  cont_p <- cov_grid(xpdb_x, covtypes = "cont")
  expect_setequal(
    names(cont_p$data),
    xp_var(xpdb_x, type="contcov")$col
  )
  cat_p <- cov_grid(xpdb_x, covtypes = "cat")
  expect_setequal(
    names(cat_p$data),
    xp_var(xpdb_x, type="catcov")$col
  )
  both_p <- cov_grid(xpdb_x)
  expect_setequal(
    names(both_p$data),
    xp_var(xpdb_x, type=c("contcov","catcov"))$col
  )
  cont_p <- cov_grid(xpdb_x, cols = c(SEX,CLCR))
  expect_setequal(
    names(cont_p$data),
    c("SEX","CLCR")
  )

  labl_x <- xpdb_x %>%
    xpose::set_var_labels(AGE="Age", MED1 = "Digoxin", .problem = 1) %>%
    xpose::set_var_units(AGE="yrs", .problem = 1) %>%
    set_var_levels(SEX=lvl_sex(), MED1 = lvl_bin(), .problem = 1)
  cov_labd <- cov_grid(labl_x)
  expect_true(
    any(
      grepl("Age",names(cov_labd$data))
    )
  )
  expect_true(
    any(
      grepl("yrs",names(cov_labd$data))
    )
  )
  expect_true(
    any(
      grepl("Digoxin",names(cov_labd$data))
    )
  )
  expect_true(
    any(
      grepl("(Yes|No)",cov_labd$data$Digoxin)
    )
  )
  expect_true(
    any(
      grepl("(Male|Female)",cov_labd$data$SEX )
    )
  )
  expect_true(
    any(
      grepl("N\\s*=\\s*\\d",cov_labd$data$SEX )
    )
  )
  expect_false(
    any(
      grepl("N\\w*=\\w*\\d",cov_grid(labl_x, show_n = FALSE)$data$SEX )
    )
  )

  etacont_p <- eta_vs_cov_grid(xpdb_x, covtypes = "cont")
  transform_eta <- function(x) stringr::str_replace(x, stringr::regex("^ET(A?)(\\d+)$"),
                                                    "ETA(\\2)")
  expect_setequal(
    names(etacont_p$data),
    xp_var(xpdb_x, type=c("contcov","eta"))$col %>%
      transform_eta()
  )
  etacont_p <- eta_vs_cov_grid(xpdb_x, covtypes = "cat")
  expect_setequal(
    names(etacont_p$data),
    xp_var(xpdb_x, type=c("catcov","eta"))$col %>%
      transform_eta()
  )
  etacont_p <- eta_vs_cov_grid(xpdb_x)
  expect_setequal(
    names(etacont_p$data),
    xp_var(xpdb_x, type=c("contcov","catcov","eta"))$col %>%
      transform_eta()
  )
  etacont_p <- eta_vs_cov_grid(xpdb_x, etavar = ETA1)
  expect_setequal(
    names(etacont_p$data),
    c(xp_var(xpdb_x, type=c("contcov","catcov"))$col, "ETA1") %>%
      transform_eta()
  )
  etacont_p <- eta_vs_cov_grid(xpdb_x, cols = CLCR, etavar = ETA1)
  expect_setequal(
    names(etacont_p$data),
    c("CLCR", "ETA1") %>%
      transform_eta()
  )
  expect_identical(
    names(etacont_p$data),
    c("CLCR", "ETA1") %>%
      transform_eta()
  )
  etacont_p <- eta_vs_cov_grid(xpdb_x, cols = CLCR, etavar = ETA1, etacov = FALSE)
  expect_identical(
    names(etacont_p$data),
    c("ETA1","CLCR") %>%
      transform_eta()
  )


  #### vdiffr tests to skip on CRAN
  skip_on_cran()
  skip_on_covr()
  skip()
  library(vdiffr)
  expect_doppelganger("from xpose_data", wo_xpx) # expect same as snapshot
  expect_doppelganger("from xp_xtra", w_xpx) # expect same as snapshot
  expect_doppelganger("from xpose_data", w_xpx) # expect same as xpose_data snapshot


  expect_doppelganger("eta1 plot", eta1_grid)
  expect_doppelganger("eta12 plot", eta12_grid)
  expect_failure(expect_doppelganger(
    "eta1 plot", eta2_grid
  ))
  expect_failure(expect_doppelganger(
    "eta1 plot", eta12_grid
  ))
  expect_failure(expect_doppelganger(
    "eta1 plot", eta_grid(xpdb_x)
  ))
  expect_failure(expect_doppelganger(
    "eta12 plot", eta_grid(xpdb_x, etavar = c(ETA1,ETA2),
                           pairs_opts = list(contcont_opts=list(stars=TRUE)))
  ))


})

test_that("individual eta-cov plots", {
  xpdb_x <- set_option(xpdb_x, quiet=TRUE)

  expect_s3_class(
    eta_vs_contcov(xpdb_x, etavar = ETA1),
    "xpose_plot"
  )
  expect_length(
    eta_vs_contcov(xpdb_x),
    nrow(xp_var(xpdb_x, type = "eta"))
  )

  expect_s3_class(
    eta_vs_catcov(xpdb_x, etavar = ETA1),
    "xpose_plot"
  )
  expect_length(
    eta_vs_catcov(xpdb_x),
    nrow(xp_var(xpdb_x, type = "eta"))
  )

  expect_true(
    any(
      grepl("N\\s*=\\s*\\d",eta_vs_catcov(xpdb_x, etavar=ETA1)$data$value )
    )
  )
  expect_false(
    any(
      grepl("N\\w*=\\w*\\d",eta_vs_catcov(xpdb_x, etavar=ETA1, show_n = FALSE)$data$value )
    )
  )
  expect_false(
    any(
      grepl("N\\w*=\\w*\\d",eta_vs_catcov(xpose::xpdb_ex_pk,
                                          etavar=ETA1, show_n = TRUE, quiet=TRUE)$data$value )
    )
  )

  expect_failure(expect_identical(
    eta_vs_catcov(xpdb_x, etavar=ETA1),
    eta_vs_catcov(xpdb_x, etavar=ETA1, orientation = "y")
  ))

})

test_that("eta_vs_contcov/eta_vs_catcov list=FALSE combines onto one shared, faceted plot", {
  xpdb_x <- set_option(xpdb_x, quiet = TRUE)
  n_eta <- nrow(xp_var(xpdb_x, type = "eta"))

  combined_cont <- eta_vs_contcov(xpdb_x, list = FALSE)
  expect_s3_class(combined_cont, "xpose_plot")
  expect_s3_class(combined_cont$facet, "FacetWrapPaginate")
  expect_length(unique(combined_cont$data$eta_name), n_eta)
  expect_setequal(unique(combined_cont$data$eta_name), c("ETA(1)", "ETA(2)", "ETA(3)"))

  combined_cat <- eta_vs_catcov(xpdb_x, list = FALSE)
  expect_s3_class(combined_cat, "xpose_plot")
  expect_s3_class(combined_cat$facet, "FacetWrapPaginate")
  expect_length(unique(combined_cat$data$eta_name), n_eta)

  # N= counts (per covariate level) stay correct regardless of how many
  # etas end up sharing the plot -- see the ordering note in eta_vs_catcov()
  single_n <- eta_vs_catcov(xpdb_x, etavar = ETA1, covvar = SEX)$data$value %>%
    as.character() %>%
    table()
  combined_n <- eta_vs_catcov(xpdb_x, covvar = SEX, list = FALSE)$data %>%
    dplyr::filter(eta_name == "ETA(1)") %>%
    dplyr::pull(value) %>%
    as.character() %>%
    table()
  expect_equal(unclass(single_n), unclass(combined_n))

  # A single eta short-circuits the combine path regardless of `list` --
  # compared on $data/class rather than the whole object, since aes()'s
  # captured `.Environment` differs (a fresh call frame) between any two
  # separate calls regardless of `list`, same as elsewhere in this file
  # (see the orientation `expect_failure(expect_identical(...))` above).
  p_single_combine <- eta_vs_contcov(xpdb_x, etavar = ETA1, list = FALSE)
  p_single_list <- eta_vs_contcov(xpdb_x, etavar = ETA1, list = TRUE)
  expect_identical(class(p_single_combine), class(p_single_list))
  expect_equal(p_single_combine$data, p_single_list$data)
})

test_that("eta_vs_contcov/eta_vs_catcov covvar restricts to selected covariates", {
  xpdb_x <- set_option(xpdb_x, quiet = TRUE)

  p_cont <- eta_vs_contcov(xpdb_x, etavar = ETA1, covvar = AGE)
  expect_setequal(levels(p_cont$data$variable), "AGE")

  p_cat <- eta_vs_catcov(xpdb_x, etavar = ETA1, covvar = SEX)
  expect_setequal(levels(p_cat$data$variable), "SEX")

  expect_error(
    eta_vs_contcov(xpdb_x, etavar = ETA1, covvar = SEX, quiet = TRUE),
    "should only include continuous covariate.*SEX"
  )
  expect_error(
    eta_vs_catcov(xpdb_x, etavar = ETA1, covvar = AGE, quiet = TRUE),
    "should only include categorical covariate.*AGE"
  )
})

test_that("errors and special plot circumstances are correctly caught", {
  expect_error(
    vismo_pomod %>% eta_grid(etavar = P1, quiet = TRUE),
    "should only include eta.*P1"
  )
  expect_error(
    vismo_pomod %>%
      set_var_types(catcov=COHORT,contcov=AGE) %>%
      eta_vs_cov_grid(etavar = P1, quiet = TRUE, drop_fixed = FALSE),
    "should only include eta.*P1"
  )
  expect_error(
    vismo_pomod %>%
      set_var_types(catcov=COHORT,contcov=AGE) %>%
      eta_vs_contcov(etavar = P1, quiet = TRUE, drop_fixed = FALSE),
    "should only include eta.*P1"
  )
  expect_error(
    vismo_pomod %>%
      set_var_types(catcov=COHORT,contcov=AGE) %>%
      eta_vs_catcov(etavar = P1, quiet = TRUE, drop_fixed = FALSE),
    "should only include eta.*P1"
  )
  expect_error(
    pheno_base %>% cov_grid(covtypes = "bbb", quiet=TRUE),
    "Invalid.*bbb"
  )
  expect_error(
    pheno_base %>% eta_vs_cov_grid(covtypes = "bbb", quiet=TRUE),
    "Invalid.*bbb"
  )
  expect_error(
    pheno_base %>% cov_grid(cols=WT,covtypes = "cat", quiet=TRUE),
    "should only include.*cat.*WT"
  )
  expect_error(
    pheno_base %>% cov_grid(cols=APGR,covtypes = "cont", quiet=TRUE),
    "should only include.*cont.*APGR"
  )
  expect_error(
    pheno_base %>% eta_vs_cov_grid(cols=WT,covtypes = "cat", quiet=TRUE),
    "should only include.*cat.*WT"
  )
  suppressMessages(expect_message(
    xpose::xpdb_ex_pk %>% cov_grid(),
    "Cannot show N"
  ))
  suppressMessages(expect_message(
    xpose::xpdb_ex_pk %>% eta_vs_cov_grid(),
    "Cannot show N"
  ))
  suppressMessages(expect_no_message(
    xpose::xpdb_ex_pk %>% cov_grid(show_n = FALSE),
    message="Cannot show N"
  ))



})

test_that("no cov and no eta cases", {

  ## Any issues with no or few covariates?
  xpdb_x_nocov <- xpdb_x %>%
    set_var_types(1, na = all_of(c(
      xp_var(xpdb_x, 1, type = "contcov")$col,
      xp_var(xpdb_x, 1, type = "catcov")$col
    )))
  expect_error(
    xpdb_x_nocov %>%
      eta_vs_cov_grid(),
    "No.*cont.*cat.*covariate column found"
  )
  expect_error(
    xpdb_x_nocov %>%
      set_var_types(catcov=SEX) %>%
      eta_vs_cov_grid(covtypes ="cont"),
    "No.*cont.*covariate column found"
  )
  expect_error(
    xpdb_x_nocov %>%
      set_var_types(contcov=AGE) %>%
      eta_vs_cov_grid(covtypes ="cat"),
    "No.*cat.*covariate column found"
  )
  suppressMessages(expect_no_error(
    xpdb_x_nocov %>%
      set_var_types(catcov=SEX) %>%
      eta_vs_cov_grid(covtypes = "cat")
  ))
  suppressMessages(expect_no_error(
    xpdb_x_nocov %>%
      set_var_types(catcov=SEX) %>%
      eta_vs_cov_grid()
  ))
  suppressMessages(expect_no_error(
    xpdb_x_nocov %>%
      set_var_types(contcov=AGE) %>%
      eta_vs_cov_grid(covtypes = "cont")
  ))
  suppressMessages(expect_no_error(
    xpdb_x_nocov %>%
      set_var_types(contcov=AGE) %>%
      eta_vs_cov_grid()
  ))



  ## Any issues with no or few etas?
  xpdb_x_noeta <- xpdb_x %>%
    set_var_types(1, na = all_of(c(
      xp_var(xpdb_x, 1, type = "eta")$col
    )))
  expect_error(
    xpdb_x_noeta %>%
      eta_vs_cov_grid(),
    "No eta col"
  )
})

test_that("pairs_opts named entries are forwarded to xplot_pairs (grid plots)", {
  xpdb_x <- set_option(xpdb_x, quiet = TRUE)

  # Un-skipped (unlike the vdiffr comparisons above) exercise of the
  # pairs_opts_ purrr::map() branch that actually forwards a user-supplied
  # `_opts` entry, for each of the three grid plot wrappers
  expect_no_error(
    eta_grid(xpdb_x, etavar = c(ETA1, ETA2),
             pairs_opts = list(contcont_opts = list(stars = TRUE)), quiet = TRUE)
  )
  expect_no_error(
    cov_grid(xpdb_x, covtypes = "cont",
             pairs_opts = list(contcont_opts = list(stars = TRUE)), quiet = TRUE)
  )
  expect_no_error(
    eta_vs_cov_grid(xpdb_x, covtypes = "cont",
                     pairs_opts = list(catcont_opts = list()), quiet = TRUE)
  )
})

test_that("eta_vs_contcov linsm=TRUE uses an lm smooth method", {
  xpdb_x <- set_option(xpdb_x, quiet = TRUE)

  p_lm <- eta_vs_contcov(xpdb_x, etavar = ETA1, linsm = TRUE, quiet = TRUE)
  smooth_layer <- p_lm$layers[[which(purrr::map_chr(p_lm$layers, ~class(.x$geom)[1]) == "GeomSmooth")]]
  expect_equal(smooth_layer$stat_params$method, "lm")

  p_theme <- eta_vs_contcov(xpdb_x, etavar = ETA1, linsm = FALSE, quiet = TRUE)
  smooth_layer_theme <- p_theme$layers[[which(purrr::map_chr(p_theme$layers, ~class(.x$geom)[1]) == "GeomSmooth")]]
  expect_false(identical(smooth_layer_theme$stat_params$method, "lm"))
})

test_that("eta_vs_catcov informs when show_n is requested on a non-xp_xtras object", {
  # Gathering the unlabeled eta column together with the labeled/leveled
  # catcov column (tidyr::gather() inside xpose::fetch_data()) cannot
  # reconcile their differing attributes, so it drops them and warns; this
  # is expected upstream xpose behavior for this deliberately-degraded
  # (non-xp_xtras + show_n) code path, not a regression, so we assert it
  # explicitly here rather than letting it pass through unchecked.
  expect_warning(
    expect_message(
      eta_vs_catcov(xpose::xpdb_ex_pk, etavar = ETA1, show_n = TRUE, quiet = FALSE),
      "Cannot show N"
    ),
    "attributes are not identical"
  )
})

test_that("cov_forest", {
  x <- xpdb_x %>%
    add_cov_association(
      TVCL ~ power(CLCR, THETA7, ref = 64),
      TVCL ~ catshift(SEX, THETA4, ref = 1)
    )

  p <- cov_forest(x, quiet = TRUE)
  expect_s3_class(p, "xpose_plot")
  expect_equal(nrow(p$data), 5)
  expect_true(all(c("effect", "ci_low", "ci_high", "row_label", "param") %in% names(p$data)))

  # Faceted by param
  expect_s3_class(p$facet, "FacetWrap")
  expect_true("param" %in% names(p$facet$params$facets))

  # No associations declared -> informative error, not a downstream crash
  expect_error(
    xpdb_x %>% cov_forest(quiet = TRUE),
    "add_cov_association"
  )

  # Dots are forwarded to prm_cov() for selector filtering
  p_filtered <- cov_forest(x, TVCL ~ CLCR, quiet = TRUE)
  expect_equal(nrow(p_filtered$data), 3)
  expect_true(all(p_filtered$data$covariate == "CLCR"))

  # forest_opts flows through to xplot_forest()
  p_point_only <- cov_forest(x, forest_opts = list(type = "p"), quiet = TRUE)
  expect_setequal(
    purrr::map_chr(p_point_only$layers, ~class(.x$geom)[1]),
    "GeomPoint"
  )
})

test_that("cov_forest violin layer (type includes 'v')", {
  x <- xpdb_x %>%
    add_cov_association(
      TVCL ~ power(CLCR, THETA7, ref = 64),
      TVCL ~ catshift(SEX, THETA4, ref = 1)
    )

  p <- cov_forest(x, type = "pilv", nsim = 200, quiet = TRUE)
  geoms <- purrr::map_chr(p$layers, ~class(.x$geom)[1])
  expect_setequal(geoms, c("GeomVline", "GeomLinerange", "GeomPoint", "GeomViolin"))

  violin_layer <- p$layers[[which(geoms == "GeomViolin")]]
  expect_false(violin_layer$inherit.aes)
  # one row per draw per category (5 categories x 200 draws)
  expect_equal(nrow(violin_layer$data), 5 * 200)
  # reference-level rows are degenerate (all draws == 1), by construction
  expect_true(all(violin_layer$data$draws[violin_layer$data$row_label == "SEX: 1"] == 1))

  # violin requires simulation draws; delta + "v" errors clearly
  expect_error(
    cov_forest(x, type = "pilv", ci_method = "delta", quiet = TRUE),
    "simulation"
  )
})

test_that("cov_forest show_ref, region, and log", {
  x <- xpdb_x %>%
    add_cov_association(
      TVCL ~ power(CLCR, THETA7, ref = 64),
      TVCL ~ catshift(SEX, THETA4, ref = 1)
    )

  # default includes the shaded reference region (type='pilr')
  p <- cov_forest(x, quiet = TRUE)
  geoms <- purrr::map_chr(p$layers, ~class(.x$geom)[1])
  expect_true("GeomRect" %in% geoms)
  rect_layer <- p$layers[[which(geoms == "GeomRect")]]
  expect_equal(rect_layer$data$xmin, 0.8)
  expect_equal(rect_layer$data$xmax, 1.25)

  # custom region flows through
  p_region <- cov_forest(x, region = c(0.7, 1.43), quiet = TRUE)
  rect_layer2 <- p_region$layers[[which(purrr::map_chr(p_region$layers, ~class(.x$geom)[1]) == "GeomRect")]]
  expect_equal(rect_layer2$data$xmin, 0.7)
  expect_equal(rect_layer2$data$xmax, 1.43)

  # show_ref = FALSE drops reference rows
  expect_equal(nrow(p$data), 5)
  p_noref <- cov_forest(x, show_ref = FALSE, quiet = TRUE)
  expect_equal(nrow(p_noref$data), 3)
  expect_false(any(p_noref$data$is_ref))

  # log is a plain boolean now (not the "x"/NULL axis-selector convention);
  # verify via the built panel range, since a narrow x range makes log vs
  # linear labels look identical (both round to the same displayed values)
  p_log <- cov_forest(x, log = TRUE, quiet = TRUE)
  p_linear <- cov_forest(x, log = FALSE, quiet = TRUE)
  b_log <- ggplot2::ggplot_build(p_log)
  b_linear <- ggplot2::ggplot_build(p_linear)
  expect_false(isTRUE(all.equal(
    b_log$layout$panel_scales_x[[1]]$range$range,
    b_linear$layout$panel_scales_x[[1]]$range$range
  )))
})

test_that("cov_forest defaults quiet from xpdb options when omitted", {
  x <- xpdb_x %>%
    set_option(quiet = TRUE) %>%
    add_cov_association(
      TVCL ~ power(CLCR, THETA7, ref = 64),
      TVCL ~ catshift(SEX, THETA4, ref = 1)
    )
  expect_no_error(cov_forest(x))
})

test_that("cov_forest errors informatively when show_ref = FALSE removes every row", {
  # A categorical covariate whose only observed level is the reference
  # level: every prm_cov() row is a reference row, so show_ref = FALSE
  # leaves nothing to plot
  x <- xpdb_x %>%
    xpose::mutate(FLAG = 1, .problem = 1) %>%
    set_var_types(.problem = 1, catcov = FLAG) %>%
    add_cov_association(
      TVCL ~ custom(FLAG, THETA4, ref = 1, fun = function(cov, ref, theta) 1)
    )

  expect_equal(nrow(prm_cov(x, quiet = TRUE)), 1)
  expect_true(prm_cov(x, quiet = TRUE)$is_ref)

  expect_error(
    cov_forest(x, show_ref = FALSE, quiet = TRUE),
    "No rows left to plot after.*show_ref = FALSE"
  )
})

test_that("shk grid plots appear as expected", {
  xpdb_shk <- xpdb_x %>% set_option(quiet = TRUE) %>% backfill_shk(quiet = TRUE)
  shk_cols <- xp_var(xpdb_shk, type = "shk")$col

  shk1_grid <- shk_grid(xpdb_shk, shkvar = ETA1_SHK)
  expect_setequal(names(shk1_grid$data), "ETA1_SHK")
  shk12_grid <- shk_grid(xpdb_shk, shkvar = c(ETA1_SHK, ETA2_SHK))
  expect_setequal(names(shk12_grid$data), c("ETA1_SHK", "ETA2_SHK"))

  cont_p <- shk_vs_cov_grid(xpdb_shk, covtypes = "cont")
  expect_setequal(
    names(cont_p$data),
    c(shk_cols, xp_var(xpdb_shk, type = "contcov")$col)
  )
  cat_p <- shk_vs_cov_grid(xpdb_shk, covtypes = "cat")
  expect_setequal(
    names(cat_p$data),
    c(shk_cols, xp_var(xpdb_shk, type = "catcov")$col)
  )

  onecol_p <- shk_vs_cov_grid(xpdb_shk, cols = CLCR, shkvar = ETA1_SHK)
  expect_identical(names(onecol_p$data), c("CLCR", "ETA1_SHK"))
  onecol_p_rev <- shk_vs_cov_grid(xpdb_shk, cols = CLCR, shkvar = ETA1_SHK, shkcov = FALSE)
  expect_identical(names(onecol_p_rev$data), c("ETA1_SHK", "CLCR"))
})

test_that("individual shk-cov plots", {
  xpdb_shk <- xpdb_x %>% set_option(quiet = TRUE) %>% backfill_shk(quiet = TRUE)
  shk_cols <- xp_var(xpdb_shk, type = "shk")$col

  expect_s3_class(
    shk_vs_contcov(xpdb_shk, shkvar = ETA1_SHK),
    "xpose_plot"
  )
  expect_length(shk_vs_contcov(xpdb_shk), length(shk_cols))

  expect_s3_class(
    shk_vs_catcov(xpdb_shk, shkvar = ETA1_SHK),
    "xpose_plot"
  )
  expect_length(shk_vs_catcov(xpdb_shk), length(shk_cols))

  expect_true(
    any(grepl("N\\s*=\\s*\\d", shk_vs_catcov(xpdb_shk, shkvar = ETA1_SHK)$data$value))
  )
  expect_false(
    any(grepl("N\\w*=\\w*\\d", shk_vs_catcov(xpdb_shk, shkvar = ETA1_SHK, show_n = FALSE)$data$value))
  )

  expect_failure(expect_identical(
    shk_vs_catcov(xpdb_shk, shkvar = ETA1_SHK),
    shk_vs_catcov(xpdb_shk, shkvar = ETA1_SHK, orientation = "y")
  ))
})

test_that("shk_vs_contcov/shk_vs_catcov list=FALSE combines onto one shared, faceted plot", {
  xpdb_shk <- xpdb_x %>% set_option(quiet = TRUE) %>% backfill_shk(quiet = TRUE)
  n_shk <- nrow(xp_var(xpdb_shk, type = "shk"))

  combined_cont <- shk_vs_contcov(xpdb_shk, list = FALSE)
  expect_s3_class(combined_cont, "xpose_plot")
  expect_s3_class(combined_cont$facet, "FacetWrapPaginate")
  expect_length(unique(combined_cont$data$shk_name), n_shk)
  expect_setequal(unique(combined_cont$data$shk_name), c("ETA1_SHK", "ETA2_SHK", "ETA3_SHK"))

  combined_cat <- shk_vs_catcov(xpdb_shk, list = FALSE)
  expect_s3_class(combined_cat, "xpose_plot")
  expect_s3_class(combined_cat$facet, "FacetWrapPaginate")
  expect_length(unique(combined_cat$data$shk_name), n_shk)

  # N= counts (per covariate level) stay correct regardless of how many
  # shk columns end up sharing the plot
  single_n <- shk_vs_catcov(xpdb_shk, shkvar = ETA1_SHK, covvar = SEX)$data$value %>%
    as.character() %>%
    table()
  combined_n <- shk_vs_catcov(xpdb_shk, covvar = SEX, list = FALSE)$data %>%
    dplyr::filter(shk_name == "ETA1_SHK") %>%
    dplyr::pull(value) %>%
    as.character() %>%
    table()
  expect_equal(unclass(single_n), unclass(combined_n))

  # A single shk column short-circuits the combine path regardless of
  # `list` -- compared on $data/class rather than the whole object, since
  # aes()'s captured `.Environment` differs (a fresh call frame) between
  # any two separate calls regardless of `list` (see the analogous eta
  # test above).
  p_single_combine <- shk_vs_contcov(xpdb_shk, shkvar = ETA1_SHK, list = FALSE)
  p_single_list <- shk_vs_contcov(xpdb_shk, shkvar = ETA1_SHK, list = TRUE)
  expect_identical(class(p_single_combine), class(p_single_list))
  expect_equal(p_single_combine$data, p_single_list$data)
})

test_that("shk_vs_contcov/shk_vs_catcov covvar restricts to selected covariates", {
  xpdb_shk <- xpdb_x %>% set_option(quiet = TRUE) %>% backfill_shk(quiet = TRUE)

  p_cont <- shk_vs_contcov(xpdb_shk, shkvar = ETA1_SHK, covvar = AGE)
  expect_setequal(levels(p_cont$data$variable), "AGE")

  p_cat <- shk_vs_catcov(xpdb_shk, shkvar = ETA1_SHK, covvar = SEX)
  expect_setequal(levels(p_cat$data$variable), "SEX")

  expect_error(
    shk_vs_contcov(xpdb_shk, shkvar = ETA1_SHK, covvar = SEX, quiet = TRUE),
    "should only include continuous covariate.*SEX"
  )
  expect_error(
    shk_vs_catcov(xpdb_shk, shkvar = ETA1_SHK, covvar = AGE, quiet = TRUE),
    "should only include categorical covariate.*AGE"
  )
})

test_that("covvar is an alias for cols on eta_vs_cov_grid/shk_vs_cov_grid", {
  xpdb_x <- set_option(xpdb_x, quiet = TRUE)
  xpdb_shk <- xpdb_x %>% backfill_shk(quiet = TRUE)

  expect_identical(
    eta_vs_cov_grid(xpdb_x, cols = c(AGE, SEX), etavar = ETA1)$data,
    eta_vs_cov_grid(xpdb_x, covvar = c(AGE, SEX), etavar = ETA1)$data
  )
  # covvar takes precedence when both are supplied
  expect_identical(
    eta_vs_cov_grid(xpdb_x, cols = SEX, covvar = AGE, etavar = ETA1)$data,
    eta_vs_cov_grid(xpdb_x, covvar = AGE, etavar = ETA1)$data
  )

  expect_identical(
    shk_vs_cov_grid(xpdb_shk, cols = c(AGE, SEX), shkvar = ETA1_SHK)$data,
    shk_vs_cov_grid(xpdb_shk, covvar = c(AGE, SEX), shkvar = ETA1_SHK)$data
  )
})

test_that("shk plot errors are correctly caught", {
  # No shk columns backfilled yet
  expect_error(
    shk_grid(xpdb_x, quiet = TRUE),
    "No shrinkage contribution column"
  )
  expect_error(
    shk_vs_cov_grid(xpdb_x, quiet = TRUE),
    "No shrinkage contribution column"
  )
  expect_error(
    shk_vs_contcov(xpdb_x, quiet = TRUE),
    "No shrinkage contribution column"
  )
  expect_error(
    shk_vs_catcov(xpdb_x, quiet = TRUE),
    "No shrinkage contribution column"
  )

  xpdb_shk <- xpdb_x %>% set_option(quiet = TRUE) %>% backfill_shk(quiet = TRUE)

  expect_error(
    shk_grid(xpdb_shk, shkvar = ID, quiet = TRUE),
    "should only include shrinkage contribution.*ID"
  )
  expect_error(
    shk_vs_cov_grid(xpdb_shk, shkvar = ID, quiet = TRUE),
    "should only include shrinkage contribution.*ID"
  )
  expect_error(
    shk_vs_contcov(xpdb_shk, shkvar = ID, quiet = TRUE),
    "should only include shrinkage contribution.*ID"
  )
  expect_error(
    shk_vs_catcov(xpdb_shk, shkvar = ID, quiet = TRUE),
    "should only include shrinkage contribution.*ID"
  )

  expect_error(
    shk_vs_cov_grid(xpdb_shk, covtypes = "bbb", quiet = TRUE),
    "Invalid.*bbb"
  )
  expect_error(
    shk_vs_cov_grid(xpdb_shk, cols = WT, covtypes = "cat", quiet = TRUE),
    "should only include.*cat.*WT"
  )
})

test_that("pairs_opts named entries are forwarded to xplot_pairs (shk grid plots)", {
  xpdb_shk <- xpdb_x %>% set_option(quiet = TRUE) %>% backfill_shk(quiet = TRUE)

  expect_no_error(
    shk_grid(xpdb_shk, shkvar = c(ETA1_SHK, ETA2_SHK),
             pairs_opts = list(contcont_opts = list(stars = TRUE)), quiet = TRUE)
  )
  expect_no_error(
    shk_vs_cov_grid(xpdb_shk, covtypes = "cont",
                    pairs_opts = list(catcont_opts = list()), quiet = TRUE)
  )
})

test_that("shk_vs_contcov linsm=TRUE uses an lm smooth method", {
  xpdb_shk <- xpdb_x %>% set_option(quiet = TRUE) %>% backfill_shk(quiet = TRUE)

  p_lm <- shk_vs_contcov(xpdb_shk, shkvar = ETA1_SHK, linsm = TRUE, quiet = TRUE)
  smooth_layer <- p_lm$layers[[which(purrr::map_chr(p_lm$layers, ~class(.x$geom)[1]) == "GeomSmooth")]]
  expect_equal(smooth_layer$stat_params$method, "lm")

  p_theme <- shk_vs_contcov(xpdb_shk, shkvar = ETA1_SHK, linsm = FALSE, quiet = TRUE)
  smooth_layer_theme <- p_theme$layers[[which(purrr::map_chr(p_theme$layers, ~class(.x$geom)[1]) == "GeomSmooth")]]
  expect_false(identical(smooth_layer_theme$stat_params$method, "lm"))
})
