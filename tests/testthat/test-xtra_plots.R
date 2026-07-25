# Most functions are tested indirectly
test_that("xp_themes are consistent with expectations", {

  expect_true(
    xpose::is.xpose.theme(xp_xtra_theme())
  )
  expect_true(
    xpose::is.xpose.theme(xp4_xtra_theme())
  )

  expect_in(
    "sharkdn_size",
    names(xp_xtra_theme())
  )

  expect_equal(
    xpose::theme_xp_default()$histogram_fill,
    xp_xtra_theme()$boxplot_fill
  )

  # Updated theme values are preferred if based_on is set
  new_theme <- xpose::theme_xp_default()
  new_theme$boxplot_fill <- "red"
  new_xtra_theme <- xp_xtra_theme(new_theme)
  expect_equal(
    new_theme$boxplot_fill,
    new_xtra_theme$boxplot_fill
  )
  expect_failure(expect_equal(
    new_theme$boxplot_fill,
    xp_xtra_theme()$boxplot_fill
  ))

  # xpose4 theme
  expect_equal(
    xp4_xtra_theme()$histogram_fill,
    xpose::theme_xp_xpose4()$histogram_fill
  )

})

test_that("xpose_plot(s) can be grabbed", {

  test_title <- "@y vs. @x | @run"
  test_plot <- xpose::dv_vs_idv(pheno_base, quiet=TRUE,
                          title = test_title)

  expect_equal(
    test_plot$labels$title,
    test_title
  )

  test_grabbed <- suppressMessages(grab_xpose_plot(test_plot))
  grabbed_title <- sprintf("DV vs. TIME | %s", get_prop(pheno_base, "run"))
  expect_equal(
    test_grabbed$labels$title,
    grabbed_title
  )

  expect_error(
    grab_xpose_plot(xpose::ind_plots(pkpd_m3, quiet = TRUE)),
    "Use built-in xpose pagination"
  )

  # list-of-plots recurses over each element
  grabbed_list <- suppressMessages(grab_xpose_plot(list(test_plot, test_plot)))
  expect_type(grabbed_list, "list")
  expect_length(grabbed_list, 2)
  expect_equal(grabbed_list[[1]]$labels$title, grabbed_title)
  expect_equal(grabbed_list[[2]]$labels$title, grabbed_title)

})

test_that("apply_lul_wide auto-detects columns when `cols` is not supplied", {
  # apply_lul_wide() is designed around one-row-per-subject wide covariate
  # tables (see covariates.R callers); build a small one from xpdb_x so the
  # `cols = NULL` auto-detection path (mirroring the underlying xpdb's data
  # column names) has consistent types to pivot.
  xpdb_small <- xpdb_x
  full_data <- xpose::get_data(xpdb_x, .problem = 1, quiet = TRUE)
  baseline <- full_data %>%
    dplyr::group_by(ID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(ID, SEX, MED1, MED2, DOSE, WT, AGE, CLCR)
  idx <- which(xpdb_small$data$problem == 1)
  xpdb_small$data$data[[idx]] <- baseline

  lvl_cols <- c("ID", "SEX", "MED1", "MED2")
  fn_auto <- apply_lul_wide(xpdb_small, .problem = 1, lvl_cols = lvl_cols)
  fn_explicit <- apply_lul_wide(xpdb_small, cols = names(baseline), .problem = 1, lvl_cols = lvl_cols)

  expect_identical(fn_auto(baseline), fn_explicit(baseline))
})

test_that("GGally wrapper works", {
  wrapped <- wrap_xp_ggally("count", xp_xtra_theme())(data.frame(a=1:10),aes(x=a,y=a))
  direct <- GGally::ggally_count(data.frame(a=1:10),aes(x=a,y=a))

  expect_identical(
    wrapped$data,
    direct$data
  )

})

test_that("stratified_alloc allocates proportionally with largest-remainder rounding", {
  # Evenly divisible: no rounding needed
  expect_equal(stratified_alloc(c(50, 50), 10), c(5L, 5L))

  # 59/15 split (SEX in xpdb_x) sampling 9: raw = 7.176/1.824 -> remainder to SEX2
  expect_equal(stratified_alloc(c(59, 15), 9), c(7L, 2L))

  # A tiny stratum smaller than its proportional share contributes all of
  # its members, with the shortfall reallocated elsewhere.
  expect_equal(stratified_alloc(c(1, 99), 10), c(1L, 9L))

  # n larger than total is capped at total
  expect_equal(stratified_alloc(c(3, 4), 100), c(3L, 4L))

  # single stratum gets everything
  expect_equal(stratified_alloc(10, 4), 4L)
})

test_that("ind_plots_sample samples the requested number of individuals", {
  p <- ind_plots_sample(xpdb_x, n = 6, seed = 42, quiet = TRUE)

  expect_s3_class(p, "xpose_plot")
  expect_length(unique(p$data$ID), 6)
})

test_that("ind_plots_sample defaults to n = 9", {
  p <- ind_plots_sample(xpdb_x, seed = 1, quiet = TRUE)
  expect_length(unique(p$data$ID), 9)
})

test_that("ind_plots_sample uses all individuals when n exceeds availability", {
  n_ids <- length(unique(xpose::get_data(xpdb_x, quiet = TRUE)$ID))

  p <- ind_plots_sample(xpdb_x, n = n_ids + 100, quiet = TRUE)
  expect_length(unique(p$data$ID), n_ids)
})

test_that("ind_plots_sample errors on invalid `n`", {
  expect_error(ind_plots_sample(xpdb_x, n = 0, quiet = TRUE))
  expect_error(ind_plots_sample(xpdb_x, n = -1, quiet = TRUE))
  expect_error(ind_plots_sample(xpdb_x, n = 5.5, quiet = TRUE))
})

test_that("ind_plots_sample is reproducible with `seed` and restores RNG state", {
  p1 <- ind_plots_sample(xpdb_x, n = 6, seed = 42, quiet = TRUE)
  p2 <- ind_plots_sample(xpdb_x, n = 6, seed = 42, quiet = TRUE)
  expect_identical(sort(unique(p1$data$ID)), sort(unique(p2$data$ID)))

  # RNG state before/after should be unaffected by an internally-set seed
  set.seed(123)
  before <- .Random.seed
  invisible(ind_plots_sample(xpdb_x, n = 6, seed = 999, quiet = TRUE))
  expect_identical(.Random.seed, before)

  # Same, but for the no-prior-seed case
  if (exists(".Random.seed", envir = .GlobalEnv)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  invisible(ind_plots_sample(xpdb_x, n = 6, seed = 999, quiet = TRUE))
  expect_false(exists(".Random.seed", envir = .GlobalEnv))
})

test_that("ind_plots_sample stratifies proportionally and facets by the strata", {
  p <- ind_plots_sample(xpdb_x, n = 9, stratify = SEX, seed = 42, quiet = TRUE)

  id_tbl <- p$data[!duplicated(p$data$ID), c("ID", "SEX")]
  expect_equal(nrow(id_tbl), 9)
  expect_equal(sort(as.integer(table(id_tbl$SEX))), c(2, 7))

  facet_names <- names(p$facet$params$facets)
  expect_setequal(facet_names, c("ID", "SEX"))
})

test_that("ind_plots_sample drops strata that receive a zero allocation", {
  # With n = 1 across 2 strata, one stratum's allocation rounds down to
  # zero and should contribute no individuals at all.
  p <- ind_plots_sample(xpdb_x, n = 1, stratify = SEX, seed = 42, quiet = TRUE)

  id_tbl <- p$data[!duplicated(p$data$ID), c("ID", "SEX")]
  expect_equal(nrow(id_tbl), 1)
})

test_that("ind_plots_sample defaults `quiet` from the xpdb when not supplied", {
  xpdb_quiet <- xpdb_x
  xpdb_quiet$options$quiet <- TRUE

  expect_no_message(p <- ind_plots_sample(xpdb_quiet, n = 5))
  expect_s3_class(p, "xpose_plot")
})

test_that("ind_plots_sample supports multi-column stratification", {
  p <- ind_plots_sample(xpdb_x, n = 8, stratify = c(SEX, MED1), seed = 42, quiet = TRUE)

  id_tbl <- p$data[!duplicated(p$data$ID), c("ID", "SEX", "MED1")]
  expect_equal(nrow(id_tbl), 8)

  facet_names <- names(p$facet$params$facets)
  expect_setequal(facet_names, c("ID", "SEX", "MED1"))
})

test_that("ind_plots_sample errors when `stratify` resolves only to the id column", {
  expect_error(
    ind_plots_sample(xpdb_x, n = 5, stratify = ID, quiet = TRUE),
    "did not resolve to any columns"
  )
})

test_that("ind_plots_sample respects explicitly supplied `facets`", {
  p <- ind_plots_sample(xpdb_x, n = 5, facets = "SEX", quiet = TRUE)
  # Default logic (facet by id [+ stratify]) is skipped entirely when the
  # caller supplies their own `facets`.
  expect_setequal(names(p$facet$params$facets), "SEX")
})

test_that("ind_plots_sample passes `...` through to xpose::ind_plots", {
  p <- ind_plots_sample(xpdb_x, n = 5, seed = 1, quiet = TRUE, title = "custom title")
  expect_equal(p$labels$title, "custom title")
})
