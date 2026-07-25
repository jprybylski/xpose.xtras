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
