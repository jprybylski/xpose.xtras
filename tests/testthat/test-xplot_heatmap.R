test_that("xplot_heatmap defaults/theming branches are covered", {
  m <- matrix(
    c(1, 0.6, NA, 0.6, 1, NA, NA, NA, 1),
    nrow = 3,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )

  # missing `quiet` falls back to xpdb$options$quiet (not quiet -> no error;
  # xplot_heatmap does not fetch data via xpose::fetch_data(), so there's no
  # "Using data from" message here, just the ordinary quiet default lookup)
  expect_no_error(
    xplot_heatmap(xpdb_x, m)
  )

  # xp_theme override is applied
  def_plot <- xplot_heatmap(xpdb_x, m, quiet = TRUE)
  themed_plot <- xplot_heatmap(xpdb_x, m, quiet = TRUE, xp_theme = xpose::theme_xp_xpose4())
  expect_failure(expect_identical(def_plot, themed_plot))

  # non-xp_xtras input still works, getting a themed xp_xtra_theme() applied
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  expect_no_error(
    xplot_heatmap(xpdb_ex_pk, m, quiet = TRUE)
  )

  # explicit gg_theme override is applied (rather than the xpdb's default)
  gg_themed_plot <- xplot_heatmap(xpdb_x, m, quiet = TRUE, gg_theme = xpose::theme_bw2())
  expect_equal(gg_themed_plot$theme$panel.border, xpose::theme_bw2()$panel.border)
  expect_failure(expect_equal(def_plot$theme$panel.border, xpose::theme_bw2()$panel.border))

  # all-NA matrix (after dropping masked cells) has no data left to plot
  all_na <- matrix(NA_real_, nrow = 2, ncol = 2, dimnames = list(c("A", "B"), c("A", "B")))
  expect_error(
    xplot_heatmap(xpdb_x, all_na, quiet = TRUE),
    "No data available"
  )
})
