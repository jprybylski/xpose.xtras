test_that("xplot_forest draws the expected geoms per type", {
  test_df <- tibble::tibble(
    y = factor(c("a", "b", "c"), levels = c("a", "b", "c")),
    x = c(0.9, 1.0, 1.2),
    xmin = c(0.8, 0.95, 1.1),
    xmax = c(1.0, 1.05, 1.3)
  )
  opt <- xpose::data_opt(post_processing = function(d) test_df)
  vars <- ggplot2::aes(x = x, y = y, xmin = xmin, xmax = xmax)

  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])

  # pi is default
  p_default <- xplot_forest(xpdb_x, mapping = vars, opt = opt, quiet = TRUE)
  expect_setequal(geoms_lists(p_default), c("GeomPoint", "GeomLinerange"))
  expect_s3_class(p_default, "xpose_plot")
  expect_identical(
    geoms_lists(p_default),
    geoms_lists(xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "pi", quiet = TRUE))
  )

  # reference line is opt-in via "l"
  p_line <- xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "pil", vline_xintercept = 1, quiet = TRUE)
  expect_setequal(geoms_lists(p_line), c("GeomVline", "GeomLinerange", "GeomPoint"))

  # point-only
  expect_setequal(
    geoms_lists(xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "p", quiet = TRUE)),
    "GeomPoint"
  )

  # interval-only
  expect_setequal(
    geoms_lists(xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "i", quiet = TRUE)),
    "GeomLinerange"
  )

  # orientation="x" flips the reference line to a hline
  expect_setequal(
    geoms_lists(xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "l", orientation = "x", quiet = TRUE)),
    "GeomHline"
  )

  # invalid type letters warn (not implemented yet -- see xplot_forest() docs),
  # same lenient behavior as other xplot_* functions' unrecognized type letters
  expect_warning(
    xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "v", quiet = TRUE),
    "not recognized"
  )
})

test_that("xplot_forest errors on empty data", {
  empty_opt <- xpose::data_opt(post_processing = function(d) d[0, ])
  expect_error(
    xplot_forest(xpdb_x, opt = empty_opt, quiet = TRUE),
    "No data available"
  )
})
