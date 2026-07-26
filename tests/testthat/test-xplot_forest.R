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

  # other unrecognized letters still get xpose's own lenient warning
  expect_warning(
    xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "z", quiet = TRUE),
    "not recognized"
  )
})

test_that("xplot_forest defaults/theming branches are covered", {
  test_df <- tibble::tibble(
    y = factor(c("a", "b", "c"), levels = c("a", "b", "c")),
    x = c(0.9, 1.0, 1.2),
    xmin = c(0.8, 0.95, 1.1),
    xmax = c(1.0, 1.05, 1.3)
  )
  opt <- xpose::data_opt(post_processing = function(d) test_df)
  vars <- ggplot2::aes(x = x, y = y, xmin = xmin, xmax = xmax)

  # missing `quiet` falls back to xpdb$options$quiet (not quiet -> message)
  expect_message(
    xplot_forest(xpdb_x, mapping = vars, opt = opt),
    "Using data from"
  )

  # missing `opt` falls back to xpose::data_opt() (default data source)
  expect_no_error(
    xplot_forest(xpdb_x, mapping = ggplot2::aes(x = MED1, y = ETA1, xmin = ETA1, xmax = ETA1), quiet = TRUE)
  )

  # xp_theme override is applied
  def_plot <- xplot_forest(xpdb_x, mapping = vars, opt = opt, quiet = TRUE)
  themed_plot <- xplot_forest(xpdb_x, mapping = vars, opt = opt, quiet = TRUE,
                              xp_theme = xpose::theme_xp_xpose4())
  expect_failure(expect_identical(def_plot, themed_plot))

  # non-xp_xtras input still works
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  expect_no_error(
    xplot_forest(xpdb_ex_pk, mapping = vars, opt = opt, quiet = TRUE)
  )

  # explicit gg_theme override is applied (rather than the xpdb's default)
  gg_themed_plot <- xplot_forest(xpdb_x, mapping = vars, opt = opt, quiet = TRUE,
                                 gg_theme = xpose::theme_bw2())
  expect_equal(gg_themed_plot$theme$panel.border, xpose::theme_bw2()$panel.border)
  expect_failure(expect_equal(def_plot$theme$panel.border, xpose::theme_bw2()$panel.border))
})

test_that("xplot_forest errors on empty data", {
  empty_opt <- xpose::data_opt(post_processing = function(d) d[0, ])
  expect_error(
    xplot_forest(xpdb_x, opt = empty_opt, quiet = TRUE),
    "No data available"
  )
})

test_that("xplot_forest violin layer needs its own violin_opt/mapping", {
  test_df <- tibble::tibble(
    y = factor(c("a", "b"), levels = c("a", "b")),
    x = c(0.9, 1.2),
    xmin = c(0.8, 1.1),
    xmax = c(1.0, 1.3)
  )
  opt <- xpose::data_opt(post_processing = function(d) test_df)
  vars <- ggplot2::aes(x = x, y = y, xmin = xmin, xmax = xmax)

  # type="v" without violin_opt errors with a specific, actionable message
  expect_error(
    xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "v", quiet = TRUE),
    "violin_opt"
  )

  draws_df <- tibble::tibble(
    y = rep(c("a", "b"), each = 100),
    val = c(stats::rnorm(100, 0.9, 0.05), stats::rnorm(100, 1.2, 0.05))
  )
  violin_opt <- xpose::data_opt(post_processing = function(d) draws_df)
  violin_vars <- xpose::aes_c(vars, ggplot2::aes(violin_x = val, violin_y = y))

  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])

  expect_message(
    p_violin <- xplot_forest(xpdb_x, mapping = violin_vars, opt = opt, violin_opt = violin_opt,
                             type = "piv", vline_xintercept = 1, quiet = FALSE),
    "violin layer"
  )
  expect_setequal(geoms_lists(p_violin), c("GeomPoint", "GeomLinerange", "GeomViolin"))

  # the violin layer must not inherit opt's xmin/xmax (different data/shape)
  violin_layer <- p_violin$layers[[which(geoms_lists(p_violin) == "GeomViolin")]]
  expect_false(violin_layer$inherit.aes)
  expect_equal(violin_layer$data, draws_df, ignore_attr = TRUE)
})

test_that("xplot_forest shaded reference region ('r')", {
  test_df <- tibble::tibble(
    y = factor(c("a", "b"), levels = c("a", "b")),
    x = c(0.9, 1.2),
    xmin = c(0.8, 1.1),
    xmax = c(1.0, 1.3)
  )
  opt <- xpose::data_opt(post_processing = function(d) test_df)
  vars <- ggplot2::aes(x = x, y = y, xmin = xmin, xmax = xmax)

  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  rect_layer <- function(gg) gg$layers[[which(geoms_lists(gg) == "GeomRect")]]

  # default region (region = NULL, "r" requested) falls back to c(0.8, 1.25)
  p_default_region <- xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "pr", quiet = TRUE)
  expect_setequal(geoms_lists(p_default_region), c("GeomPoint", "GeomRect"))
  rl <- rect_layer(p_default_region)
  expect_equal(rl$data$xmin, 0.8)
  expect_equal(rl$data$xmax, 1.25)
  expect_equal(rl$data$ymin, -Inf)
  expect_equal(rl$data$ymax, Inf)

  # custom region
  p_custom_region <- xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "pr", region = c(0.7, 1.43), quiet = TRUE)
  rl2 <- rect_layer(p_custom_region)
  expect_equal(rl2$data$xmin, 0.7)
  expect_equal(rl2$data$xmax, 1.43)

  # invalid region errors clearly
  expect_error(
    xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "pr", region = c(1.25, 0.8), quiet = TRUE),
    "low < high"
  )
  expect_error(
    xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "pr", region = 1.25, quiet = TRUE),
    "length-2"
  )

  # region is drawn first (bottom-most layer)
  expect_identical(unname(geoms_lists(p_default_region)[1]), "GeomRect")

  # orientation="x" flips region onto y
  p_region_x <- xplot_forest(xpdb_x, mapping = vars, opt = opt, type = "r", orientation = "x", quiet = TRUE)
  rl3 <- rect_layer(p_region_x)
  expect_equal(rl3$data$ymin, 0.8)
  expect_equal(rl3$data$ymax, 1.25)
  expect_equal(rl3$data$xmin, -Inf)
})
