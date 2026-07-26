test_that("xplot_binned is a generic connected-series trend plot template", {
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  base_mapping <- aes(x = .data[["MED1"]], y = .data[["ETA1"]])

  # default type 'pl': point + line
  def_plot <- xplot_binned(xpdb_x, base_mapping, group = "SEX", quiet = TRUE)
  expect_s3_class(def_plot, "xpose_plot")
  expect_setequal(geoms_lists(def_plot), c("GeomPoint", "GeomLine"))

  # type toggles
  expect_setequal(
    geoms_lists(xplot_binned(xpdb_x, base_mapping, group = "SEX", type = "p", quiet = TRUE)),
    "GeomPoint"
  )
  expect_setequal(
    geoms_lists(xplot_binned(xpdb_x, base_mapping, group = "SEX", type = "l", quiet = TRUE)),
    "GeomLine"
  )
  expect_setequal(
    geoms_lists(xplot_binned(xpdb_x, base_mapping, group = "SEX", type = "s", quiet = TRUE)),
    "GeomSmooth"
  )
  expect_setequal(
    geoms_lists(xplot_binned(xpdb_x, base_mapping, group = "SEX", type = "pls", quiet = TRUE)),
    c("GeomPoint", "GeomLine", "GeomSmooth")
  )

  # Series are coloured by `group`, not shadowed by xp_theme's fixed
  # point/line colour default (see xp_geoms()/xp_map(): a fixed layer
  # param normally wins over an *inherited* aes unless excluded)
  built <- ggplot2::ggplot_build(def_plot)
  point_layer <- which(geoms_lists(def_plot) == "GeomPoint")
  point_colours <- unique(built$data[[point_layer]]$colour)
  expect_gt(length(point_colours), 1)

  # requires row/col-having, non-empty data
  expect_error(
    xplot_binned(xpdb_x, base_mapping, group = "SEX",
                 opt = xpose::data_opt(post_processing = function(df) df[0, ]), quiet = TRUE),
    "No data available"
  )

  # facets pass through like other generic templates
  facet_plot <- xplot_binned(xpdb_x, base_mapping, group = "SEX", facets = "SEX", quiet = TRUE)
  expect_false(is.null(facet_plot$facet))
})

test_that("xplot_binned defaults/theming branches are covered", {
  base_mapping <- aes(x = .data[["MED1"]], y = .data[["ETA1"]])

  # missing `quiet` falls back to xpdb$options$quiet (and, being not quiet,
  # emits xpose's usual "Using data from" message)
  expect_message(
    xplot_binned(xpdb_x, base_mapping, group = "SEX"),
    "Using data from"
  )

  # non-xp_xtras input still works, getting a themed xp_xtra_theme() applied
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  expect_no_error(
    xplot_binned(xpdb_ex_pk, base_mapping, group = "SEX", quiet = TRUE)
  )

  # xp_theme override is applied
  def_plot <- xplot_binned(xpdb_x, base_mapping, group = "SEX", quiet = TRUE)
  themed_plot <- xplot_binned(xpdb_x, base_mapping, group = "SEX", quiet = TRUE,
                              xp_theme = xpose::theme_xp_xpose4())
  expect_failure(expect_identical(def_plot, themed_plot))

  # explicit gg_theme override is applied (rather than the xpdb's default)
  gg_themed_plot <- xplot_binned(xpdb_x, base_mapping, group = "SEX", quiet = TRUE,
                                 gg_theme = xpose::theme_bw2())
  expect_equal(gg_themed_plot$theme$panel.border, xpose::theme_bw2()$panel.border)
  expect_failure(expect_equal(def_plot$theme$panel.border, xpose::theme_bw2()$panel.border))
})
