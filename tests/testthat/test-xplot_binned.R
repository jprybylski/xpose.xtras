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
