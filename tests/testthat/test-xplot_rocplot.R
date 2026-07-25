test_that("xplot_rocplot adds expected geoms", {
  opt <- xpose::data_opt(post_processing = function(df) {
    df %>%
      dplyr::group_by(ID) %>%
      dplyr::slice_head(n = 2) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(prob = dplyr::row_number() / dplyr::n(),
                    OBS = as.integer(dplyr::row_number() %% 2))
  })
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  base_args <- list(xpdb = xpdb_x, like_col = "prob",
                    obs_col = "OBS", obs_target = 1, opt = opt, guide = FALSE,
                    quiet = TRUE)
  roc_c <- do.call(xplot_rocplot, c(base_args, list(type = "c")))
  expect_true("GeomPath" %in% geoms_lists(roc_c))
  roc_p <- do.call(xplot_rocplot, c(base_args, list(type = "p", group = "ID")))
  expect_true("GeomPoint" %in% geoms_lists(roc_p))
  roc_t <- do.call(xplot_rocplot, c(base_args, list(type = "t", group = "ID")))
  expect_true("GeomText" %in% geoms_lists(roc_t))
  roc_a <- do.call(xplot_rocplot, c(base_args, list(type = "ca")))
  expect_true("GeomLabel" %in% geoms_lists(roc_a))
  roc_k <- do.call(xplot_rocplot, c(base_args, list(type = "ck")))
  expect_true("GeomPoint" %in% geoms_lists(roc_k))

  # `guide` (default TRUE) adds the unity reference line, and `facets`
  # threads through to xpose_panels() -- both need a per-group (strata)
  # computation distinct from the "no facets" cases above
  roc_guided_facets <- xplot_rocplot(xpdb_x, like_col = "prob", obs_col = "OBS",
                                     obs_target = 1, opt = opt, type = "c",
                                     facets = "SEX", quiet = TRUE)
  expect_true("GeomAbline" %in% geoms_lists(roc_guided_facets))
  expect_false(is.null(roc_guided_facets$facet))
})

test_that("xplot_rocplot defaults/theming/error branches are covered", {
  opt <- xpose::data_opt(post_processing = function(df) {
    df %>%
      dplyr::group_by(ID) %>%
      dplyr::slice_head(n = 2) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(prob = dplyr::row_number() / dplyr::n(),
                    OBS = as.integer(dplyr::row_number() %% 2))
  })
  base_args <- list(xpdb = xpdb_x, like_col = "prob",
                    obs_col = "OBS", obs_target = 1, opt = opt, guide = FALSE)

  # missing `quiet` falls back to xpdb$options$quiet (not quiet -> message)
  expect_message(
    do.call(xplot_rocplot, c(base_args, list(type = "c"))),
    "Using data from"
  )

  # missing `opt` falls back to xpose::data_opt() (default data source)
  expect_no_error(
    xplot_rocplot(pkpd_m3, like_col = "LIKE", obs_col = "BLQ", obs_target = 1,
                  type = "c", guide = FALSE, quiet = TRUE)
  )

  # empty data errors informatively
  empty_opt <- xpose::data_opt(post_processing = function(d) d[0, ])
  expect_error(
    xplot_rocplot(xpdb_x, like_col = "prob", obs_col = "OBS", obs_target = 1,
                  opt = empty_opt, quiet = TRUE),
    "No data available"
  )

  # curve-level 0s warning: when the (ungrouped) curve data itself has only
  # one BLQ value present, TPR or FPR is NaN for every threshold
  id5_opt <- xpose::data_opt(post_processing = function(df) dplyr::filter(df, ID == 5))
  expect_warning(
    xplot_rocplot(pkpd_m3, like_col = "LIKE", obs_col = "BLQ", obs_target = 1,
                  type = "c", opt = id5_opt, guide = FALSE, quiet = TRUE),
    "not calculable"
  )

  # points-layer-specific 0s warning: per-ID grouping can leave some
  # individuals with a single BLQ value (P or N == 0 for that ID), which is
  # a distinct code path from the curve-level (ungrouped) 0s warning above
  expect_warning(
    xplot_rocplot(pkpd_m3, like_col = "LIKE", obs_col = "BLQ", obs_target = 1,
                  type = "p", group = "ID", guide = FALSE, quiet = TRUE),
    "not calculable"
  )

  # xp_theme override is applied
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  def_plot <- do.call(xplot_rocplot, c(base_args, list(type = "c", quiet = TRUE)))
  themed_plot <- do.call(xplot_rocplot, c(base_args, list(type = "c", quiet = TRUE,
                                                          xp_theme = xpose::theme_xp_xpose4())))
  expect_failure(expect_identical(def_plot, themed_plot))

  # non-xp_xtras input still works
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  base_args_nonxtra <- base_args
  base_args_nonxtra$xpdb <- xpdb_ex_pk
  expect_no_error(
    do.call(xplot_rocplot, c(base_args_nonxtra, list(type = "c", quiet = TRUE)))
  )

  # explicit gg_theme override is applied (rather than the xpdb's default)
  gg_themed_plot <- do.call(xplot_rocplot, c(base_args, list(type = "c", quiet = TRUE,
                                                             gg_theme = xpose::theme_bw2())))
  expect_equal(gg_themed_plot$theme$panel.border, xpose::theme_bw2()$panel.border)
  expect_failure(expect_equal(def_plot$theme$panel.border, xpose::theme_bw2()$panel.border))
})

test_that("xplot_rocplot errors when requirements not met", {
  opt <- xpose::data_opt(post_processing = function(df) {
    df %>%
      dplyr::group_by(ID) %>%
      dplyr::slice_head(n = 2) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(prob = dplyr::row_number() / dplyr::n(),
                    OBS = as.integer(dplyr::row_number() %% 2))
  })
  expect_error(
    xplot_rocplot(xpdb_x, type = "p", like_col = "prob", obs_col = "OBS",
                  obs_target = 1, opt = opt, quiet = TRUE),
    "group"
  )
  expect_error(
    xplot_rocplot(xpdb_x, type = "a", group = "ID", like_col = "prob",
                  obs_col = "OBS", obs_target = 1, opt = opt, quiet = TRUE),
    "Need curve"
  )
})
# New tests for wrapper functions

test_that("roc_plot adds expected geoms", {
  xpdb <- pkpd_m3 %>%
    set_var_types(catdv = BLQ, dvprobs = LIKE) %>%
    set_dv_probs(1, 1 ~ LIKE, .dv_var = BLQ) %>%
    set_var_levels(1, BLQ = lvl_bin())
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  expect_warning(
    roc_plot(xpdb, cutpoint = 1, type = "cak", quiet = TRUE, guide = FALSE),
    ".*sens.*spec.*not calc.*0s.*"
  )
  roc <- suppressWarnings(roc_plot(xpdb, cutpoint = 1, type = "cak", quiet = TRUE, guide = FALSE))
  expect_true("GeomPath" %in% geoms_lists(roc))
  expect_true("GeomLabel" %in% geoms_lists(roc))
  expect_true("GeomPoint" %in% geoms_lists(roc))
})

test_that("ind_roc returns set of ROC curves", {
  xpdb <- xpdb_x %>%
    mutate(
      # Ensure each person has mix of 1 or 0 BLQ
      BLQ = 1*(seq_len(length(DV))%%2 == 0),
      # Doesn't matter what this is
      LIKE = runif(length(DV))
    ) %>%
    set_var_types(catdv = BLQ, dvprobs = LIKE) %>%
    set_dv_probs(1, 1 ~ LIKE, .dv_var = BLQ) %>%
    set_var_levels(1, BLQ = lvl_bin())
  roc <- suppressWarnings(ind_roc(xpdb, type = "c", quiet = TRUE))
  # Ensure number of facets is number of IDs
  expect_equal(
    length(ggplot2::ggplot_build(roc)$layout$layout$PANEL),
    as.numeric(get_prop(xpdb, "nind"))
  )
})

test_that("roc_by_mod set ROC curves per model", {
  base <- xpdb_x %>%
    mutate(
      # Dummy
      BLQ = 1*(seq_len(length(DV))%%2 == 0),
      LIKE = runif(length(DV))
    ) %>%
    set_var_types(catdv = BLQ, dvprobs = LIKE) %>%
    set_dv_probs(1, 1 ~ LIKE, .dv_var = BLQ) %>%
    set_var_levels(1, BLQ = lvl_bin())
  m3_set <- xpose_set(
    run1 = set_prop(base, run = "run1"),
    run2 = set_prop(base, run = "run2")
  )
  roc <- roc_by_mod(m3_set, type = "c", quiet = TRUE)
  # Ensure number of facets is number of runs
  expect_equal(
    length(ggplot2::ggplot_build(roc)$layout$layout$PANEL),
    length(m3_set)
  )
})

