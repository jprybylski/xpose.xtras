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

