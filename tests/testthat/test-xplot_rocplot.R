test_that("xplot_rocplot adds expected geoms", {
  data("xpdb_x", package = "xpose.xtras", envir = environment())
  opt <- xpose::data_opt(post_processing = function(df) {
    df %>%
      dplyr::group_by(ID) %>%
      dplyr::slice_head(n = 2) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(prob = dplyr::row_number() / dplyr::n(),
                    OBS = as.integer(dplyr::row_number() %% 2))
  })
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  base_args <- list(xpdb = xpdb_x, group = "ID", like_col = "prob",
                    obs_col = "OBS", obs_target = 1, opt = opt, guide = FALSE,
                    quiet = TRUE)
  roc_c <- do.call(xplot_rocplot, c(base_args, list(type = "c")))
  expect_true("GeomPath" %in% geoms_lists(roc_c))
  roc_p <- do.call(xplot_rocplot, c(base_args, list(type = "p")))
  expect_true("GeomPoint" %in% geoms_lists(roc_p))
  roc_t <- do.call(xplot_rocplot, c(base_args, list(type = "t")))
  expect_true("GeomText" %in% geoms_lists(roc_t))
  roc_a <- do.call(xplot_rocplot, c(base_args, list(type = "ca")))
  expect_true("GeomLabel" %in% geoms_lists(roc_a))
  roc_k <- do.call(xplot_rocplot, c(base_args, list(type = "ck")))
  expect_true("GeomPoint" %in% geoms_lists(roc_k))
})

test_that("xplot_rocplot errors when requirements not met", {
  data("xpdb_x", package = "xpose.xtras", envir = environment())
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
  data("pkpd_m3", package = "xpose.xtras", envir = environment())
  xpdb <- pkpd_m3 %>%
    set_var_types(catdv = BLQ, dvprobs = LIKE) %>%
    set_dv_probs(1, 1 ~ LIKE, .dv_var = BLQ) %>%
    set_var_levels(1, BLQ = lvl_bin())
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  roc <- roc_plot(xpdb, cutpoint = 1, type = "cak", quiet = TRUE, guide = FALSE)
  expect_true("GeomPath" %in% geoms_lists(roc))
  expect_true("GeomLabel" %in% geoms_lists(roc))
  expect_true("GeomPoint" %in% geoms_lists(roc))
})

test_that("ind_roc returns patchwork of ROC curves", {
  data("pkpd_m3", package = "xpose.xtras", envir = environment())
  xpdb <- pkpd_m3 %>%
    set_var_types(catdv = BLQ, dvprobs = LIKE) %>%
    set_dv_probs(1, 1 ~ LIKE, .dv_var = BLQ) %>%
    set_var_levels(1, BLQ = lvl_bin())
  roc <- ind_roc(xpdb, type = "c", quiet = TRUE)
  expect_s3_class(roc, "patchwork")
  first_plot <- roc$patches$plots[[1]]
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  expect_true("GeomPath" %in% geoms_lists(first_plot))
})

test_that("roc_by_mod produces ROC curves per model", {
  data("pkpd_m3", package = "xpose.xtras", envir = environment())
  base <- pkpd_m3 %>%
    set_var_types(catdv = BLQ, dvprobs = LIKE) %>%
    set_dv_probs(1, 1 ~ LIKE, .dv_var = BLQ) %>%
    set_var_levels(1, BLQ = lvl_bin())
  m3_set <- xpose_set(
    run1 = set_prop(base, run = "run1"),
    run2 = set_prop(base, run = "run2")
  )
  roc <- roc_by_mod(m3_set, type = "c", quiet = TRUE)
  expect_s3_class(roc, "patchwork")
  first_plot <- roc$patches$plots[[1]]
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])
  expect_true("GeomPath" %in% geoms_lists(first_plot))
})

