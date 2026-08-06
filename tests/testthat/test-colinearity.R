test_that("get_cov_matrix works for nonmem models", {
  prm <- get_prm(xpdb_x, show_all = TRUE)
  n_free <- sum(!prm$fixed)

  cor_mat <- get_cov_matrix(xpdb_x, quiet = TRUE)
  expect_true(is.matrix(cor_mat))
  expect_equal(dim(cor_mat), c(n_free, n_free))
  expect_equal(rownames(cor_mat), colnames(cor_mat))
  # diagonal should be 1 (NONMEM stores SE on the .cor diagonal, not 1)
  expect_equal(unname(diag(cor_mat)), rep(1, n_free))
  expect_true(all(cor_mat >= -1 & cor_mat <= 1))
  expect_true(isSymmetric(unname(cor_mat)))

  cov_mat <- get_cov_matrix(xpdb_x, type = "covariance", quiet = TRUE)
  expect_equal(dim(cov_mat), c(n_free, n_free))
  expect_true(isSymmetric(unname(cov_mat)))
  # covariance diagonal is variance, left untouched (unlike .cor, no fixup needed)
  expect_true(all(diag(cov_mat) > 0))

  # drop_fixed = FALSE keeps NONMEM's placeholder zero rows/cols
  full_mat <- get_cov_matrix(xpdb_x, drop_fixed = FALSE, quiet = TRUE)
  expect_equal(dim(full_mat), c(nrow(prm), nrow(prm)))
})

test_that("get_cov_matrix picks the right table for multi-subprob models", {
  # pheno_saem has SAEM (subprob 1) then IMP (subprob 2) cor/cov tables;
  # the default (final, unspecified) subprob should be used, same as get_prm
  prm <- get_prm(pheno_saem, show_all = TRUE, quiet = TRUE)
  expect_equal(attr(prm, "method"), "imp")

  cor_mat <- get_cov_matrix(pheno_saem, quiet = TRUE)
  n_free <- sum(!prm$fixed)
  expect_equal(dim(cor_mat), c(n_free, n_free))
  expect_setequal(rownames(cor_mat), prm$name[!prm$fixed])

  # explicitly requesting the saem (subprob 1) table should differ. Use
  # drop_fixed = FALSE here: xpose::get_prm() has a pre-existing, unrelated
  # limitation transforming this particular SAEM .ext output, and drop_fixed
  # is the only thing that would otherwise route through get_prm().
  saem_mat <- get_cov_matrix(pheno_saem, .subprob = 1, drop_fixed = FALSE, quiet = TRUE)
  shared_names <- intersect(rownames(cor_mat), rownames(saem_mat))
  expect_false(isTRUE(all.equal(
    cor_mat[shared_names, shared_names],
    saem_mat[shared_names, shared_names]
  )))
})

test_that("get_cov_matrix errors informatively when the covariance step is missing (nonmem)", {
  xpdb_nocov <- xpdb_x
  xpdb_nocov$files <- xpdb_nocov$files[!xpdb_nocov$files$extension %in% c("cor", "cov"), ]

  expect_error(
    get_cov_matrix(xpdb_nocov, quiet = TRUE),
    "No correlation matrix available"
  )
  expect_error(
    get_cov_matrix(xpdb_nocov, type = "covariance", quiet = TRUE),
    "No covariance matrix available"
  )
})

test_that("get_cov_matrix errors for unsupported software", {
  xpdb_fake <- xpdb_x
  xpdb_fake$summary$value[xpdb_fake$summary$label == "software"] <- "monolix"

  expect_error(
    get_cov_matrix(xpdb_fake, quiet = TRUE),
    "not implemented for.*monolix"
  )
})

test_that("get_cov_matrix works for nlmixr2 models", {
  skip_if_not_installed("rxode2")
  skip_if(
    utils::packageVersion("rxode2") < "5.0",
    "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)"
  )
  skip_if_not_installed("nlmixr2est")
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("xpose.nlmixr2")

  xpdb_nlmixr2 <- cached_nlmixr_example("xpdb_nlmixr2")

  cor_mat <- get_cov_matrix(xpdb_nlmixr2, quiet = TRUE)
  expect_true(is.matrix(cor_mat))
  expect_true(isSymmetric(unname(cor_mat)))
  expect_equal(unname(diag(cor_mat)), rep(1, nrow(cor_mat)))
  expect_true(all(cor_mat >= -1 & cor_mat <= 1))

  cov_mat <- get_cov_matrix(xpdb_nlmixr2, type = "covariance", quiet = TRUE)
  expect_equal(dim(cov_mat), dim(cor_mat))

  # nlmixr2 always reports uncertainty for fixed effects; newer nlmixr2est
  # versions may also report it for omega diagonal (variance) elements,
  # named "om.<eta name>" -- anything beyond the theta names must be one
  # of those, not something unexpected
  prm <- get_prm(xpdb_nlmixr2, show_all = TRUE, quiet = TRUE)
  theta_names <- prm$name[prm$type == "the"]
  expect_true(all(theta_names %in% rownames(cor_mat)))
  extra_names <- setdiff(rownames(cor_mat), theta_names)
  expect_true(all(extra_names %in% paste0("om.", prm$name[prm$type == "ome"])))

  # drop_fixed has no effect for nlmixr2 (nothing to drop)
  expect_equal(
    get_cov_matrix(xpdb_nlmixr2, drop_fixed = FALSE, quiet = TRUE),
    cor_mat
  )
})

test_that("get_cov_matrix errors informatively when the covariance step is missing (nlmixr2)", {
  skip_if_not_installed("rxode2")
  skip_if(
    utils::packageVersion("rxode2") < "5.0",
    "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)"
  )
  skip_if_not_installed("nlmixr2est")
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("xpose.nlmixr2")

  xpdb_nocov <- cached_nlmixr_example("xpdb_nlmixr2_nocov")

  expect_error(
    get_cov_matrix(xpdb_nocov, quiet = TRUE),
    "No correlation matrix available"
  )
  expect_error(
    get_cov_matrix(xpdb_nocov, type = "covariance", quiet = TRUE),
    "No covariance matrix available"
  )
})

test_that("get_cov_matrix requires a nlmixr2 fit to be attached", {
  skip_if_not_installed("rxode2")
  skip_if_not_installed("nlmixr2est")

  xpdb_no_fit <- xpdb_x
  xpdb_no_fit$summary$value[xpdb_no_fit$summary$label == "software"] <- "nlmixr2"

  expect_error(
    get_cov_matrix(xpdb_no_fit, quiet = TRUE),
    "fit"
  )
})

test_that("xplot_heatmap is a generic, standalone matrix plotting template", {
  m <- matrix(
    c(1, 0.6, NA, 0.6, 1, NA, NA, NA, 1),
    nrow = 3,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  )

  p <- xplot_heatmap(xpdb_x, m, quiet = TRUE)
  expect_s3_class(p, "xpose_plot")
  geoms <- purrr::map_chr(p$layers, ~ class(.x$geom)[1])
  expect_setequal(geoms, c("GeomTile", "GeomText"))

  # only non-NA cells are plotted
  expect_equal(nrow(p$data), sum(!is.na(m)))

  # requires dimnames
  m_noname <- unname(m)
  expect_error(
    xplot_heatmap(xpdb_x, m_noname, quiet = TRUE),
    "row and column names"
  )

  # requires a numeric matrix
  expect_error(
    xplot_heatmap(xpdb_x, as.data.frame(m), quiet = TRUE)
  )
})

test_that("cormat produces a heatmap for nonmem models", {
  p <- cormat(xpdb_x, quiet = TRUE)
  expect_s3_class(p, "xpose_plot")
  expect_true(inherits(p, "gg"))

  geoms <- purrr::map_chr(p$layers, ~ class(.x$geom)[1])
  expect_setequal(geoms, c("GeomTile", "GeomText"))

  # covariance variant
  p_cov <- cormat(xpdb_x, type = "covariance", quiet = TRUE)
  expect_s3_class(p_cov, "xpose_plot")

  # multi-subprob model (pheno_saem) works too
  p_saem <- cormat(pheno_saem, quiet = TRUE)
  expect_s3_class(p_saem, "xpose_plot")
})

test_that("cormat respects xp_theme and gg_theme", {
  def <- cormat(xpdb_x, quiet = TRUE)
  themed <- cormat(xpdb_x, quiet = TRUE, gg_theme = xpose::theme_bw2())
  expect_failure(expect_equal(def$theme$panel.border, themed$theme$panel.border))

  expect_failure(expect_identical(
    def,
    cormat(xpdb_x, quiet = TRUE, xp_theme = xpose::theme_xp_xpose4())
  ))
})

test_that("cormat errors informatively when the covariance step is missing", {
  xpdb_nocov <- xpdb_x
  xpdb_nocov$files <- xpdb_nocov$files[!xpdb_nocov$files$extension %in% c("cor", "cov"), ]

  expect_error(
    cormat(xpdb_nocov, quiet = TRUE),
    "No correlation matrix available"
  )
})

test_that("get_cov_matrix and cormat default quiet from xpdb options when omitted", {
  expect_no_error(get_cov_matrix(xpdb_x))
  expect_no_error(cormat(xpdb_x))
})

test_that("cormat requires at least two estimated parameters", {
  xpdb_1prm <- xpdb_x
  cor_idx <- which(xpdb_1prm$files$extension == "cor")
  xpdb_1prm$files$data[[cor_idx]] <- xpdb_1prm$files$data[[cor_idx]][1, c("NAME", "THETA1")]

  expect_error(
    cormat(xpdb_1prm, quiet = TRUE),
    "At least two estimated parameters"
  )
})

test_that("cormat works for nlmixr2 models", {
  skip_if_not_installed("rxode2")
  skip_if(
    utils::packageVersion("rxode2") < "5.0",
    "nlmixr2 tests require rxode2 >= 5.0 (incompatible serialization in older versions)"
  )
  skip_if_not_installed("nlmixr2est")
  skip_if_not_installed("nlmixr2data")
  skip_if_not_installed("xpose.nlmixr2")

  xpdb_nlmixr2 <- cached_nlmixr_example("xpdb_nlmixr2")
  p <- cormat(xpdb_nlmixr2, quiet = TRUE)
  expect_s3_class(p, "xpose_plot")

  xpdb_nocov <- cached_nlmixr_example("xpdb_nlmixr2_nocov")
  expect_error(
    cormat(xpdb_nocov, quiet = TRUE),
    "No correlation matrix available"
  )
})
