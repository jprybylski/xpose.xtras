test_that("xp_from_bbr converts a finished bbr model to an xp_xtras object", {
  skip_if_not_installed("bbr")

  mod <- make_bbr_fixture(description = "Phenobarbital SAEM model")
  xpdb <- suppressWarnings(xp_from_bbr(mod))

  expect_true(is_xp_xtras(xpdb))
  expect_true(check_xpdb_x(xpdb))
  expect_identical(get_prop(xpdb, "descr"), "Phenobarbital SAEM model")
})

test_that("xp_from_bbr respects .use_bbr_descr = FALSE", {
  skip_if_not_installed("bbr")

  mod <- make_bbr_fixture(description = "Phenobarbital SAEM model")
  xpdb <- suppressWarnings(xp_from_bbr(mod, .use_bbr_descr = FALSE))

  expect_false(identical(get_prop(xpdb, "descr"), "Phenobarbital SAEM model"))
})

test_that("xp_from_bbr forwards ... to xpose::xpose_data()", {
  skip_if_not_installed("bbr")

  mod <- make_bbr_fixture()
  xpdb <- suppressWarnings(xp_from_bbr(mod, gg_theme = ggplot2::theme_bw))

  expect_identical(attr(xpdb$gg_theme, "theme"), c("::", "ggplot2", "theme_bw"))
})

test_that("xp_from_bbr rejects objects that are not bbi_nonmem_model", {
  skip_if_not_installed("bbr")

  expect_error(xp_from_bbr(list()), class = "rlang_error")
  expect_error(xp_from_bbr(1:5), class = "rlang_error")
})

test_that("xp_from_bbr rejects non-basic model types, e.g. bootstrap runs", {
  skip_if_not_installed("bbr")

  mod <- make_bbr_fixture()
  boot <- bbr::new_bootstrap_run(mod)

  expect_error(xp_from_bbr(boot), regexp = "bbi_nonmem_model")
})

test_that("xp_from_bbr errors clearly when the model has not finished running", {
  skip_if_not_installed("bbr")

  mod <- make_bbr_fixture(finished = FALSE)

  expect_error(xp_from_bbr(mod), regexp = "not finished running")
})
