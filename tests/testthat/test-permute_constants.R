library(testthat)
library(xpose.xtras)


test_that("permute_constants produces all permutations and recalculates pre-exponentials", {
  skip_if_not_installed("rxode2")
  df <- tibble::tibble(KA = 1, ALPHA = 0.5, BETA = 0.1)
  res <- permute_constants(df)
  expect_equal(nrow(res), factorial(3))
  expect_true(all(c("permutation", "KA", "ALPHA", "BETA", "A", "B") %in% names(res)))
  first <- dplyr::filter(res, permutation == 1)
  expect_equal(dplyr::select(first, KA, ALPHA, BETA), df)
  expect_equal(first$A, 4.5)
  expect_equal(first$B, -1.3888889, tolerance = 1e-7)
})

test_that("permute_constants allows custom naming of rates and pre-exponentials", {
  skip_if_not_installed("rxode2")
  df <- tibble::tibble(KABS = 1, LAMBDA1 = 0.5, LAMBDA2 = 0.1)
  res <- permute_constants(df, ka_col = "KABS", exp_vars = "lambda", pre_vars = c("P", "Q"))
  expect_equal(nrow(res), factorial(3))
  expect_true(all(c("permutation", "KABS", "LAMBDA1", "LAMBDA2", "P", "Q") %in% names(res)))
})

test_that("permute_constants uppercases rxDerived names and overwrites", {
  skip_if_not_installed("rxode2")
  df <- tibble::tibble(KA = 1, ALPHA = 0.5, BETA = 0.1, k12 = 0)
  res <- permute_constants(df)
  expect_true("K12" %in% names(res))
  expect_false("k12" %in% names(res))
  expect_false(any(res$K12 == 0))
})

test_that("permute_constants can restrict permutations to KA swaps", {
  skip_if_not_installed("rxode2")
  df <- tibble::tibble(KA = 1, ALPHA = 0.5, BETA = 0.1)
  res <- permute_constants(df, swap_ka = TRUE)
  expect_equal(nrow(res), 3)
  expect_equal(sort(unique(res$permutation)), 1:3)
  first <- dplyr::filter(res, permutation == 1)
  expect_equal(dplyr::select(first, KA, ALPHA, BETA), df)
})

test_that("one-compartment permutations retain positive volume", {
  skip_if_not_installed("rxode2")
  df <- tibble::tibble(KA = 1, ALPHA = 0.5)
  res <- permute_constants(df, swap_ka = TRUE)
  expect_true(all(res$V > 0))
})

test_that("VC and V are identical in the original permutation", {
  skip_if_not_installed("rxode2")
  df <- tibble::tibble(KA = 1, ALPHA = 0.5, BETA = 0.1, VC = 10, V = 10)
  res <- permute_constants(df)
  first <- dplyr::filter(res, permutation == 1)
  expect_equal(first$VC, first$V)
})
