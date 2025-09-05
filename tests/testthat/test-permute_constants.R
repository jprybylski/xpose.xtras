library(testthat)
library(xpose.xtras)


test_that("permute_constants produces all permutations and recalculates pre-exponentials", {
  skip_if_not_installed("rxode2")
  df <- tibble::tibble(KA = 1, ALPHA = 0.5, BETA = 0.1)
  res <- permute_constants(df)
  expect_equal(nrow(res), factorial(3))
  expect_true(all(c("permutation", "KA", "ALPHA", "BETA", "A", "B") %in% names(res)))
  first <- dplyr::filter(res, permutation == 1)
  expect_equal(first$A, 2.25)
  expect_equal(first$B, -1.25)
})
