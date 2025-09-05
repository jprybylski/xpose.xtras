library(testthat)
library(xpose.xtras)


test_that("permute_constants produces all permutations and recalculates pre-exponentials", {
  skip_if_not_installed("rxode2")
  df <- tibble::tibble(KA = 1, LAMBDA1 = 0.5, LAMBDA2 = 0.1)
  res <- permute_constants(df)
  expect_equal(nrow(res), factorial(3))
  expect_true(all(c("perm", "KA", "LAMBDA1", "LAMBDA2", "A1", "A2") %in% names(res)))
  first <- dplyr::filter(res, perm == 1)
  expect_equal(first$A1, 2.25)
  expect_equal(first$A2, -1.25)
})
