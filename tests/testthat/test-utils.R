test_that("get_* functions work", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_error(get_prop(xpdb_ex_pk, c("descr", "etashk")))
  expect_error(get_prop(xpdb_ex_pk, "fakeprop"))

  expect_identical(
    get_prop(xpdb_ex_pk, "descr"),
    xpdb_ex_pk %>% xpose::get_summary() %>% dplyr::filter(label=="descr") %>% dplyr::pull(value)
  )
  # backwards approach to ensure shrinkage is same format in current version of xpose
  expect_identical(
    get_shk(xpdb_ex_pk) %>% sprintf("%s [%i]", ., seq_along(.)) %>% paste(collapse=", "),
    get_prop(xpdb_ex_pk, "etashk")
  )
  expect_identical(
    get_shk(xpdb_ex_pk, wh="eps") %>% sprintf("%s [%i]", ., seq_along(.)) %>% paste(collapse=", "),
    get_prop(xpdb_ex_pk, "epsshk")
  )

})
