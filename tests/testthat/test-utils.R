test_that("get_* functions work", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_error(get_prop(xpdb_ex_pk, c("descr", "etashk")), regexp = "one property")
  expect_error(get_prop(xpdb_ex_pk, "fakeprop"), regexp = "fakeprop")

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

test_that("set_* functions works", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_error(set_prop(xpdb_ex_pk, repeat_name="", repeat_name=""), regexp = "have unique")
  expect_error(set_prop(xpdb_ex_pk, fake_prop=""), regexp = "fake_prop")
  expect_error(set_prop(xpdb_ex_pk, descr=c("multiple","values")), regexp = "set to one value")

  rand_desc <- paste(sample(letters, 5), collapse="")
  new_desc <- set_prop(xpdb_ex_pk, descr = rand_desc)
  expect_equal(
    get_prop(new_desc, "descr"), rand_desc
  )

  rand_desc <- paste(sample(letters, 5), collapse="")
  new_desc <- set_prop(xpdb_ex_pk, descr = rand_desc, .problem = 1)
  expect_failure(expect_equal(
    get_prop(new_desc, "descr"), rand_desc
  ))


  expect_failure(expect_equal(
    get_prop(xpdb_ex_pk, "label", .problem = 1),
    get_prop(xpdb_ex_pk, "label", .problem = 2)
  ))
  rand_label <- paste(sample(letters, 5), collapse="")
  new_label <- set_prop(xpdb_ex_pk, label = rand_label)
  expect_equal(
    get_prop(new_label, "label", .problem = 1),
    get_prop(new_label, "label", .problem = 2)
  )
  expect_equal(
    get_prop(new_label, "label", .problem = 1),
    rand_label
  )
  expect_equal(
    get_prop(new_label, "label", .problem = 2),
    rand_label
  )


  rand_label <- paste(sample(letters, 5), collapse="")
  new_label <- set_prop(xpdb_ex_pk, label = rand_label, .problem = 1)
  expect_failure(expect_equal(
    get_prop(new_label, "label", .problem = 1),
    get_prop(new_label, "label", .problem = 2)
  ))
  expect_equal(
    get_prop(new_label, "label", .problem = 1),
    rand_label
  )
  expect_failure(expect_equal(
    get_prop(new_label, "label", .problem = 2),
    rand_label
  ))

})

test_that("get-set index works", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_error(
    get_index(c())
  )
  expect_error(
    get_index(xpdb_ex_pk, NULL, sddd=1),
    regexp = "sddd"
  )

  expect_s3_class(
    get_index(xpdb_ex_pk),
    "data.frame"
  )

  expect_setequal(
    xpdb_ex_pk$data$problem,
    get_index(xpdb_ex_pk)$problem
  )
  expect_setequal(
    1,
    get_index(xpdb_ex_pk, .problem = 1)$problem
  )
  expect_setequal(
    2,
    get_index(xpdb_ex_pk, .problem = 2)$problem
  )


  expect_failure(expect_identical(
    get_index(xpdb_ex_pk),
    get_index(set_index(xpdb_ex_pk, get_index(xpdb_ex_pk)))
  ))
  expect_s3_class(
    set_index(xpdb_ex_pk, get_index(xpdb_ex_pk)),
    "xp_xtras"
  )
  expect_identical(
    get_index(as_xpdb_x(xpdb_ex_pk)),
    get_index(set_index(xpdb_ex_pk, get_index(xpdb_ex_pk)))
  )


})

test_that("convenience functions return expected", {
  expect_false(
    is_formula_list(list())
  )
  expect_false(
    is_formula_list(a~b)
  )
  expect_true(
    is_formula_list(c(a~b))
  )
  expect_true(
    is_formula_list(list(a~b))
  )
})
