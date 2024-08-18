# Ensure xpdb_set is the one with the package
test_env(package = "xpose.xtras")


test_that("check criteria catches trivial cases", {

  expect_error(check_xpose_set(NULL))
  expect_error(check_xpose_set_item(NULL))

  obj <- xpdb_set[1]
  names(obj) <- "fakename"
  expect_error(check_xpose_set(obj))

  obj <- xpdb_set[1]
  obj[[1]]$parent <- NULL
  expect_error(check_xpose_set(obj))

  obj <- xpdb_set[1:2]
  obj[[1]]$label <- "fakename"
  obj[[2]]$label <- "fakename"
  expect_error(check_xpose_set(obj))

  obj <- xpdb_set[3:4] # missing parents
  expect_message(check_xpose_set(obj))

  obj <- xpdb_set[1]
  obj[[1]]$xpdb <- "fake"
  expect_error(check_xpose_set(obj))


})

test_that("built-in data satisfies check criteria", {

  expect_true(check_xpose_set(xpdb_set))

  expect_no_message(check_xpose_set(xpdb_set))

})

test_that("xpose_set() assembly works", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # Arbitrary copies
  xx1 <- xx2 <- xpdb_ex_pk2 <- xpdb_ex_pk3 <- xpdb_ex_pk4 <- xpdb_ex_pk

  # Basic assembly
  expect_no_message(xpose_set(xpdb_ex_pk, xpdb_ex_pk2))
  expect_error(xpose_set(xpdb_ex_pk, xpdb_ex_pk))
  expect_error(xpose_set(a=xpdb_ex_pk, a=xpdb_ex_pk))
  ulist <- list(xpdb_ex_pk, xpdb_ex_pk2, xpdb_ex_pk3, xpdb_ex_pk4)
  expect_error(xpose_set(!!!ulist)) # special case of homonyms (coverage test incorrect)

  expect_error(xpose_set(not_xpdb = TRUE, also_not = "foo"))
  expect_error(xpose_set())

  object_named <- xpose_set(xx1, xx2)
  arg_named <- xpose_set(a=xpdb_ex_pk, b=xpdb_ex_pk2)
  expect_identical(names(object_named), c("xx1", "xx2"))
  expect_identical(names(arg_named), c("a", "b"))

  expect_no_message(check_xpose_set(object_named))
  expect_no_message(check_xpose_set(arg_named))
  expect_true(check_xpose_set(object_named))

  expect_length(xpose_set(xpdb_ex_pk, xpdb_ex_pk2), 2)
  expect_length(xpose_set(xpdb_ex_pk, xpdb_ex_pk2, xpdb_ex_pk3), 3)

})


test_that("xpose_set() relationships works", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # Arbitrary copies
  xx1 <- xx2 <- xpdb_ex_pk2 <- xpdb_ex_pk3 <- xpdb_ex_pk4 <- xpdb_ex_pk

  # Relationship
  expect_warning(xpose_set(xx1, xx2, .relationships = xx2~xx1, .as_ordered = TRUE))
  expect_warning(xpose_set(xx1, xx2, .relationships = "parent"))
  expect_warning(xpose_set(xx1, xx2, .as_ordered = "yes"))
  expect_identical(
    xpose_set(xx1, xx2, .relationships = xx2~xx1),
    xpose_set(xx1, xx2, .as_ordered = TRUE)
  )
  expect_identical(
    xpose_set(xx1, xx2, .relationships = xx2~xx1),
    xpose_set(xx1, xx2, .relationships = list(xx2~xx1))
  )
  expect_equal(total_relationships(xpose_set(xx1, xx2, .as_ordered = TRUE)), 1)
  expect_equal(total_relationships(xpose_set(xx1, xx2,aa=xx1, bb=xx2, .as_ordered = TRUE)), 3)


})

test_that("relationship companion functions work", {


  # Additional tests of add_relationships (direct)
  curr_rels <- total_relationships(xpdb_set)
  expect_equal(total_relationships(
    add_relationship(xpdb_set, fix2~mod1+mod2,fix1~mod1)
  ), curr_rels+3)
  expect_warning(add_relationship(xpdb_set, list(fix2~mod1+mod2),list(fix1~mod1)))
  expect_identical(xpdb_set, add_relationship(xpdb_set))


  # Relationship check
  expect_error(check_relationships(NULL, xpdb_set))
  expect_error(check_relationships(list("hey"), xpdb_set))
  expect_message(check_relationships(list(mod1~aaa), xpdb_set))
  expect_message(check_relationships(list(aaa~mod1), xpdb_set))

})


test_that("xpose_set generating functions work", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  set <- add_xpdb(xpdb_set, xpdb_ex_pk)
  expect_equal(length(set), length(xpdb_set)+1)
  expect_contains(names(set), "xpdb_ex_pk")

  set <- add_xpdb(xpdb_set, aaa=xpdb_ex_pk, bbb=xpdb_ex_pk)
  expect_equal(length(set), length(xpdb_set)+2)
  expect_contains(names(set), "aaa")
  expect_contains(names(set), "bbb")

  expect_error(c(xpdb_set, xpdb_set))

  set <- xpose_set(aaa=xpdb_ex_pk, bbb=xpdb_ex_pk)
  expect_identical(
    add_xpdb(xpdb_set, aaa=xpdb_ex_pk, bbb=xpdb_ex_pk),
    c(xpdb_set, set)
  )

  xpdb_set %>%
    add_xpdb(aaa=xpdb_ex_pk, .relationships = aaa~mod1) %>%
    total_relationships() %>%
    expect_equal(total_relationships(xpdb_set)+1)

})

test_that("reshaping works for xpose_set", {
  expect_identical(
    xpdb_set %>% reshape_set() %>% unreshape_set(),
    xpdb_set
  )

  expect_s3_class(xpdb_set %>% reshape_set(), "tbl")

  expect_error(unreshape_set(NULL))
  expect_error(unreshape_set(dplyr::tibble(label=c("a","a","b"))))


})

test_that("properties can be exposed", {
  expect_error(expose_property(NULL))
  expect_error(expose_property(xpdb_set, nonsense))

  expose_property(xpdb_set, ofv) %>%
    .[[1]] %>%
    names() %>%
    expect_contains("..ofv")

  expose_property(xpdb_set, ofv, descr) %>%
    .[[1]] %>%
    names() %>%
    expect_contains(c("..ofv", "..descr"))

  testdf <- expose_property(xpdb_set, ofv, descr, term) %>%
    reshape_set()
  expect_type(testdf$..ofv, "double")
  expect_type(testdf$..descr, "character")
  expect_type(testdf$..term, "character")

  shks <- expose_property(xpdb_set, etashk, epsshk)
  expect_identical(
    shks[[1]]$..etashk,
    get_shk(xpdb_set[[1]]$xpdb)
  )
  expect_identical(
    shks[[1]]$..epsshk,
    get_shk(xpdb_set[[1]]$xpdb, wh="eps")
  )


})


test_that("methods work", {
  # c() tested in xpose_set generating functions
  data("xpdb_ex_pk", package = "xpose", envir = environment())
  big_set <- purrr::map(1:10, ~xpdb_ex_pk) %>% setNames(letters[1:10]) %>% {xpose_set(!!!.)}
  exp_set <- expose_property(xpdb_set, ofv, descr)

  expect_message(print(xpdb_set))
  expect_message(print(big_set), regexp = "truncated")
  suppressMessages(expect_no_message(print(xpdb_set), message = "truncated"))
  expect_message(print(exp_set), regexp = "(ofv|descr)")
  suppressMessages(expect_no_message(print(xpdb_set), message = "(ofv|descr)"))
  expect_message(print(xpdb_set[[1]]))


  expect_identical(xpdb_set, xpdb_set[])
  expect_identical(xpdb_set, xpdb_set[seq_along(xpdb_set)])

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # duplicate copies
  xpose_set(a=xpdb_ex_pk, b=xpdb_ex_pk, c=xpdb_ex_pk) %>%
    duplicated() %>%
    tail(-1) %>%
    all() %>%
    expect_true()
  xpose_set(a=xpdb_ex_pk, b=xpdb_ex_pk, c=xpose::filter(xpdb_ex_pk, TIME>2)) %>%
    duplicated() %>%
    expect_setequal(c(FALSE, TRUE, FALSE))

  expect_error(mutate(xpdb_set, label="bad"))
  expect_no_message(mutate(xpdb_set, label=sample(letters, length(xpdb_set)), .force = TRUE))
  expect_message(mutate(xpdb_set, label=sample(letters, length(xpdb_set)), .force = TRUE, .retest = TRUE))
  xpdb_set %>%
    mutate(foo="good") %>%
    .[[1]] %>%
    names() %>%
    expect_contains("foo")
  xpdb_set %>%
    mutate(foo="good") %>%
    .[[sample(length(xpdb_set), 1)]] %>%
    .[["foo"]] %>%
    expect_equal("good")
  xpdb_set %>%
    mutate(n_parents=length(parent)) %>%
    .[[1]] %>%
    .[["n_parents"]] %>%
    expect_equal(4)
  xpdb_set %>%
    mutate(n_parents=length(parent), .rowwise = TRUE) %>%
    .[[1]] %>%
    .[["n_parents"]] %>%
    expect_equal(1)
})
