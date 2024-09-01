test_that("xp_xtra class can be set", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # example data passes check
  expect_true(check_xpdb_x(xpdb_x))

  expect_false(
    is_xp_xtras(xpdb_ex_pk)
  )
  expect_true(
    is_xp_xtras(as_xpdb_x(xpdb_ex_pk))
  )

  expect_s3_class(
    as_xpdb_x(xpdb_ex_pk),
    "xp_xtras"
  )
  expect_identical(
    as_xpdb_x(xpdb_ex_pk),
    as_xpdb_x(as_xpdb_x(xpdb_ex_pk))
  )

  expect_message(check_xpdb_x(xpose::xpdb_ex_pk),
                 regexp = "xpose_data")

  # edge case where an xp_xtras object loses its class due to cross-compatibility
  secret_xp_xtra <- xpose::set_var_units(as_xpdb_x(xpdb_ex_pk), AGE="yr")
  expect_false(is_xp_xtras(secret_xp_xtra)) # "stric"
  expect_true(check_xpdb_x(secret_xp_xtra)) # compatibility checker
  expect_no_message(check_xpdb_x(secret_xp_xtra))

  # Test alias
  expect_identical(
    check_xp_xtras(secret_xp_xtra),
    check_xpdb_x(secret_xp_xtra)
  )

  # other trivial checks
  expect_false(check_xpdb_x(c()))
  xpose_themed <- as_xpdb_x(xpdb_ex_pk)
  xpose_themed$xp_theme <- xpose::theme_xp_default()
  expect_false(is_xp_xtras(xpose_themed)) # invalid test_coverage

})

test_that("set_var_types is class-dependent", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_gte(length(methods(set_var_types)),2)

  xpdb__ex_pk2 <- as_xpdb_x(xpdb_ex_pk)
  expect_failure(expect_identical(
    set_var_types(xpdb_ex_pk),
    set_var_types(xpdb__ex_pk2)
  ))

})

test_that("levels can be set for categories", {
  data("xpdb_ex_pk", package = "xpose", envir = environment())

  expect_message(try(set_var_levels(xpdb_ex_pk), silent = TRUE), regexp="xpose_data")
  suppressMessages(expect_error(set_var_levels(xpdb_ex_pk), regexp="xp_xtras object required"))
  expect_no_error(set_var_levels(xpdb_x))

  # Check level processor (with any formula list)
  expect_s3_class(
    proc_levels(c(1~"s",2~"d")),
    "data.frame"
  )
  expect_error(
    proc_levels(c(a~b)),
    "vectors"
  )
  expect_error(
    proc_levels(c(1~0,2~"foo")),
    "combine"
  )

  # Check level checker
  lvl_list <- list(
    MED1 = c(
      0 ~ "nope",
      1 ~ "yeah"
    ))
  x_indx <- get_index(xpdb_ex_pk)
  expect_no_error(check_levels(lvl_list,x_indx))
  expect_error(
    check_levels(list(
      MED1 = 1~"y"
    ),
    x_indx),
    "not a formula list"
  )
  expect_error(
    check_levels(list(
      fake_data = c(1~"y")
    ),
    x_indx),
    "not in data"
  )
  expect_error(
    check_levels(list(
      MED1 = "Not a formula"
    ),
    x_indx),
    "neither"
  )
  expect_error(
    check_levels(list(
      MED1 = lvl_bin(),
      MED1 = c(1 ~ "overwrite 1")
    ),
    x_indx),
    "must be formula lists"
  )
  expect_no_error(
    check_levels(list(
      MED1 = c(0 ~ "for 0"),
      MED1 = c(1 ~ "for 1")
    ),
    x_indx)
  )
  expect_warning(
    check_levels(list(
      AGE = c(45 ~ "median")
    ),
    x_indx),
    "not compatible with levels"
  )


  # Check levelers
  expect_true(is_leveler(lvl_bin()))
  expect_false(is_leveler(c(0~"n",1~"y")))
  expect_false(is_leveler(c("No","Yes")))
  expect_true(is_leveler(as_leveler(c("n","y"))))
  expect_error(lvl_bin(c("nope","yep","perhaps")), "binary variables")
  expect_no_error(lvl_bin(c("No conmeds", "Conmeds")))
  expect_no_error(lvl_bin(c("Fed", "Unsure fed"), .start_index = 1))
  expect_setequal(lvl_bin(), c("No","Yes"))
  expect_setequal(lvl_bin(c("n","y")), c("n","y"))
  expect_setequal(lvl_sex(), c("Male","Female"))
  expect_equal(attr(lvl_sex(), "start"), 1)
  expect_setequal(lvl_inord(letters), letters)

  # Check set_var_levels
  expect_error(set_var_levels(xpdb_x, .problem = 3), "3 not valid")
  expect_error(set_var_levels(xpdb_x, .handle_missing = "not an option abc"), "not an option abc")
  expect_no_error(
    set_var_levels(xpdb_x, MED1 = lvl_bin(), SEX = c(1~"Male", 2~"Female"))
  )
  expect_no_warning(
    set_var_levels(xpdb_x, SEX = c(1~"Male", 2~"Female",3~"Not provided"))
  )
  expect_no_warning(
    set_var_levels(xpdb_x, SEX = c(1~"Male", 2~"Female",3~"Not provided"), .handle_missing = "quiet")
  )
  expect_warning(
    set_var_levels(xpdb_x, SEX = c(1~"Male", 2~"Female",3~"Not provided"), .handle_missing = "warn"),
    "SEX.*3"
  )
  expect_error(
    set_var_levels(xpdb_x, SEX = c(1~"Male", 2~"Female",3~"Not provided"), .handle_missing = "error"),
    "SEX.*3"
  )
  expect_no_warning(
    set_var_levels(xpdb_x, SEX = c(1~"Any"))
  )
  expect_no_warning(
    set_var_levels(xpdb_x, SEX = c(1~"Any"), .handle_missing = "quiet")
  )
  expect_warning(
    set_var_levels(xpdb_x, SEX = c(1~"Any"), .handle_missing = "warn"),
    "SEX values.*missing"
  )
  expect_error(
    set_var_levels(xpdb_x, SEX = c(1~"Any"), .handle_missing = "error"),
    "SEX values.*missing"
  )

  expect_error(
    set_var_levels(xpdb_x, SEX = c(1~"m",2~2)),
    "LHS.*numeric.*RHS.*quoted strings"
  )
  expect_error(
    set_var_levels(xpdb_x, SEX = c("1"~"m")),
    "LHS.*numeric.*RHS.*quoted strings"
  )

  test_leveled <- set_var_levels(xpdb_x, MED1=lvl_bin(), SEX=lvl_sex())
  expect_identical(
    get_index(test_leveled,1) %>% filter(col=="SEX") %>% pull(levels) %>% .[[1]],
    proc_levels(c(1~"Male",2~"Female"))
  )
  expect_identical(
    get_index(test_leveled,1) %>% filter(col=="MED1") %>% pull(levels) %>% .[[1]],
    proc_levels(c(0~"No",1~"Yes"))
  )
  expect_equal(
    nrow(get_index(test_leveled,1) %>% filter(col=="MED2") %>% pull(levels) %>% .[[1]]),
    0
  )


})
