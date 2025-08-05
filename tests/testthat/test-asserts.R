test_that("assert wrappers work as expected", {
  expect_no_error(
    xpa("true", TRUE)
  )
  expect_identical(
    xpa("true", TRUE),
    TRUE
  )
  expect_no_error(
    xpa("count", c(1))
  )
  expect_identical(
    xpa("count", c(1)),
    1
  )
  expect_no_error(
    xpa("character", letters)
  )
  expect_identical(
    xpa("character", letters),
    letters
  )


  expect_error(
    xpa("true", FALSE)
  )
  expect_error(
    xpa("count", c(1:3))
  )
  expect_error(
    xpa("character", 1)
  )

  expect_error(
    xpa("true", FALSE),
    regexp = "Error evaluating"
  )
  rangom_string <- paste(sample(letters,8), collapse="")
  expect_error(
    xpa("true", FALSE, custom_msg = rangom_string),
    regexp = rangom_string
  )
  expect_error(
    xpa("true", FALSE, custom_msg = letters),
    regexp = "length"
  )

})
