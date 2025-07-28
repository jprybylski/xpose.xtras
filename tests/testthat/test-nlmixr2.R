test_that("nlmixr2 is compatible", {
  expect_no_error(
    as_xp_xtras(xpdb_nlmixr2)
  )
  expect_no_error(
    as_xp_xtras(xpdb_nlmixr2_saem)
  )

  # Make sure properties can be found and manipulated
  # Including: get_prop, set_prop, backfill and options functions


  # Make sure new single xpdb functions can be run without error


  # Make sure xpose_sets can be made (several iterations)


  # Make sure model comparison plots can be created


  # Make sure model-averaging plots can be created


  # Make sure get_prm and prm associations work


})
