test_that("multiplication works", {
  two_mod_set <- xpose_set(pheno_base,pheno_final)
  expect_error(
    xset_waterfall(two_mod_set),
    "select.*at least.*one.*column"
  )
  expect_no_error(
    xset_waterfall(two_mod_set,.cols = "ETA1", quiet = TRUE)
  )

  with_NAs <- two_mod_set %>%
    focus_xpdb(everything()) %>%
    focus_function(mutate_x, ETA1=ifelse(ID%in%sample(ID,5),NA,ETA1)) %>%
    unfocus_xpdb()
  expect_warning(
    xset_waterfall(with_NAs, .cols = ETA1, quiet = TRUE),
    "NA values.*will be.*removed"
  )

  expect_warning(
    xpdb_set[1:2] %>% xset_waterfall(.cols = CL, quiet=TRUE, max_nind = 9000),
    "All.*differences.*0.*probably.*same model"
  )

  # nind checks
  ten_percent <- xset_waterfall(two_mod_set,.cols = "ETA1", quiet = TRUE, max_nind = 0.1)
  expect_lte(
    length(unique(ten_percent$data$ID)),
    0.9*as.numeric(get_prop(pheno_base, "nind"))+1
  )
  for (reps in 1:5) {
    randprop <- runif(1)
    rand_percent <- xset_waterfall(two_mod_set,.cols = "ETA1", quiet = TRUE, max_nind = randprop)
    expect_lte(
      length(unique(rand_percent$data$ID)),
      (1-randprop)*as.numeric(get_prop(pheno_base, "nind"))+1
    )
  }
  ten_total <- xset_waterfall(two_mod_set,.cols = "ETA1", quiet = TRUE, max_nind = 10)
  expect_lte(
    length(unique(ten_total$data$ID)),
    10
  )
  for (reps in 1:5) {
    rannum <- sample(as.numeric(get_prop(pheno_base, "nind")),1)
    rand_total <- xset_waterfall(two_mod_set,.cols = "ETA1", quiet = TRUE, max_nind = rannum)
    expect_lte(
      length(unique(rand_total$data$ID)),
      rannum
    )
  }
  expect_error(
    xpdb_set[1:2] %>% xset_waterfall(.cols = CL, quiet=TRUE),
    "max_nind.*all data.*different value"
  )

  # high level test of scaling
  all_scaled <- xset_waterfall(two_mod_set,.cols = "ETA1", quiet = TRUE, max_nind = 9999)
  all_unscaled <- xset_waterfall(two_mod_set,.cols = "ETA1", quiet = TRUE, scale_diff = FALSE, max_nind = 9999)
  expect_equal(
    cor(all_unscaled$data$value,all_scaled$data$value),
    1 # will be perfectly correlated if scaled
  )
  expect_equal(
    all_unscaled$data$value/sd(all_scaled$data$ETA1),
    all_scaled$data$value
  )


  # type testing
  # wrapper function to get geom type
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])

  expect_setequal(
    geoms_lists(xset_waterfall(two_mod_set,.cols = "ETA1", quiet = TRUE)),
    c("GeomBar","GeomHline")
  )
  expect_setequal(
    geoms_lists(xset_waterfall(two_mod_set,.cols = "ETA1", type="bh",quiet = TRUE)),
    c("GeomBar","GeomHline")
  )
  expect_setequal(
    geoms_lists(xset_waterfall(two_mod_set,.cols = "ETA1", type="t", quiet = TRUE)),
    c("GeomText")
  )
  expect_setequal(
    geoms_lists(xset_waterfall(two_mod_set,.cols = "ETA1", type="bt", quiet = TRUE)),
    c("GeomBar","GeomText")
  )

  # facets
  expect_error(
    xset_waterfall(two_mod_set,.cols = "ETA1", facets = APGR),
    ".*Facets.*simple"
  )
  expect_no_error(
    xset_waterfall(two_mod_set,.cols = "ETA1", facets = "APGR", quiet = TRUE)
  )
  expect_in(
    "APGR",
    xset_waterfall(two_mod_set,.cols = "ETA1", facets = "APGR", quiet = TRUE) %>%
      {.$facet$params$facets} %>%
      names()
  )

})

test_that("waterfall_helper is helpful", {
  col_env <- new.env()
  two_mod_set <- xpose_set(pheno_base,pheno_final)
  two_set_dots(two_mod_set, envir = col_env)
  col_env$.problem <- 1

  expect_error(
    waterfall_helper("param","param", col_env),
    "No.*param.*available"
  )
  expect_error(
    waterfall_helper("param","parameter", col_env),
    "No.*parameter.*available"
  )
  expect_error(
    waterfall_helper("param","paRAmeter", col_env),
    "No.*paRAmeter.*available"
  )
  expect_setequal(
    c("mod1","mod2",".problem"),
    ls(envir = col_env, all.names = TRUE)
  )
  expect_no_error(
    waterfall_helper("ipred","ipred", col_env)
  )
  expect_equal(
    col_env$m1col,
    "IPRED"
  )
  waterfall_helper("eta","eta", col_env)
  expect_setequal(
    col_env$m1col,
    xp_var(col_env$mod1$xpdb, .problem = 1, type="eta")$col
  )
  col_env <- new.env()
  two_set_dots(two_mod_set, envir = col_env)
  col_env$.problem <- 1
  col_env$mod1$xpdb <- select(col_env$mod1$xpdb, -ETA1)
  expect_error(
    waterfall_helper("eta","eta", col_env),
    "Eta.*not identical.*ETA1.*model 1"
  )
  col_env <- new.env()
  two_set_dots(two_mod_set, envir = col_env)
  col_env$.problem <- 1
  col_env$mod2$xpdb <- select(col_env$mod2$xpdb, -ETA2)
  expect_error(
    waterfall_helper("eta","eta", col_env),
    "Eta.*not identical.*ETA2.*model 2"
  )
  col_env <- new.env()
  two_set_dots(two_mod_set, envir = col_env)
  col_env$.problem <- 1
  col_env$mod2$xpdb <- select(col_env$mod2$xpdb, -ETA2)
  expect_error(
    waterfall_helper("eta","eTTTa", col_env),
    "eTTTa.*not identical.*ETA2.*model 2"
  )

})
