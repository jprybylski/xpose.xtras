test_that("shark_plot works as expected", {
  expect_error( # no ofv
    pheno_set %>%
      shark_plot(run6,run11),
    "OFV.*required.*no.*auto_backfill"
  )
  expect_no_error(
    pheno_set %>%
      focus_qapply(backfill_iofv) %>%
      shark_plot(run6,run11,quiet = TRUE)
  )
  expect_warning(
    xpdb_set[1:2] %>% focus_qapply(backfill_iofv) %>% shark_plot(quiet=TRUE,df=1),
    "All.*probably.*same.*model"
  )
  expect_warning(
    xpose_set(pheno_base,pheno_final) %>% focus_qapply(backfill_iofv) %>% shark_plot(quiet=TRUE),
    "Guessing df.*0.*Using.*1.*df.*alpha"
  )


  two_mod_set <- xpose_set(naive=pheno_set[[1]]$xpdb, pheno_base) %>% focus_qapply(backfill_iofv)

  # df checks
  expect_warning(
    shark_plot(two_mod_set,quiet = TRUE,df="no numeric"),
    "df.*guess.*or.*number.*Using.*1"
  )
  expect_warning(
    shark_plot(two_mod_set,quiet = TRUE,df=-3.5),
    "df.*LRT.*greater.*0.*Using.*1"
  )


  # nind checks
  ten_percent <- shark_plot(two_mod_set,quiet = TRUE, text_cutoff = 0.1)
  expect_lte(
    length(unique(ten_percent$layers[[5]]$data$ID)), # 5 and 6 are the (expected default) text layers
    0.9*as.numeric(get_prop(pheno_base, "nind"))+1
  )
  expect_lte(
    length(unique(ten_percent$layers[[6]]$data$ID)),
    0.9*as.numeric(get_prop(pheno_base, "nind"))+1
  )
  for (reps in 1:3) {
    randprop <- runif(1)
    rand_percent <-shark_plot(two_mod_set,quiet = TRUE, text_cutoff = randprop)
    expect_lte(
      length(unique(rand_percent$layers[[5]]$data$ID)),
      (1-randprop)*as.numeric(get_prop(pheno_base, "nind"))+1
    )
    expect_lte(
      length(unique(rand_percent$layers[[6]]$data$ID)),
      (1-randprop)*as.numeric(get_prop(pheno_base, "nind"))+1
    )
  }
  ten_total <- shark_plot(two_mod_set,quiet = TRUE, text_cutoff = 10)
  expect_lte(
    length(unique(ten_total$layers[[5]]$data$ID)),
    10
  )
  expect_lte(
    length(unique(ten_total$layers[[6]]$data$ID)),
    10
  )
  for (reps in 1:3) {
    rannum <- sample(as.numeric(get_prop(pheno_base, "nind")),1)
    rand_total <- shark_plot(two_mod_set,quiet = TRUE, text_cutoff = rannum)
    expect_lte(
      length(unique(rand_total$layers[[5]]$data$ID)),
      rannum
    )
    expect_lte(
      length(unique(rand_total$layers[[5]]$data$ID)),
      rannum
    )
  }



  # type testing
  # wrapper function to get geom type
  geoms_lists <- function(gg) purrr::map_chr(gg$layers, ~class(.x$geom)[1])

  expect_setequal(
    geoms_lists(shark_plot(two_mod_set,quiet = TRUE)),
    c("GeomPoint","GeomText","GeomHline")
  )
  expect_setequal(
    geoms_lists(shark_plot(two_mod_set,quiet = TRUE,type="p")),
    c("GeomPoint")
  )
  expect_setequal(
    geoms_lists(shark_plot(two_mod_set,quiet = TRUE,type="pl")),
    c("GeomPoint","GeomHline")
  )
  expect_setequal(
    geoms_lists(shark_plot(two_mod_set,quiet = TRUE,type="tl")),
    c("GeomText","GeomHline")
  )
  expect_setequal(
    geoms_lists(shark_plot(two_mod_set,quiet = TRUE,type="tp")),
    c("GeomPoint","GeomText")
  )

  # alias
  expect_identical(
    shark_plot(two_mod_set,quiet = TRUE),
    dofv_vs_id(two_mod_set,quiet = TRUE)
  )

  # facets
  expect_error(
    shark_plot(two_mod_set,quiet = TRUE, facets = APGR),
    ".*Facets.*simple"
  )
  expect_no_error(
    shark_plot(two_mod_set, facets = "APGR", quiet = TRUE)
  )
  expect_in(
    "APGR",
    shark_plot(two_mod_set, facets = "APGR", quiet = TRUE) %>%
      {.$facet$params$facets} %>%
      names()
  )
})

test_that("shark colors can change", {
  two_random_colors <- sample( # https://stackoverflow.com/a/33144808
    grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)],
    2
  )

  new_color <- xpose_set(pheno_base, pheno_final) %>%
    # forward functions affecting xpdb objects
    focus_xpdb(everything()) %>%
    # Add iOFVs
    focus_function(backfill_iofv) %>%
    # Change color of all xpdb xp_themes (though only the first one needs to change)
    focus_function(
      function(x) shark_colors(
        x,
        upcolor = two_random_colors[1],
        dncolor = two_random_colors[2]
      )) %>%
    unfocus_xpdb() %>%
    shark_plot(df=1, quiet = TRUE)
  # colors apply to (expect default) point layers
  expect_equal(
    new_color$layers[[3]]$aes_params$colour,
    two_random_colors[1]
  )
  expect_equal(
    new_color$layers[[4]]$aes_params$colour,
    two_random_colors[2]
  )
  # colors apply to (expect default) text layers
  expect_equal(
    new_color$layers[[5]]$aes_params$colour,
    two_random_colors[1]
  )
  expect_equal(
    new_color$layers[[6]]$aes_params$colour,
    two_random_colors[2]
  )
})
