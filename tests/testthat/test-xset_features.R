

test_that("base model can be set, unset and gotten", {
  expect_error(
    pheno_set %>%
      set_base_model(run6,run3),
    "only.*one.*base"
  )
  expect_message(
    pheno_set %>%
      set_base_model(run6) %>%
      set_base_model(run3),
    "Base.*already set.*Overwrit"
  )
  expect_message(
    pheno_set %>%
      print(),
    "Base.*none"
  )
  expect_message(
    pheno_set %>%
      set_base_model(run3) %>%
      print(),
    "Base.*run3"
  )
  expect_equal(
    pheno_set %>%
      set_base_model(run6) %>%
      get_base_model(),
    "run6"
  )
  expect_null(
    pheno_set %>%
      get_base_model()
  )
  expect_null(
    pheno_set %>%
      set_base_model(run6) %>%
      unset_base_model() %>%
      get_base_model()
  )
})

test_that("diff produces expected, convenient results", {
  random_subset <- pheno_set[sample(length(pheno_set),5)] %>%
    # extract xpdbs
    purrr::map(~.x$xpdb) %>%
    # create new set with ordered lineage
    {xpose_set(!!!., .as_ordered = TRUE)}
  subset_ofvs <- random_subset %>%
    purrr::map_dbl(~as.numeric(get_prop(.x$xpdb, "ofv"))) %>%
    unname()


  expect_identical(
    diff(random_subset),
    diff(subset_ofvs)
  )

  expect_true(
    diff(pheno_set,run3,run6) %>%
      is.list()
  )
})


test_that("set lineages can be determined", {
  random_subset <- pheno_set[sample(length(pheno_set),5)] %>%
    # extract xpdbs
    purrr::map(~.x$xpdb) %>%
    # create new set with ordered lineage
    {xpose_set(!!!., .as_ordered = TRUE)}
  not_in_subset <- pheno_set[!names(pheno_set) %in% names(random_subset)]

  expect_identical(
    names(random_subset),
    xset_lineage(random_subset)
  )
  expect_identical(
    random_subset %>%
      select(2:last_col()) %>%
      names(),
    random_subset %>%
      select(2:last_col()) %>%
      xset_lineage()
  )
  base_num <- sample(length(random_subset),1)
  expect_equal(
    random_subset %>%
      set_base_model(all_of(base_num)) %>%
      xset_lineage() %>%
      .[1],
    names(random_subset)[base_num]
  )

  # ignore recursion
  expect_no_error(
    random_subset %>%
      add_relationship(stats::as.formula(
        paste(
          names(random_subset)[c(1,length(random_subset))],
          collapse = "~"
          )
      )) %>%
      xset_lineage()
  )

  ## longest lineage is detected
  # another child on penultimate xpdb is ignored (first longest in tie wins)
  random_subset2 <- random_subset %>%
    add_xpdb(
      new_one = not_in_subset[[sample(length(not_in_subset),1)]]$xpdb,
      .relationships = stats::as.formula(
      paste0(
        "new_one",
        "~",
        names(random_subset)[c(length(random_subset)-1)]
      )
    ))
  expect_identical(
    xset_lineage(random_subset),
    xset_lineage(random_subset2)
  )
  # longest child is still found, even if older
  add_branch <- sample(length(not_in_subset),2)
  random_subset3 <- random_subset %>%
    add_xpdb(
      new_one = not_in_subset[[add_branch[1]]]$xpdb,
      new_one_child = not_in_subset[[add_branch[2]]]$xpdb,
      .relationships = c(
        stats::as.formula(
          paste0(
            "new_one",
            "~",
            names(random_subset)[c(length(random_subset)-1)]
          )
        ),
        new_one_child ~ new_one
      ))
  expect_identical(
    xset_lineage(random_subset3),
    c(head(xset_lineage(random_subset),-1),"new_one","new_one_child")
  )
})

test_that("parentage operator works", {
  random_subset <- pheno_set[sample(length(pheno_set),5)] %>%
    # extract xpdbs
    purrr::map(~.x$xpdb) %>%
    # create new set with ordered lineage
    {xpose_set(!!!., .as_ordered = TRUE)}

  expect_true(
    random_subset[[1]] %p% random_subset[[2]]
  )

  expect_false(
    random_subset[[4]] %p% random_subset[[2]]
  )
  little_set <- xpose_set(pheno_base,pheno_saem)
  coin_toss <- sample(2,1)
  expect_false(
    little_set[[coin_toss]] %p% random_subset[[3-coin_toss]]
  )
})


# spinner test (this doesn't really work)
cli::test_that_cli("test ellipsis", {
  skip_on_cran() # because of dirty dots in output
  skip_on_covr()
  local_reproducible_output()
  with_mocked_bindings({
    expect_snapshot(
      xset_lineage(pheno_set)
    )
  },
  is_interactive = function(...) TRUE,
  .package = "xpose.xtras")
})
