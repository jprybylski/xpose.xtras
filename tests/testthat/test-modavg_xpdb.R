test_that("model averaging xpdb (modavg_xpdb) works", {

  #selection
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(run3, .lineage = TRUE, avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_error(
    pheno_set %>%
      modavg_xpdb(run3, run6, .lineage = TRUE, avg_cols = DV, auto_backfill = TRUE, quiet=TRUE),
    "list.*lineage.*multiple.*empty.*single.*"
  )
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(run3,run8,run9, avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(run15~run7+run6, avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_no_error(
    pheno_set %>%
      modavg_xpdb(run5:run8, avg_cols = DV, auto_backfill = TRUE, quiet=TRUE)
  )
  expect_error(
    modavg_xpdb(pheno_set, run5:run8, quiet = TRUE, avg_cols = DV, auto_backfill = FALSE),
    "Indiv.*OFV.*required.*Set.*auto_backfill"
  )

  # Test setup for calculations and algorithm
  set.seed(100) # this works for most cases, except really small denominators,
                # so the selected seed is to protect against that trivial case
  backfilled_set <- pheno_set %>% focus_qapply(backfill_iofv)
  ofvs <- c(1,1)
  while (diff(ofvs)==0) {
    random_two <- sample(length(backfilled_set),2)
    subsetted <- backfilled_set[random_two]
    ofvs <- expose_property(subsetted, ofv) %>% dplyr::pull(..ofv)
  }
  datas <- purrr::map(subsetted, ~xpose::get_data(.x$xpdb,quiet = TRUE))
  npars <- purrr::map_int(subsetted,~{get_prm(.x$xpdb,quiet = TRUE) %>% dplyr::pull(fixed) %>% magrittr::not() %>% sum()})
  totalress <- purrr::map_dbl(datas,~{.x %>% dplyr::pull(RES) %>% sum()})
  # Two individuals with one having lower ofv in mod1 and the other having higher
  # test algorithm calcs
  lower_ofvres_id <- NULL
  upper_ofvres_id <- NULL
  while (is.null(lower_ofvres_id) || is.null(upper_ofvres_id)) {
    try_ids <- datas[[1]] %>% .$ID %>%
      unique() %>%
      .[!. %in% c(lower_ofvres_id,upper_ofvres_id)] %>%
      sample(2)
    iofvs1 <- purrr::map_dbl(try_ids,~{
      datas[[1]] %>% filter(ID==.x) %>% dplyr::pull(iOFV) %>% head(1)
    })
    iofvs2 <- purrr::map_dbl(try_ids,~{
      datas[[2]] %>% filter(ID==.x) %>% dplyr::pull(iOFV) %>% head(1)
    })
    # using current res calculation that does not limit to obs only
    res1 <- purrr::map_dbl(try_ids,~{
      datas[[1]] %>% filter(ID==.x) %>% dplyr::pull(RES) %>% sum()
    })
    res2 <- purrr::map_dbl(try_ids,~{
      datas[[2]] %>% filter(ID==.x) %>% dplyr::pull(RES) %>% sum()
    })
    if (iofvs1[1] < iofvs2[1] && res1[1] < res2[1])
      lower_ofvres_id <- try_ids[1]
    if (iofvs1[2] > iofvs2[2] && res1[2] > res2[2])
      upper_ofvres_id <- try_ids[2]
  }
  # confirm msa versus maa (also assumes defaults)
  maa_example <- modavg_xpdb(subsetted,avg_by_type = "ipred") %>% xpose::get_data(quiet = TRUE)
  msa_example <- modavg_xpdb(subsetted,avg_by_type = "ipred",algorithm = "msa") %>% xpose::get_data(quiet = TRUE)
  iofv_weighting_1 <- sum(exp(-0.5*c(iofvs1[1],iofvs2[1])))
  expect_equal(
    maa_example %>%
      filter(ID==lower_ofvres_id) %>%
      dplyr::pull(IPRED),
    (datas[[1]] %>%
       filter(ID==lower_ofvres_id) %>%
       dplyr::pull(IPRED) %>%
       magrittr::multiply_by(exp(-0.5*iofvs1[1]))) %>%
      magrittr::add(datas[[2]] %>%
                      filter(ID==lower_ofvres_id) %>%
                      dplyr::pull(IPRED) %>%
                      magrittr::multiply_by(exp(-0.5*iofvs2[1]))) %>%
      magrittr::divide_by(iofv_weighting_1),
    tolerance = 0.001
  )
  expect_equal(
    msa_example %>%
      filter(ID==lower_ofvres_id) %>%
      dplyr::pull(IPRED),
    datas[[1]] %>% # mod1 would win selection of lower
      filter(ID==lower_ofvres_id) %>%
      dplyr::pull(IPRED),
    tolerance = 0.001
  )
  iofv_weighting_2 <- sum(exp(-0.5*c(iofvs1[2],iofvs2[2])))
  expect_equal(
    maa_example %>%
      filter(ID==upper_ofvres_id) %>%
      dplyr::pull(IPRED),
    (datas[[1]] %>%
       filter(ID==upper_ofvres_id) %>%
       dplyr::pull(IPRED) %>%
       magrittr::multiply_by(exp(-0.5*iofvs1[2]))) %>%
      magrittr::add(datas[[2]] %>%
                      filter(ID==upper_ofvres_id) %>%
                      dplyr::pull(IPRED) %>%
                      magrittr::multiply_by(exp(-0.5*iofvs2[2]))) %>%
      magrittr::divide_by(iofv_weighting_2),
    tolerance = 0.001
  )
  expect_equal(
    msa_example %>%
      filter(ID==upper_ofvres_id) %>%
      dplyr::pull(IPRED),
    datas[[2]] %>% # mod2 would win selection of upper
      filter(ID==upper_ofvres_id) %>%
      dplyr::pull(IPRED),
    tolerance = 0.001
  )
  # confirm weight basis
  aic_example <- modavg_xpdb(subsetted,avg_by_type = "ipred", weight_basis = "aic") %>% xpose::get_data(quiet = TRUE)
  res_example <- modavg_xpdb(subsetted,avg_by_type = "ipred", weight_basis = "res") %>% xpose::get_data(quiet = TRUE)
  for (idnum in 1:2) { # for loop wo make it a little shorter
    use_id <- c(lower_ofvres_id,upper_ofvres_id)[idnum]
    aic_weighting <- sum(exp(-0.5*(c(iofvs1[idnum],iofvs2[idnum]) + 2*npars)))
    expect_equal(
      aic_example %>%
        filter(ID==use_id) %>%
        dplyr::pull(IPRED),
      (datas[[1]] %>%
         filter(ID==use_id) %>%
         dplyr::pull(IPRED) %>%
         magrittr::multiply_by(exp(-0.5*(iofvs1[idnum] + 2*npars[1])))) %>%
        magrittr::add(datas[[2]] %>%
                        filter(ID==use_id) %>%
                        dplyr::pull(IPRED) %>%
                        magrittr::multiply_by(exp(-0.5*(iofvs2[idnum] + 2*npars[2])))) %>%
        magrittr::divide_by(aic_weighting),
      tolerance = 0.001
    )
    res_weighting <- sum(exp(-0.5*c(res1[idnum],res2[idnum])))
    expect_equal(
      res_example %>%
        filter(ID==use_id) %>%
        dplyr::pull(IPRED),
      (datas[[1]] %>%
         filter(ID==use_id) %>%
         dplyr::pull(IPRED) %>%
         magrittr::multiply_by(exp(-0.5*res1[idnum]))) %>%
        magrittr::add(datas[[2]] %>%
                        filter(ID==use_id) %>%
                        dplyr::pull(IPRED) %>%
                        magrittr::multiply_by(exp(-0.5*res2[idnum]))) %>%
        magrittr::divide_by(res_weighting),
      tolerance = 0.001
    )
  }
  # individual versus population
  pop_example <- modavg_xpdb(subsetted,avg_by_type = "ipred",weight_type = "population") %>% xpose::get_data(quiet = TRUE)
  pop_ofv_weighting <- sum(exp(-0.5*ofvs))
  expect_equal(
    pop_example %>%
      filter(ID==lower_ofvres_id) %>%
      dplyr::pull(IPRED),
    (datas[[1]] %>%
       filter(ID==lower_ofvres_id) %>%
       dplyr::pull(IPRED) %>%
       magrittr::multiply_by(exp(-0.5*ofvs[1]))) %>%
      magrittr::add(datas[[2]] %>%
                      filter(ID==lower_ofvres_id) %>%
                      dplyr::pull(IPRED) %>%
                      magrittr::multiply_by(exp(-0.5*ofvs[2]))) %>%
      magrittr::divide_by(pop_ofv_weighting),
    tolerance = 0.001
  )
  expect_equal(
    pop_example %>%
      filter(ID==upper_ofvres_id) %>%
      dplyr::pull(IPRED),
    (datas[[1]] %>%
       filter(ID==upper_ofvres_id) %>%
       dplyr::pull(IPRED) %>%
       magrittr::multiply_by(exp(-0.5*ofvs[1]))) %>%
      magrittr::add(datas[[2]] %>%
                      filter(ID==upper_ofvres_id) %>%
                      dplyr::pull(IPRED) %>%
                      magrittr::multiply_by(exp(-0.5*ofvs[2]))) %>%
      magrittr::divide_by(pop_ofv_weighting),
    tolerance = 0.001
  )
  # combo pop and res or aic
  popres_example <- modavg_xpdb(subsetted,avg_by_type = "ipred",weight_type = "population",weight_basis = "res") %>% xpose::get_data(quiet = TRUE)
  pop_res_weighting <- sum(exp(-0.5*totalress))
  expect_equal(
    popres_example %>%
      filter(ID==lower_ofvres_id) %>%
      dplyr::pull(IPRED),
    (datas[[1]] %>%
       filter(ID==lower_ofvres_id) %>%
       dplyr::pull(IPRED) %>%
       magrittr::multiply_by(exp(-0.5*totalress[1]))) %>%
      magrittr::add(datas[[2]] %>%
                      filter(ID==lower_ofvres_id) %>%
                      dplyr::pull(IPRED) %>%
                      magrittr::multiply_by(exp(-0.5*totalress[2]))) %>%
      magrittr::divide_by(pop_res_weighting),
    tolerance = 0.001
  )
  popaic_example <- modavg_xpdb(subsetted,avg_by_type = "ipred",weight_type = "population",weight_basis = "aic") %>% xpose::get_data(quiet = TRUE)
  pop_aic_weighting <- sum(exp(-0.5*(ofvs + 2*npars)))
  expect_equal(
    popaic_example %>%
      filter(ID==lower_ofvres_id) %>%
      dplyr::pull(IPRED),
    (datas[[1]] %>%
       filter(ID==lower_ofvres_id) %>%
       dplyr::pull(IPRED) %>%
       magrittr::multiply_by(exp(-0.5*(ofvs[1] + 2*npars[1])))) %>%
      magrittr::add(datas[[2]] %>%
                      filter(ID==lower_ofvres_id) %>%
                      dplyr::pull(IPRED) %>%
                      magrittr::multiply_by(exp(-0.5*(ofvs[2] + 2*npars[2])))) %>%
      magrittr::divide_by(pop_aic_weighting),
    tolerance = 0.001
  )
  # combo res and msa
  res_maa_example <- modavg_xpdb(subsetted,avg_by_type = "ipred", weight_basis = "res", algorithm = "msa") %>% xpose::get_data(quiet = TRUE)
  for (idnum in 1:2) {
    use_id <- c(lower_ofvres_id,upper_ofvres_id)[idnum]
    expect_equal(
      res_maa_example %>%
        filter(ID==use_id) %>%
        dplyr::pull(IPRED),
      datas[[idnum]] %>%
        filter(ID==use_id) %>%
        dplyr::pull(IPRED),
      tolerance = 0.001
    )
  }
})
