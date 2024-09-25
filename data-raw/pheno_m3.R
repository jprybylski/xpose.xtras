# PKPD toy model with M3 fit
# Documentation for this is in the package
tempd <- tempdir()
fitdir <- "pkpdm3"
dir.create(file.path(tempd, fitdir))
untar(here::here("inst","pkpd_m3","pkpd_m3.tar.gz"), exdir = file.path(tempd, fitdir))

pkpd_m3 <- xpose::xpose_data(runno = "", dir=file.path(tempd, fitdir)) %>%
  xpose::set_var_types(ipred="IPRE") %>%
  as_xp_xtras() %>%
  set_option(
    dir = paste0("~/",fitdir)
  ) %>%
  set_prop(
    dir = paste0("~/",fitdir)
  )


usethis::use_data(pkpd_m3, overwrite = TRUE)
rm(list=ls())
