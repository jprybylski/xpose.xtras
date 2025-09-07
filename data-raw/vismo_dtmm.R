# Read in poportional odds model fit
# Documentation for this is in the package
tempd <- tempdir()
fitdir <- "vdtmm"
dir.create(file.path(tempd, fitdir))
untar(here::here("inst","vismodegib_models","dtmm.tar.gz"), exdir = file.path(tempd, fitdir))

vismo_dtmm <- xpose::xpose_data(runno = 30, dir=file.path(tempd, fitdir)) %>%
  as_xp_xtras() %>%
  set_option(
    dir = paste0("~/",fitdir)
  ) %>%
  set_prop(
    dir = paste0("~/",fitdir)
  )


usethis::use_data(vismo_dtmm, overwrite = TRUE, compress = "xz")
rm(list=ls())
