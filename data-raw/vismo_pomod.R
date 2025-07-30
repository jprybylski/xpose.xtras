# Read in poportional odds model fit
# Documentation for this is in the package
tempd <- tempdir()
fitdir <- "vpomod"
dir.create(file.path(tempd, fitdir))
untar(here::here("inst","vismodegib_models","pomod.tar.gz"), exdir = file.path(tempd, fitdir))

vismo_pomod <- xpose::xpose_data(runno = 29, dir=file.path(tempd, fitdir)) %>%
  as_xp_xtras() %>%
  set_option(
    dir = paste0("~/",fitdir)
  ) %>%
  set_prop(
    dir = paste0("~/",fitdir)
  )


usethis::use_data(vismo_pomod, overwrite = TRUE, compress = "gzip")
rm(list=ls())
