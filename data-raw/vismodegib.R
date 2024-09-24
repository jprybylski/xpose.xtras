# Read in dummy dataset using in vismodegib model fitting.
# Documentation for this is in the package
tempd <- tempdir()
untar(here::here("inst","vismodegib_models","muscle_spasm_PO_weekly2.tar.gz"), exdir = tempd)

vismodegib <- readr::read_csv(file.path(tempd, "muscle_spasm_PO_weekly2.csv"))


usethis::use_data(vismodegib, overwrite = TRUE)
rm(list=ls())
