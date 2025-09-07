# PKPD toy model with M3 fit
pkpd_m3_df <- readr::read_csv(here::here("inst", "pkpd_m3", "sim.csv"))


usethis::use_data(pkpd_m3_df, overwrite = TRUE, compress = "xz")
rm(list=ls())
