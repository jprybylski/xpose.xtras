# Assumes xpose.xtras is loaded
# Multi-method version of pheno_final
pheno_saem <- xpose::xpose_data(runno = 18, dir=here::here("inst", "pheno_saemimp")) %>%
  as_xp_xtras() %>%
  set_option(
    dir = paste0("~/pheno_saemimp")
  ) %>%
  set_prop(
    dir = paste0("~/pheno_saemimp")
  )

usethis::use_data(pheno_saem, overwrite = TRUE, compress = "gzip")
rm(list=ls())
