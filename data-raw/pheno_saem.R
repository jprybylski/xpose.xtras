# Assumes xpose.xtras is loaded
# Multi-method version of pheno_final
pheno_saem <- xpose::xpose_data(runno = 18, dir=here::here("inst", "pheno_saemimp")) %>%
  as_xp_xtras()

usethis::use_data(pheno_saem, overwrite = TRUE)
rm(list=ls())
