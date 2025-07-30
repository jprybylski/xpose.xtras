# Assumes xpose.xtras is loaded
load(here::here("data", "pheno_set.rda")) # reload to ensure new features are included
pheno_final <- pheno_set$run16$xpdb

usethis::use_data(pheno_final, overwrite = TRUE, compress = "gzip")
rm(list=ls())
