# Assumes xpose.xtras is loaded
load(here::here("data", "pheno_set.rda")) # reload to ensure new features are included
pheno_base <- pheno_set$run6$xpdb

usethis::use_data(pheno_base, overwrite = TRUE, compress = "xz")
rm(list=ls())
