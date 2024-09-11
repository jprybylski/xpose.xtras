# Assumes xpose.xtras is loaded
pheno_base <- pheno_set$run6$xpdb

usethis::use_data(pheno_base, overwrite = TRUE)
rm(list=ls())
