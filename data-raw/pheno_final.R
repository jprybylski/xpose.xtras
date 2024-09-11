# Assumes xpose.xtras is loaded
pheno_final <- pheno_set$run16$xpdb

usethis::use_data(pheno_final, overwrite = TRUE)
rm(list=ls())
