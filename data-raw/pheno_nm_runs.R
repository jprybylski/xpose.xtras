## NONMEM output info in /pheno_nm, which used the
## pheno_sd dataset (as it is called in nlmixr2)

# NONMEM was run external to this project, but used the data file
# from nlmixr2. The command to create it was simply:
# readr::write_csv(nlmixr2data::pheno_sd, "inst/pheno_nm/data.csv")

usethis::use_data(pheno_nm_runs, overwrite = TRUE, compress = "gzip")
