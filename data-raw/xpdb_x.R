xpdb_x <- as_xpdb_x(xpose::xpdb_ex_pk)


usethis::use_data(xpdb_x, overwrite = TRUE, compress = "gzip")
rm(list=ls())
