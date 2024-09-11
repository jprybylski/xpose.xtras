data("xpdb_ex_pk", package = "xpose")

# Arbitrary copies
xpdb_ex_pk2 <- xpdb_ex_pk3 <- xpdb_ex_pk4 <- xpdb_ex_pk

xpdb_set <- xpose_set(mod1=xpdb_ex_pk,
                      mod2=xpdb_ex_pk2,
                      fix1 = xpdb_ex_pk3,
                      fix2 = xpdb_ex_pk4,
                      .as_ordered = TRUE)

usethis::use_data(xpdb_set, overwrite = TRUE, compress = "gzip")
rm(list=ls())
