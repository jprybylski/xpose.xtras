## NONMEM output info in /pheno_nm, which used the
## pheno_sd dataset (as it is called in nlmixr2)

# NONMEM was run external to this project, but used the data file
# from nlmixr2data. The command to create it was simply:
# readr::write_csv(nlmixr2data::pheno_sd, "inst/pheno_nm/data.csv")

# The run results are compressed into a tree, which will be read manually
# If there is ever a function to ingest set from a directory, this approach
# should be replaced by that approach.
# TODO: update with directory consumer
tmpdir <- tempdir()
tree_tar <- here::here("inst", "pheno_nm", "clean_tree.tar.gz")

untar(tree_tar, exdir = tmpdir)

model_output_dirs <- list.dirs(tmpdir, recursive = TRUE) %>%
  .[grepl("Step \\d+", .)]

# process directory function
dir_proc <- function (dir) {
  runno <- stringr::str_extract(dir," (\\d+)$", 1)
  childno <- model_output_dirs %>%
    .[!.==dir] %>%
    .[stringr::str_detect(., stringr::fixed(dir))] %>%
    stringr::str_replace(stringr::fixed(paste0(dir,"/")), "") %>%
    .[!stringr::str_detect(., stringr::fixed("/"))] %>%
    stringr::str_extract(" (\\d+)$", 1)

  list(
    children = childno,
    xpdb = xpose::xpose_data(runno, dir=dir) %>% as_xpdb_x()
  )
}

# Flat list of objects
flat_xpdbs <- purrr::map(model_output_dirs, dir_proc)

#....

usethis::use_data(pheno_nm_runs, overwrite = TRUE, compress = "gzip")
