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
# This assumes xpose.xtras has been loaded
dir_proc <- function (dir) {
  runno <- stringr::str_extract(dir," (\\d+)$", 1)
  childno <- model_output_dirs %>%
    .[!.==dir] %>%
    .[stringr::str_detect(., stringr::fixed(dir))] %>%
    stringr::str_replace(stringr::fixed(paste0(dir,"/")), "") %>%
    .[!stringr::str_detect(., stringr::fixed("/"))] %>%
    stringr::str_extract(" (\\d+)$", 1)
  rel_dir <- fs::path_rel(dir, tmpdir) %>%
    paste0("~/", .)

  # create xpdb and define covariates
  xpdb <- xpose::xpose_data(runno, dir=dir) %>%
    as_xpdb_x() %>%
    set_var_types(
      contcov = WT,
      catcov = APGR
    ) %>%
    xpose::set_var_labels(
      WT = "Weight",
      APGR = "Apgar score"
    ) %>%
    xpose::set_var_units(
      WT = "kg"
    ) %>%
    set_var_levels(
      APGR = lvl_inord(paste(1:10))
    ) %>%
    set_option(
      dir = rel_dir
    ) %>%
    set_prop(
      dir = rel_dir
    )


  list(
    children = childno,
    xpdb = as_xp_xtras(xpdb)
  )
}

# Flat list of objects
flat_xpdbs <- purrr::map(model_output_dirs, dir_proc) %>%
  setNames(paste0("run", stringr::str_extract(model_output_dirs," (\\d+)$", 1)))

# Generate set with proper hierarchy
pheno_set <- purrr::map(flat_xpdbs, ~.x$xpdb) %>%
  {xpose_set(!!!.)}
for (run in names(flat_xpdbs)) {
  if (length(flat_xpdbs[[run]]$children)==0) next
  rel <- as.formula(paste0(
    paste(paste0("run", flat_xpdbs[[run]]$children), collapse="+")
           , "~ ", run))
  pheno_set <- add_relationship(pheno_set, rel)
}

usethis::use_data(pheno_set, overwrite = TRUE, compress = "gzip")
rm(list=ls())
