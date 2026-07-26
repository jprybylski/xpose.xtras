# Builds a bbi-style model directory (control stream + output dir) from
# copies of the bundled pheno_saemimp example, without touching inst/.
# Returns a bbi_nonmem_model object (see bbr::new_model()).
make_bbr_fixture <- function(id = "18", description = "test model", finished = TRUE) {
  src_dir <- system.file("pheno_saemimp", package = "xpose.xtras")
  mod_dir <- tempfile("bbr_fixture")
  dir.create(mod_dir)

  file.copy(file.path(src_dir, "run18.mod"), file.path(mod_dir, paste0(id, ".mod")))

  out_dir <- file.path(mod_dir, id)
  dir.create(out_dir)
  out_files <- setdiff(list.files(src_dir, pattern = "^run18\\."), "run18.mod")
  for (f in out_files) {
    file.copy(
      file.path(src_dir, f),
      file.path(out_dir, paste0(id, ".", sub("^run18\\.", "", f)))
    )
  }
  if (finished) {
    writeLines("{}", file.path(out_dir, "bbi_config.json"))
  }

  bbr::new_model(file.path(mod_dir, id), .description = description)
}
