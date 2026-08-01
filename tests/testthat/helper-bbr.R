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
  # run18.mod's $TAB record hardcodes FILE=run16tab (a leftover from the
  # original run this example was copied from), so xpose looks for that
  # literal name regardless of `id` -- copy it in unrenamed.
  file.copy(file.path(src_dir, "run16tab"), file.path(out_dir, "run16tab"))
  if (finished) {
    writeLines("{}", file.path(out_dir, "bbi_config.json"))
  }

  # Callers are responsible for `on.exit(unlink(attr(mod, "fixture_dir"),
  # recursive = TRUE), add = TRUE)` -- this dir lives under tempfile()'s own
  # tempdir() base, so it's nested under the test session's tempdir() and
  # would be swept up on normal exit regardless, but cleaning it up per-test
  # keeps it from accumulating for the whole run.
  mod <- bbr::new_model(file.path(mod_dir, id), .description = description)
  attr(mod, "fixture_dir") <- mod_dir
  mod
}
