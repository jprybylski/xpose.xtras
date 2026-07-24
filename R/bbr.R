#' Convenience function for ingesting a bbr model into xpose and xpose.xtras
#'
#' @description
#' [bbr](https://metrumresearchgroup.github.io/bbr/) is a Metrum Research
#' Group package for managing NONMEM modeling workflows via `bbi`. It is not
#' distributed on CRAN (see `Additional_repositories` in this package's
#' `DESCRIPTION` for where to obtain it).
#'
#' Reading a `bbr`-managed model into `xpose`/`xpose.xtras` normally requires
#' manually reconstructing the output file path from the model object, e.g.
#'
#' \preformatted{
#' xpose::xpose_data(
#'   file = file.path(bbr::get_output_dir(mod), paste0(bbr::get_model_id(mod), ".lst"))
#' ) |> as_xp_xtras()
#' }
#'
#' `xp_from_bbr()` wraps that pipeline.
#'
#' @param .mod <`bbi_nonmem_model`> A `bbr` NONMEM model object, e.g. as
#' returned by [bbr::read_model()].
#' @param ... Passed to [xpose::xpose_data()].
#' @param .use_bbr_descr <`logical`> If `TRUE` (default) and `.mod` carries a
#' `bbr` description, use it to set the `descr` property of the result (see
#' [set_prop()]), taking precedence over any description parsed from the
#' NONMEM output itself.
#'
#' @return An <`xp_xtras`> object
#' @export
#'
#' @seealso [bbr::read_model()]
#'
#' @examples
#' if (requireNamespace("bbr", quietly = TRUE)) {
#'   # Build a bbr-style model directory from the bundled pheno_saemimp example
#'   src_dir <- system.file("pheno_saemimp", package = "xpose.xtras")
#'   mod_dir <- tempfile("xp_from_bbr_ex")
#'   dir.create(mod_dir)
#'   file.copy(file.path(src_dir, "run18.mod"), file.path(mod_dir, "18.mod"))
#'   out_dir <- file.path(mod_dir, "18")
#'   dir.create(out_dir)
#'   out_files <- setdiff(list.files(src_dir, pattern = "^run18\\."), "run18.mod")
#'   for (f in out_files) {
#'     file.copy(
#'       file.path(src_dir, f),
#'       file.path(out_dir, paste0("18.", sub("^run18\\.", "", f)))
#'     )
#'   }
#'   # bbr considers a run finished once bbi has written this file
#'   writeLines("{}", file.path(out_dir, "bbi_config.json"))
#'
#'   mod <- bbr::new_model(file.path(mod_dir, "18"), .description = "Phenobarbital SAEM model")
#'   xp_from_bbr(mod)
#' }
xp_from_bbr <- function(.mod, ..., .use_bbr_descr = TRUE) {
  rlang::check_installed("bbr", reason = "to ingest a bbr model object with `xp_from_bbr()`.")
  if (!inherits(.mod, "bbi_nonmem_model")) {
    cli::cli_abort(
      paste(
        "{.arg .mod} must be a {.cls bbi_nonmem_model} object (from {.pkg bbr}), not {.cls {class(.mod)[1]}}.",
        "{.fn xp_from_bbr} only supports basic NONMEM models; other model types",
        "(e.g. a {.cls bbi_nmboot_model} bootstrap run) are not readable by {.pkg xpose}."
      )
    )
  }
  if (!isTRUE(bbr::check_nonmem_finished(.mod))) {
    cli::cli_abort(
      paste(
        "The model at {.path {bbr::get_model_path(.mod)}} has not finished running.",
        "Submit it (e.g. {.fn bbr::submit_model}) and wait for it to complete",
        "before calling {.fn xp_from_bbr}."
      )
    )
  }

  mod_id <- bbr::get_model_id(.mod)
  out_dir <- bbr::get_output_dir(.mod)
  lst_file <- file.path(out_dir, paste0(mod_id, ".lst"))

  if (!file.exists(lst_file)) {
    cli::cli_abort(
      "No NONMEM output found at {.path {lst_file}}, despite the run being reported as finished."
    )
  }

  xpdb <- xpose::xpose_data(file = lst_file, ...) %>%
    as_xp_xtras()

  descr <- .mod$description
  if (isTRUE(.use_bbr_descr) && !is.null(descr) && test_xpdb(xpdb, "summary")) {
    xpdb <- set_prop(xpdb, descr = descr)
  }

  xpdb
}
