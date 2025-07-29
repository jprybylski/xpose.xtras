#' Attach nlmixr2 fit object to xpose data object
#'
#' @param xpdb <`xpose_data`> The object upon which to attach the fit
#' @param obj <`nlmixr2FitData`> Result of the nlmixr2 fit
#'
#' @return An object of the same class as `xpdb` with an additional element.
#' @export
#'
#' @examples
#' \dontrun{
#' # Based on an example from nlmixr2 documentation
#' if (rlang::is_installed("nlmixr2") &&
#'     rlang::is_installed("nlmixr2data")) {
#'   one.cmt <- function() {
#'     ini({
#'       tka <- 0.45 # Ka
#'       tcl <- log(c(0, 2.7, 100)) # Log Cl
#'       tv <- 3.45; label("log V")
#'       eta.ka ~ 0.6
#'       eta.cl ~ 0.3
#'       eta.v ~ 0.1
#'       add.sd <- 0.7
#'     })
#'     model({
#'       ka <- exp(tka + eta.ka)
#'       cl <- exp(tcl + eta.cl)
#'       v <- exp(tv + eta.v)
#'       linCmt() ~ add(add.sd)
#'     })
#'   }
#'
#'   theo_sd_fit <- nlmixr2::nlmixr2(one.cmt, nlmixr2data::theo_sd,
#'       "focei", control=nlmixr2::foceiControl(print=0))
#'
#'   attach_nlmixr2(
#'     xpdb_nlmixr2, theo_sd_fit
#'   ) %>%
#'   as_xpdb_x() %>%
#'   print() # fit will be mentioned in print() method
#' }
#'}
attach_nlmixr2 <- function(
    xpdb,
    obj
  ) {
  rlang::check_installed("nlmixr2", reason = "to interact with attached object.")
  xpose::check_xpdb(xpdb)
  nlmixr2est::assertNlmixrFit(obj)

  xpdb_new <- xpdb
  xpdb_new$fit <- obj

  class(xpdb_new) <- class(xpdb)
  xpdb_new
}


#' Convenience function for ingesting an nlmixr2 model to xpose and xpose.xtras
#'
#' @description
#' A wrapper that executes the pipeline `fit |> xpose_data_nlmixr2 |> attach_nlmixr2 |> as_xp_xtras`
#'
#'
#' @param obj nlmixr2 fit object
#' @param ... Passed to [xpose_data_nlmixr2][xpose.nlmixr2::xpose_data_nlmixr2()]
#'
#' @return An <`xp_xtra`> object with fit attached
#' @export
#'
#' @seealso [attach_nlmixr2()]
#'
nlmixr2_as_xtra <- function(
  obj,
  ...
) {
  mod_name <- deparse(substitute(obj))
  xpose.nlmixr2::xpose_data_nlmixr2(
    obj = obj, ...
  ) %>%
    attach_nlmixr2(obj) %>%
    as_xp_xtras() %>%
    set_prop(file = mod_name) %>%
    mutate_files(name = dplyr::case_when(
      # In current version, this is the only case
      length(name)==1 & all(name=="obj") ~ mod_name,
      # If the situation changes, fall back to default
      TRUE ~ name
    ))
}


#' Test if xpose data object has a fit object
#'
#' @param xpdb <`xpose_data`> object
#'
#' @keywords internal
#' @export
#'
test_nlmixr2_has_fit <- function(xpdb) {
  if (xpose::software(xpdb)!="nlmixr2") return(FALSE)
  "fit" %in% names(xpdb) && inherits(xpdb$fit, "nlmixr2FitData")
}
