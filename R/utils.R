#' Get shrinkage estimates from model summary
#' @description
#'
#' This function parses shrinkages as they are currently
#' presented in \code{\link[xpose]{get_summary}}, so it
#' is dependent on the current implementation of that function.
#'
#' @param xpdb An \code{xpose_data} object.
#' @param wh The shrinkage to extract (`"eta"` or `"eps"`)
#' @param .problem Problem number to use. Uses the xpose default if not provided.
#'
#' @return A numeric vector of shrinkage estimates.
#' @export
#'
#' @examples
#' data("xpdb_ex_pk", package = "xpose")
#'
#' # eta Shrinkage
#' get_shk(xpdb_ex_pk)
#'
#' # epsilon Shrinkage
#' get_shk(xpdb_ex_pk, wh = "eps")
#'
#'
get_shk <- function(xpdb, wh = "eta", .problem = NULL) {
  xpose::get_summary(xpdb) %>%
    dplyr::filter(label==stringr::str_c(wh, "shk")) %>%
    dplyr::filter(problem == dplyr::coalesce(.env$.problem[1],xpose::default_plot_problem(xpdb))) %>%
    dplyr::pull(value) %>%
    stringr::str_split(" \\[\\d+\\],? ?") %>%
    purrr::list_c() %>%
    readr::parse_double() %>%
    purrr::discard(is.na)
}
