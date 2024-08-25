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
  get_prop(
      xpdb = xpdb,
      prop = stringr::str_c(wh, "shk"),
      .problem = .problem
    ) %>%
    stringr::str_split(" \\[\\d+\\],? ?") %>%
    purrr::list_c() %>%
    readr::parse_double() %>%
    purrr::discard(is.na)
}


#' Generic function to extract a property from a model summary
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param prop <`character`> Property to extract
#' @param .problem <`numeric`> Problem number to use. Uses the xpose default if not provided.
#'
#' @return Exact value for the property
#' @export
#'
#' @examples
#'
#' data("xpdb_ex_pk", package = "xpose")
#'
#' get_prop(xpdb_ex_pk, "descr")
get_prop <- function(xpdb, prop, .problem = NULL) {
  use_problem <- dplyr::coalesce(.problem[1],xpose::default_plot_problem(xpdb))

  summ <- xpose::get_summary(xpdb)

  if (length(prop)>1 || !prop %in% summ$label) {
    rlang::abort("Request one property at a time from the xpdb model summary.")
  }

  summ %>%
    dplyr::filter(label==prop) %>%
    # Include 0 for non-problem values
    dplyr::filter(problem %in% c(0,use_problem)) %>%
    dplyr::pull(value)
}

#' Get full index for xpose_data data
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param .problem <`numeric`> Problem number to use. Uses the all problems if `NULL`
#' @param ... Ignored. Here for future expansion
#'
#' @return Tibble of index
#' @export
#'
#' @examples
#' get_index(xpose::xpdb_ex_pk)
get_index <- function(xpdb, .problem=NULL, ...) {
  xpose::check_xpdb(xpdb, check = "data")
  xp_d <- xpdb$data
  if (is.null(.problem)) .problem <- xp_d$problem
  xp_d %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      index = list(mutate(index, problem = .env$problem))
    ) %>%
    dplyr::filter(problem %in% .problem) %>%
    dplyr::pull(index) %>%
    dplyr::bind_rows()
}




#' Convenience functions used in package
#'
#' @rdname convience
#' @order 1
#'
#' @param x object to test
#'
#' @export
is_formula_list <- function(x) {
  if (!is.list(x)) return(FALSE)
  all(purrr::map_lgl(x, rlang::is_formula))
}
