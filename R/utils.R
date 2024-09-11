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

  if (length(prop)>1) {
    rlang::abort("Request one property at a time from the xpdb model summary.")
  }
  if (!prop %in% summ$label) {
    cli::cli_abort("{cli::col_cyan(prop)} not in xpdb model summary.")
  }

  summ %>%
    dplyr::filter(label==prop) %>%
    # Include 0 for non-problem values
    dplyr::filter(problem %in% c(0,use_problem)) %>%
    dplyr::pull(value)
}

#' Set a summary property
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> defining which properties to transform. Argument should be valid label.
#' @param .problem <`numeric`> Problem number to use. Uses all problem if not provided.
#'
#' @return `xp_xtras` object
#' @export
#'
#' @examples
#'
#' set_prop(xpose::xpdb_ex_pk, descr = "New model description") %>%
#'   xpose::get_summary()
#'
set_prop <- function(xpdb, ..., .problem = NULL) {
  summ <- xpose::get_summary(xpdb)

  props_to_set <- rlang::dots_list(..., .homonyms = "error", .ignore_empty = "all")

  # Error checks
  if (any(!names(props_to_set) %in% summ$label)) {
    cli::cli_abort("Cannot set non-existant properties, which should be matched to labels: {setdiff(names(props_to_set), summ$label)}")
  }

  # Validate values
  check_sum <- purrr::map_lgl(props_to_set, ~ length(.x)==1)
  if (any(!check_sum)) {
    cli::cli_abort("Properties can only by set to one value. (applies to {names(props_to_set)[!check_sum]})")
  }

  # Convert any numeric or factors to characters for convenience
  props_to_set <- purrr::map(props_to_set, ~{
    if (is.numeric(.x) || inherits(.x, "factor")) paste(.x) else .x
  })

  check_chr <- purrr::map_lgl(props_to_set, ~ inherits(.x, "character"))
  if (any(!check_chr)) {
    cli::cli_abort("Properties can only by set to character/string values. (applies to {names(props_to_set)[!check_chr]})")
  }

  # Row update tibble
  ru_tbl <- tibble::tibble(
    label = names(props_to_set),
    value = purrr::list_c(props_to_set)
  )
  if (!is.null(.problem)) ru_tbl <- purrr::map_dfr(.problem, ~ dplyr::mutate(ru_tbl, problem=.x))

  new_summ <- summ %>%
    dplyr::rows_update(
      ru_tbl,
      by = c("label", `if`(is.null(.problem), NULL, "problem")),
      unmatched = "ignore"
    )
  xpdb$summary <- new_summ
  as_xpdb_x(xpdb)
}

#' Get full index for xpose_data data
#'
#' @rdname get_set_index
#' @order 1
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param .problem <`numeric`> Problem number to use. Uses the all problems if `NULL`
#' @param index <`tibble`> Index to set
#' @param ... Ignored. Here for future expansion
#'
#' @return Tibble of index
#' @export
#'
#' @examples
#' get_index(xpose::xpdb_ex_pk)
get_index <- function(xpdb, .problem=NULL, ...) {
  xpose::check_xpdb(xpdb, check = "data")
  rlang::check_dots_empty0(...)
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

#' @rdname get_set_index
#' @order 1
#' @export
set_index <- function(xpdb, index, ...) {
  xpose::check_xpdb(xpdb, check = "data")
  new_index <- index %>%
    dplyr::nest_by(problem) %>%
    dplyr::ungroup() %>%
    dplyr::rename(index=data) %>%
    dplyr::mutate(index = as.list(index))
  new_d <- dplyr::rows_update(
    xpdb$data,
    new_index,
    by = "problem"
  )
  xpdb$data <- new_d
  as_xpdb_x(xpdb)
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
  check <- purrr::map_lgl(x, rlang::is_formula)
  if (length(check)==0) return(FALSE)
  all(check)
}

#' Reportable digits for model fit
#'
#' @description
#' An opinionated function where for optimization routines
#' that report number of significant digits (eg, FO-based), only
#' those number of digits are considered reportable.
#'
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param .default <`numeric`> Default number of digits to return if not found
#'
#' @return Number of reportable digits
#' @export
#'
#' @examples
#'
#' reportable_digits(xpdb_x)
#'
reportable_digits <- function(xpdb, .default = 3) {
  xpose::check_xpdb(xpdb, "summary")
  digs <- rlang::try_fetch(
    floor(
      as.numeric(
        get_prop(xpdb, "nsig")
      )
    ),
    error = function(x) .default,
    warning = function(x) .default
    )
  if (is.na(digs)) digs <- .default
  digs
}

#' Set an `xpose` option
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments in
#' the form of `option = value`
#'
#' @return `xp_xtras` object
#' @export
#'
#' @examples
#'
#' xpdb_x <- set_option(xpdb_x, quiet = TRUE)
#'

set_option <- function(xpdb, ...) {
  new_opts <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "error")

  old_opts <- xpdb$options

  xpdb$options <- utils::modifyList(old_opts, new_opts)

  as_xpdb_x(xpdb)

}

# TODO: more general extraction of model description from comments
# add option to do this be default when converting to xp_xtra
desc_from_comments <- function(xpdb, start_check = ".*description.*", end_check = "^\\$") {

}
