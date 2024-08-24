# These functions create and manipulate the xp_xtras object, which is an extension of the xpose_data object.
# xp_xtras objects are backwards compatible with xpose_data objects, and can be used in the same way.

#' Convert an object to an `xpose_data` and `xp_xtras` object
#'
#' @description
#' This function masks the default in `xpose` package, adding the
#' `xp_xtras` class to default `xpose_data` objects.
#'
#' @param x <`list`> object with `xpose_data` and `xp_xtras` components
#'
#' @return <[`xpose_data`][xpose::xpose_data]> and <`xp_xtras`> object
#' @export
#'
#' @examples
#'
#' c()
as_xpdb_x <- function(x) { # TODO
  new_x <- structure(x, class=c("xp_xtras","uneval")) # skips more basic checks below

  # Next check if it has _xtras parts already
  if (check_xpdb_x(new_x)) {
    # If it does, just return new_x
    return(x)
  } else{
    # If it doesn't, fill info with empty versions of true\\

    # Space for levels in index
    new_x$data <- new_x$data %>%
      # add nested levels to index
      mutate(
        index = purrr::map(index, ~{
          mutate(.x, levels = NA)
        })
      )

  }


  # First just declare class
  new_x <- structure(
    new_x,
    class = c("xp_xtras", "xpose_data", "uneval")
  )
  new_x
}

#' @export
check_xpdb_x <- function(x) {
  # Basic check first
  if (inherits(x, "xpose_data") && !inherits(x, "xp_xtras")) {
    cli::cli_alert_warning(
      paste(
        "{cli::col_cyan(deparse(substitute(x)))} is an xpose_data object, but lacks xp_xtras feature.",
        "Use as_xpdb_x() to convert to cross-compatible xp_xtras object.",
        sep ="\n"
      )
    )
  }
  if (!inherits(x, "xp_xtras")) return(FALSE)

  # Check for xp_xtras list elements in an xpose_data object


  ### check for "levels" in index
  if ("data" %in% names(x) &&
      !"levels" %in% names(x$data$index[[1]])
  ) {
    return(FALSE)
  }

  TRUE
}


#' @export
set_var_types <- function (xpdb, .problem = NULL, ..., auto_factor = TRUE, quiet) {
  UseMethod("set_var_types")
}


#' @export
set_var_types.default <- function (xpdb, .problem = NULL, ..., auto_factor = TRUE, quiet) {
  args <- tail(as.list(match.call(expand.dots = TRUE)),-1)
  eval(rlang::call2(xpose::set_var_types,!!!args))
}

#' @export
set_var_types.xp_xtras <- function (xpdb, .problem = NULL, ..., auto_factor = TRUE, quiet) {
  args <- tail(as.list(match.call(expand.dots = TRUE)),-1)
  eval(rlang::call2(set_var_types_x,!!!args))
}


set_var_levels <- function(xpdb, .problem = NULL, ...) {
  # Expected format:
  # set_var_levels(xpdb, MED1 = c(1~AAA,2~BBB,TRUE~CCC)

  # Basic check
  if (!check_xpdb_x(xpdb)) rlang::abort("xp_xtras object required.")

  # Consume dots
  lvl_list <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "keep")
}

#
check_levels <- function(lvl_list, xpdb) {
  # Basic check
  if (!is_formula_list(lvl_list)) rlang::abort("List of formulas required.")

  # Make sure all names in lvl_list are in index


}

#
proc_levels <-  function(lvl_list) {
  purrr::map_dfr(
    lvl_list,
    ~ {
      # Extract symbols
      lhs <- .x[[2]]
      rhs <- .x[[3]]
      # Create a tibble
      tibble::tibble(
        value = list(lhs),
        level = list(rhs)
      )
    }
  )
}

# Predefined levels
as_leveller <- function(x) {
  structure(
    x,
    class = c("levels",class(vals))
  )
}
is_leveller <- function(x) inherits(x, "levels")
lvl_bin <- function(x = c("No","Yes")) {
  as_leveller(x)
}
