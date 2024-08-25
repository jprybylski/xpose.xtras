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


set_var_levels <- function(xpdb, .problem = NULL, ..., .missing = "Other", .handle_missing = c("quiet","warn","error")) {
  # Expected format:
  # set_var_levels(xpdb, MED1 = c(1~AAA,2~BBB,TRUE~CCC)

  # Basic check
  if (!check_xpdb_x(xpdb)) rlang::abort("xp_xtras object required.")
  xpose::check_xpdb(xpdb, check = "data")
  xp_d <- xpdb$data
  if (!is.null(.problem) && !.problem %in% xp_d$problem) cli::cli_abort("Problem number { .problem} not valid.")

  # Relevant index
  full_index <- get_index(xpose::xpdb_ex_pk, .problem=.problem)

  # Consume dots
  lvl_list <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "keep")
  check_levels(lvl_list, full_index)


}

#
check_levels <- function(lvl_list, index) {
  # Basic check
  #if (!is_formula_list(lvl_list)) rlang::abort("List of formulas required.")

  # Make sure all names in lvl_list are in index
  if (!all(names(lvl_list) %in% index$col))
      cli::cli_abort("Levels provided for elements not in data: {setdiff(names(lvl_list), index$col)}")

  # Make sure each element of lvl_list is either formula list or levels function
  for (li_ind in seq_along(lvl_list)) {
    li <- lvl_list[[li_ind]]
    li_nm <- names(lvl_list)[li_ind]
    if (rlang::is_formula(li))  cli::cli_abort("{cli::style_bold(li_nm)} is a formula, but not a formula list. Try to simply wrap in c().")
    if (!is_formula_list(li) && !is_leveller(li)) cli::cli_abort("{cli::style_bold(li_nm)} is neither a list of formulas nor a leveller convenience function.")
  }

  # Make sure for repeated elements, none are levellers
  repeated <- lvl_list[duplicated(names(lvl_list)) | duplicated(names(lvl_list), fromLast=TRUE)]
  if (length(repeated)>0) {
    test_levellers <- purrr::map_lgl(repeated, is_leveller)
    if (any(test_levellers)) cli::cli_abort("Repeated elements must be formula lists, not levellers.")
  }

  # Warn if level won't matter (not a leveled var type)
  level_types <- c("catcov", "dvid", "occ")
  valid_index <- dplyr::filter(index, type %in% level_types)
  if (!all(names(lvl_list) %in% valid_index$col)) {
    cli::cli_warn("Var types not compatible with levels, but levels will still be applied: {setdiff(names(lvl_list), valid_index$col)}\n")
  }


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
    class = c("xp_levels", class(x))
  )
}
is_leveller <- function(x) inherits(x, "xp_levels")
lvl_bin <- function(x = c("No","Yes")) {
  if (length(x)!=2) cli::cli_abort("This is a convience function for binary variables.")
  as_leveller(x)
}
lvl_inord <- function(x) {
  as_leveller(x)
}

# index function

