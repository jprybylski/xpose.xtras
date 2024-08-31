# These functions create and manipulate the xp_xtras object, which is an extension of the xpose_data object.
# xp_xtras objects are backwards compatible with xpose_data objects, and can be used in the same way.

#' Convert an object to an `xpose_data` and `xp_xtras` object
#' @rdname xp_xtras
#' @order 1
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
#' xp_x <- as_xpdb_x(xpose::xpdb_ex_pk)
#' check_xpdb_x(xp_x)
#'
as_xpdb_x <- function(x) {
  new_x <- structure(x, class=c("xp_xtras","uneval")) # skips more basic checks below

  # Next check if it has _xtras parts already (and already inherits the class)
  if (check_xpdb_x(new_x) || inherits(x, "xp_xtras")) {
    # If it does, set up to return x
    new_x <- x
  } else{
    # If it doesn't, fill info with empty versions of true\\

    # Space for levels in index
    new_x$data <- new_x$data %>%
      # add nested levels to index
      mutate(
        index = purrr::map(index, ~{
          mutate(.x, levels = list(NA))
        })
      )

    # Update xp_theme with xp_xtras theme
    # Would use xpose::update_themes but that does a lot more than is needed
    new_x$xp_theme <- xp_xtra_theme(new_x$xp_theme)

  }


  # Now just declare class
  new_x <- structure(
    new_x,
    class = c("xp_xtras", "xpose_data", "uneval")
  )
  new_x
}

#'
#' @rdname xp_xtras
#' @order 2
#'
#' @param x Suspected `xp_xtras` object
#'
#' @export
check_xpdb_x <- function(x) {
  # Basic check first
  if (inherits(x, "xpose_data") && !is_xp_xtras(x)) {
    # First just add the class and see if it passes the check
    # This is d/t the cross-compatibility, so an xpose function may
    # strip the xp_xtras class for a valid xp_xtras object
    test_x <- x
    class(test_x) <- c("xp_xtras", class(x))
    if (check_xpdb_x(test_x)) return(TRUE)
    # Return warning if this is valid
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
  ### check that "dotplot_" is in xp_theme
  if ("xp_theme" %in% names(x) && !any(
    grepl("^dotplot_", names(x$xp_theme))
  )) {
    return(FALSE)
  }

  TRUE
}

# Alias for name consistency
#' @export
check_xp_xtras <- function(...) check_xpdb_x(...)

#' Basic class checker for `xp_xtras`
#'
#' @param x Object to test
#'
#' @return <`logical`> TRUE if `xp_xtras` object
#' @export
#'
#' @examples
#'
#' is_xp_xtras(xpose::xpdb_ex_pk)
#'
#' is_xp_xtras(xpdb_x)
#'
is_xp_xtras <- function(x) {
  inherits(x, "xp_xtras")
}

#' @rdname set_var_types
#' @order 2
#' @export
set_var_types <- function (xpdb, .problem = NULL, ..., auto_factor = TRUE, quiet) {
  UseMethod("set_var_types")
}


#' @rdname set_var_types
#' @order 3
#' @export
set_var_types.default <- function (xpdb, .problem = NULL, ..., auto_factor = TRUE, quiet) {
  xpose::set_var_types(xpdb=xpdb, .problem = .problem, ..., auto_factor = auto_factor, quiet=quiet)
}

#' @rdname set_var_types
#' @order 4
#' @export
set_var_types.xp_xtras <- function (xpdb, .problem = NULL, ..., auto_factor = TRUE, quiet) {
  set_var_types_x(xpdb=xpdb, .problem = .problem, ..., auto_factor = auto_factor, quiet=quiet)
}


#' Set variable levels
#'
#' @description
#' For variable types such as `catcov`, it can be convenient to define
#' levels. This function provides a straightforward means to do so,
#' consistent with `tidy` functions like <[`case_when`][dplyr::case_when]>.
#'
#' Several convenience functions are provided for common levels in <[`levelers`][`as_leveler`]>.
#'
#' @param xpdb <`xp_xtras`> object
#' @param .problem <`numeric`> Problem number to use. Uses the all problems if `NULL`
#' @param ... <`list`> of formulas or leveler functions, where the relevant variable is provided as the argument,
#' @param .missing <`character`> Value to use for missing levels
#' @param .handle_missing <`character`> How to handle missing levels: "quiet", "warn", or "error"
#'
#' @return <`xp_xtras`> object with updated levels
#' @export
#'
#' @examples
#'
#' set_var_levels(xpdb_x,
#'   SEX = lvl_sex(),
#'   MED1 = lvl_bin(),
#'   MED2 = c(
#'     0 ~ "n",
#'     1 ~ "y"
#'   )
#' )
#'
set_var_levels <- function(xpdb, .problem = NULL, ..., .missing = "Other", .handle_missing = c("quiet","warn","error")) {

  # Basic check
  if (!check_xpdb_x(xpdb)) rlang::abort("xp_xtras object required.")
  xpose::check_xpdb(xpdb, check = "data")
  xp_d <- xpdb$data
  if (!is.null(.problem) && !.problem %in% xp_d$problem) cli::cli_abort("Problem number { .problem} not valid.")

  # Arg process
  .handle_missing = rlang::arg_match0(arg = .handle_missing, values = c("quiet","warn","error"))

  # Relevant index
  full_index <- get_index(xpdb, .problem=.problem)
  # Relevant data
  full_data <- xpose::get_data(xpdb, .problem=.problem, quiet = TRUE)

  # Consume dots
  lvl_list <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "keep")
  check_levels(lvl_list, full_index)

  # Add all levels
  new_x <- xpdb
  new_index <- full_index
  lvl_names <- unique(names(lvl_list))
  for (lvn in lvl_names) {
    lv_sub <- lvl_list[names(lvl_list) == lvn]

    if (is_leveler(lv_sub[[1]])) {
      levs <- lv_sub[[1]] # Should only be one, but do this to unlist
      n_levs <- length(levs)
      st_levs <- attr(levs, "start")
      seq_levs <- seq(st_levs, st_levs+n_levs-1, 1)

      lvls <- purrr::map2(seq_levs, levs, ~ stats::formula(paste0(.x,"~'",.y,"'")))

    } else {

      lvls <- do.call(c,  lv_sub)

    }

    rlang::try_fetch(
      plvls <- proc_levels(lvls),
         error = function(cnd) {
            cli::cli_abort(
              "{cli::col_red(lvn)}: LHS should all be numeric, and RHS should all be quoted strings (characters)."
            )
         })

    # make sure lhs are in data
    lvl_col <- full_data %>% dplyr::pull(!!lvn)
    lev_vals <- plvls$value
    if (!all(lev_vals %in% lvl_col) && .handle_missing!="quiet") {
      msg_txt <- "The following values are not in {lvn}: {setdiff(lev_vals,lvl_col)}."
      if (.handle_missing=="warn") cli::cli_warn(paste(msg_txt,"Level-based plots may look odd."))
      if (.handle_missing=="error") cli::cli_abort(msg_txt)
    }
    if (!all(lvl_col %in% lev_vals)) {
      msg_txt <- "{cli::col_cyan(lvn)} values are missing in levels: {setdiff(lvl_col,lev_vals)}."
      if (.handle_missing=="warn") cli::cli_warn(paste(msg_txt,"They will be treated as missing."))
      if (.handle_missing=="error") cli::cli_abort(msg_txt)
      missing_levels <- unique(setdiff(lvl_col,lev_vals)) %>%
        as.character() %>%
        as.double() %>%
        tibble::tibble(value = ., level=.missing)
      plvls <- dplyr::bind_rows(
        plvls,
        missing_levels
      )
    }

    # put processed levels in the index tibble
    new_index <- new_index %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        levels = ifelse(
          col == lvn,
          list(plvls),
          list(levels)
        )
      ) %>%
      dplyr::ungroup()
    new_x <- set_index(new_x, new_index)
  }
  new_x
}

#' Verify validity of level list
#'
#' @param lvl_list <`list`> of formulas or leveler functions
#' @param index Index of `xp_xtras` object
#'
#' @return Nothing, warning or error
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
    if (!is_formula_list(li) && !is_leveler(li)) cli::cli_abort("{cli::style_bold(li_nm)} is neither a list of formulas nor a leveler convenience function.")
  }

  # Make sure for repeated elements, none are levelers
  repeated <- lvl_list[duplicated(names(lvl_list)) | duplicated(names(lvl_list), fromLast=TRUE)]
  if (length(repeated)>0) {
    test_levelers <- purrr::map_lgl(repeated, is_leveler)
    if (any(test_levelers)) cli::cli_abort("Repeated elements must be formula lists, not levelers.")
  }

  # Warn if level won't matter (not a leveled var type)
  level_types <- c("catcov", "dvid", "occ", "catdv") # catdv is an xp_xtras type
  valid_index <- dplyr::filter(index, type %in% level_types)
  if (!all(names(lvl_list) %in% valid_index$col)) {
    cli::cli_warn("Var types not compatible with levels, but levels will still be applied: {setdiff(names(lvl_list), valid_index$col)}\n")
  }


}

#' Convert levels list into tibble
#'
#' @description
#' Consumes formula list and converts into corresponding tibble.
#'
#' @param lvl_list <`list`> of formulas
#'
#' @return <`tibble`> of levels
#'
proc_levels <-  function(lvl_list) {
  purrr::map_dfr(
    lvl_list,
    ~ {
      # Extract symbols
      lhs <- .x[[2]]
      rhs <- .x[[3]]
      # Create a tibble
      tibble::tibble(
        value = lhs,
        level = rhs
      )
    }
  )
}


#' Level-defining helper functions
#' @rdname levelers
#' @order 1
#'
#' @param x <`character`> vector of levels
#' @param .start_index <`numeric`> starting index for levels
#'
#' @return Special character vector suitable to be used as leveler
#' @export
#'
#' @examples
#'
#' set_var_levels(xpdb_x,
#'   SEX = lvl_sex(),
#'   MED1 = lvl_bin(),
#'   MED2 = lvl_inord(c("n","y"), .start_index = 0)
#'   )
#'
as_leveler <- function(x, .start_index = 1) {
  structure(
    x,
    class = c("xp_levels", class(x)),
    start = .start_index[1]
  )
}
#' @rdname levelers
#' @order 2
#' @export
is_leveler <- function(x) inherits(x, "xp_levels")
#' @rdname levelers
#' @order 3
#' @export
lvl_bin <- function(x = c("No","Yes"), .start_index = 0) {
  if (length(x)!=2) cli::cli_abort("This is a convience function for binary variables.")
  as_leveler(x, .start_index=.start_index)
}
#' @rdname levelers
#' @order 4
#' @export
lvl_sex <- function() {
  lvl_bin(x=c("Male","Female"), .start_index = 1)
}
#' @rdname levelers
#' @order 5
#' @export
lvl_inord <- function(x, .start_index = 1) {
  as_leveler(x, .start_index=.start_index)
}

