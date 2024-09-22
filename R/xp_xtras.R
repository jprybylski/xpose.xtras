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
    # If it doesn't, fill info with empty versions of true

    # Space for levels in index
    new_x$data <- new_x$data %>%
      # add nested levels to index
      mutate(
        index = purrr::map(index, ~{
          mutate(.x, levels = list(tibble::tibble()))
        })
      )

    # Update xp_theme with xp_xtras theme
    new_x <- xpose::update_themes(xpdb = xpose::as.xpdb(new_x), xp_theme = xp_xtra_theme(new_x$xp_theme))

    # Space for pars
    new_x$pars <- NULL
    # Corresponding option
    new_x$options$cvtype="exact"
  }


  # Now just declare class
  new_x <- structure(
    new_x,
    class = c("xp_xtras", "xpose_data", "uneval")
  )
  new_x
}

#' @export
as_xp_xtras <- function(x) as_xpdb_x(x)

#'
#' @rdname xp_xtras
#' @order 2
#'
#' @param x Suspected `xp_xtras` object
#' @param warn <`logical`> Whether to warn if `xpose_data` but not `xp_xtras`
#'
#'
#' @export
check_xpdb_x <- function(x, .warn=TRUE) {
  # Basic check first
  if (inherits(x, "xpose_data") && !is_xp_xtras(x)) {
    # First just add the class and see if it passes the check
    # This is d/t the cross-compatibility, so an xpose function may
    # strip the xp_xtras class for a valid xp_xtras object
    test_x <- x
    class(test_x) <- c("xp_xtras", class(x))
    if (check_xpdb_x(test_x)) return(TRUE)
    # Return warning if this is valid
    if (.warn) cli::cli_alert_warning(
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
  ### check for "pars" in top level
  if ("pars" %in% names(x)
  ) {
    return(FALSE)
  }

  TRUE
}

# Alias for name consistency
#' @export
check_xp_xtras <- function(...) check_xpdb_x(...)

# Methods
#' @rdname namespace_methods
#' @order 11
#' @method print xp_xtras
#' @export
print.xp_xtras <- function(x, ...) {
  package_flex <- cli::col_magenta(paste(cli::symbol$star, "xp_xtras"))
  cli::cli({
    cli::cli_h3("{package_flex} object")
    cli::cli_text("{cli::style_bold('Model description')}: {get_prop(x, 'descr')}")
    cli::cli_verbatim(capture.output(xpose:::print.xpose_data(x, ...)))
  })
}

# This is not exported from xpose, so to avoid issues...
# TODO: confirm this method can be removed
# #' @method print xpose_data
# #' @export
# print.xpose_data <- function(x, ...) {
#  if (suppressMessages(check_xp_xtras(x))) return(print.xp_xtras(x, ...))
#
#   xpose:::print.xpose_data(x, ...)
# }

# New functions

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
      rlang::try_fetch({
        plvls <- dplyr::bind_rows(
          plvls,
          missing_levels
        )
      },
      error = function(cnd) {
        cli::cli_abort(
          "{cli::col_red(lvn)}: LHS should all be numeric, and RHS should all be quoted strings (characters)."
        )
      })
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

#' @noMd
level_types <- c("catcov", "dvid", "occ", "catdv")# catdv is an xp_xtras type

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


#' Translate values to levels
#'
#' @description
#' This is intended to be used as a convenience function
#' in plotting where levels are set for some variable.
#'
#' @param vals vector of values associated with levels in `lvl_tbl`
#' @param lvl_tbl tibble of levels
#'
#' @export
val2lvl <- function(vals, lvl_tbl = NULL) {
  if (is.null(lvl_tbl)) return(forcats::as_factor(vals))

  lvl_v <- lvl_tbl$level[match(vals,lvl_tbl$value)] %>%
    factor(levels = unique(lvl_tbl$level))
  lvl_v
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


#### Backfill functions
#' Add individual objective function to data
#'
#' @param xpdb <`xpose_data`> or <`xp_xtras`> object
#' @param .label The name of the new column. `iOFV` is the default,
#' but as long as <[`set_var_type`]> is called there is no problem.
#'
#' @details
#' This function will only work for objects with software listed as
#' `nonmem`, which has a `phi` file and with an `OBJ` column in that
#' file.
#'
#'
#' @return <`xp_xtras`> object with new column in the data and a
#' column with `iofv` var type.
#' @export
#'
#' @examples
#'
#' xpdb_x %>%
#'   backfill_iofv() %>%
#'   list_vars()
#'
backfill_iofv <- function(xpdb, .problem=NULL, .subprob=NULL, .label = "iOFV") {
  if (xpose::software(xpdb) != 'nonmem')
    cli::cli_abort("This backfill function only works for nonmem-based objects, not those from {.strong {cli::col_yellow(xpose::software(xpdb))}}")

  xpose::check_xpdb(xpdb, "data")
  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob) # fills in .problem and .subprob if missing

  if (!"phi" %in% xpdb$files$extension) rlang::abort("phi table not found in files.")

  # Get iOFV from phi
  phi_df <- xpdb$files %>%
    dplyr::filter(extension=="phi", problem==.problem, subprob==.subprob) %>%
    dplyr::pull(data) %>%
    .[[1]]
  match_obj <- function(id) {
    phi_df$OBJ[match(id,phi_df$ID)]
  }

  new_xpdb <- as_xp_xtras(xpdb)
  for (prob in .problem) {
    # ID column
    id_col <- xp_var(new_xpdb, .problem = prob, type = "id")$col[1] # Should only be 1 id but just in case
    #? Need to get OBJs before mutate
    xpdb_data <- xpose::get_data(xpdb, .problem = prob, quiet=TRUE)
    new_objs <- function() match_obj(xpdb_data[[id_col]])


    new_xpdb <- new_xpdb %>%
      xpose::mutate(!!.label := new_objs(), .problem = prob) %>%
      set_var_types(.problem = prob, iofv = {{.label}})
  }
  new_xpdb
}

# TODO: write this function
# Calculate constants for N-CMT model (under some assumptions) given trans-dependent input prms
# dots are which prms are what, like for trans=2, dots should be CL=TCL, V=Vd, etc, so
# if the excpected prms are not named as expected
backfill_constants <- function(xpdb, ..., trans = 2) {}

#### Slight updates to list_vars

#' Updates to `list_vars`
#'
#' @description
#' To accommodate changes made in `xpose.xtras`,
#' <[`list_vars`][xpose::list_vars]> needed some minimal updates.
#'
#'
#' @param xpdb <`xpose_data`> or <`xp_xtras`> object
#' @param .problem <`numeric`> Problem number to use. Uses the all problems if `NULL`
#' @param ... Should be blank.
#'
#' @return <`tibble`> of variables`cat()` of variables
#' @export
#'
#'
#' @examples
#'
#' list_vars(xpose::xpdb_ex_pk)
#'
#' list_vars(xpdb_x)
#'
list_vars <- function (xpdb, .problem = NULL, ...) {
  UseMethod("list_vars")
}

#' @export
list_vars.default <- function (xpdb, .problem = NULL, ...) {
  if (suppressMessages(check_xp_xtras(xpdb)))
    return(list_vars.xp_xtras(xpdb, .problem = NULL, ...))

  xpose::list_vars(xpdb = xpdb, .problem = .problem)
}

#' @export
list_vars.xp_xtras  <- function (xpdb, .problem = NULL, ...) {
  #### xpose.xtras ::: Most of the default function can be copied.
  #### xpose.xtras ::: There are some minimal changes throughout for style and new var types

  # Check input
  xpose::check_xpdb(xpdb, check = 'data')

  x <- xpdb$data

  if (!is.null(.problem)) {
    if (!all(.problem %in% x$problem)) {
      cli::cli_abort("Problem no. { .problem[!.problem %in% x$problem]} not found in the data.")
    }
    x <- x[x$problem %in% .problem, ]
  }

  order <- c('id', 'dv', 'catdv', 'idv', 'dvid', 'occ', 'amt', 'evid', 'mdv', 'pred', 'ipred',
             'param', 'eta', 'iofv', 'res', 'catcov', 'contcov', 'a', 'bin', 'na')
  cli::cli({
    if (rlang::is_interactive()) sp <- cli::make_spinner(default_spinner)
    if (rlang::is_interactive()) sp$spin()
    x %>%
      dplyr::mutate(grouping = as.integer(.$problem)) %>%
      dplyr::group_by_at(.vars = 'grouping') %>%
      tidyr::nest() %>%
      dplyr::ungroup() %>%
      {purrr::map(.$data, function(df) {
        if (rlang::is_interactive()) sp$spin()
        cli::cli_bullets("List of available variables for problem no. {df$problem[1]}")
        df$index[[1]] %>%
          dplyr::mutate(type2=type) %>% # xtra :: just to keep type
          dplyr::group_by_at(.vars = 'type') %>%
          tidyr::nest() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            string = purrr::map_chr(.$data, ~{
              if (rlang::is_interactive()) sp$spin()

              cols_c <- unique(.$col)

              # Add labels and/or units
              if (!all(is.na(c(.$label,.$units)))) {
                labs_c <- .$label[!duplicated(.$col)]
                units_c <- .$units[!duplicated(.$col)]
                tocols_c <- stringr::str_c(
                  dplyr::coalesce(stringr::str_c(
                    "'", labs_c, "'"
                  ), ""),
                  dplyr::coalesce(stringr::str_c(
                    ifelse(is.na(labs_c), "", ", "),
                    units_c
                  ), "")
                ) %>% ifelse(.=="", ., paste0(" (",.,")"))
                cols_c <- stringr::str_c(cols_c, cli::style_bold(tocols_c))
              }

              # Add level count
              if (.$type2[1] %in% level_types) {
                lvls_c <- .$levels[!duplicated(.$col)]
                cols_c <- purrr::map2_chr(cols_c, lvls_c, ~{
                  paste0(.x, " [", cli::col_yellow(nrow(.y)),"]")
                })
              }

              stringr::str_c(cols_c, collapse = ', ')
            }),
            descr = dplyr::case_when(type == 'id' ~ 'Subject identifier',
                                     type == 'occ' ~ 'Occasion flag',
                                     type == 'na' ~ 'Not attributed',
                                     type == 'amt' ~ 'Dose amount',
                                     type == 'idv' ~ 'Independent variable',
                                     type == 'ipred' ~ 'Model individual predictions',
                                     type == 'pred' ~ 'Model typical predictions',
                                     type == 'res' ~ 'Residuals',
                                     type == 'evid' ~ 'Event identifier',
                                     type == 'dv' ~ 'Dependent variable',
                                     type == 'catdv' ~ 'Categorical endpoint',
                                     type == 'catcov' ~ 'Categorical covariates',
                                     type == 'contcov' ~ 'Continuous covariates',
                                     type == 'param' ~ 'Model parameter',
                                     type == 'eta' ~ 'Eta',
                                     type == 'iofv' ~ 'Individual OFV',
                                     type == 'bin' ~ 'Binned IDV',
                                     type == 'a' ~ 'Compartment amounts',
                                     type == 'dvid' ~ 'DV identifier',
                                     type == 'mdv' ~ 'Missing dependent variable',
                                     TRUE ~ "Undefined type") %>%
              sprintf("%s (%s)", ., ifelse(type%in%order, type, paste0("?",type)))
          ) %>%
          dplyr::mutate(descr = stringr::str_pad(.$descr, 37, 'right')
          ) %>%
          dplyr::slice(order(match(.$type, order))) %>%
          {stringr::str_c(' -', .$descr, ':', .$string, sep = ' ')} %>%
          stringr::str_c(collapse="\n") %>%
          cli::cli_verbatim()})}
    if (rlang::is_interactive()) sp$finish()
  })

}



#' `xp_var` Method
#'
#' @description
#' To add a small amount of functionality to <[`xp_var`][xpose::xp_var]>,
#' this method was created.
#'
#' @rdname xp_var
#'
#'
#' @inheritParams xpose::xp_var
#'
xp_var <- function (xpdb, .problem, col = NULL, type = NULL, silent = FALSE) {
  UseMethod("xp_var")
}

#' @rdname xp_var
#' @export
xp_var.default <- function (xpdb, .problem, col = NULL, type = NULL, silent = FALSE) {
  if (check_xpdb_x(xpdb, .warn = FALSE)) {
    return(xp_var.xp_xtras(xpdb=xpdb, .problem=.problem, col = col, type = type, silent = silent))
  }

  xpose::check_xpdb(xpdb, check="data") # overlooked check in current version

  xpose::xp_var(xpdb=xpdb, .problem=.problem, col = col, type = type, silent = silent)
}

#' @rdname xp_var
#' @export
xp_var.xp_xtras <- function (xpdb, .problem, col = NULL, type = NULL, silent = FALSE) {
  xpose::check_xpdb(xpdb, check="data")

  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)

  if (!all(.problem %in% xpdb$data$problem)) {
    cli::cli_abort("$prob no.{ .problem[!.problem %in% xpdb$data$problem]} not found in model output data.")
  }
  index <- xpdb$data[xpdb$data$problem == .problem, ]$index[[1]]
  if (!is.null(type)) {
    index <- index[index$type %in% type, ]
  }
  else {
    index <- index[index$col %in% col, ]
  }
  if (!is.null(type) && !is.null(col)) {
    rlang::abort("Cannot declare both `type` and `col`")
  }
  missing_cols <- if (is.null(col)) c() else col[!col%in%index$col]
  missing_types <- if (is.null(type)) c() else type[!type%in%index$type]
  if (nrow(index) == 0 || length(missing_cols)>0 || length(missing_types)>0) {
    if (silent)
      return()
    cli::cli_abort("Column {ifelse(!is.null(type) || length(missing_types)>0,
                    paste0('type ', missing_types), paste0('`',missing_cols,'`'))}
                    not available in data for problem no. { .problem}.
                   Check `list_vars()` for an exhaustive list of available columns.")
  }
  index %>% dplyr::distinct(!!rlang::sym("col"), .keep_all = TRUE) %>%
    dplyr::select(dplyr::one_of("col", "type", "label", "units", "levels")) %>%
    dplyr::arrange(type, col) # would prefer to sort by requested order, but to keep behavior consistent...
}
