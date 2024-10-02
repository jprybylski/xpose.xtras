#' Set variable types
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' <[`set_var_types`][xpose::set_var_types]> wrapper that accepts tidyselect syntax.
#' Character vector-based selection still works.
#'
#' `set_var_types_x` accepts `xpose_data` or `xp_xtras` objects.
#'
#' `set_var_types` without `_x` is defined with S3 methods. To maintain `xpose` expectations,
#' the default method is <[`set_var_types`][xpose::set_var_types]>, but if an `xp_xtras` object
#' is used, the method uses `set_var_types_x`.
#'
#' @param xpdb An \code{xpose_data} object.
#' @param .problem The problem number to which the edits will be applied.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Passed to \code{\link[xpose]{set_var_types}} after processing.
#' @param auto_factor If \code{TRUE} new columns assigned to the type 'catcov' will be converted to
#' factor.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @return An xpose_data object
#' @export
#'
#' @examples
#' data("xpdb_ex_pk", package = "xpose")
#'
#' # Change variable type
#' xpdb_2 <- set_var_types_x(
#'   xpdb_ex_pk, .problem = 1,
#'   idv = TAD,
#'   catcov = starts_with("MED"),
#'   contcov = c(CLCR,AGE)
#'   )
#'
#' @name set_var_types
set_var_types_x <- function(xpdb, .problem = NULL, ..., auto_factor = TRUE, quiet) {
  # xpose.xtras :: Same beginning to the existing function, as that is necessary

  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(quiet)) quiet <- xpdb$options$quiet

  dat <- xpdb$data

  if (!is.null(.problem) && !all(.problem %in% dat$problem)) {
    stop('Problem no.', stringr::str_c(.problem[!.problem %in% dat$problem], collapse = ', '),
         ' not found in model output data.', call. = FALSE)
  }
  if (is.null(.problem)) .problem <- unique(dat$problem)

  # xpose.xtras :: Evaluate ... with tidyselect
  dots <- rlang::expr(c(...)) # enquos() would require a loop, I think
  cenv <- rlang::current_env()

  # Get positions in the data for each column
  .positions <- purrr::map(seq_along(.problem), ~
    tidyselect::eval_select(dots, env = cenv, data = dat$data[[.x]], error_call = cenv, strict=FALSE)
  )

  # get types from ...
  .types <- names(
    #rlang::dots_list(..., .ignore_empty = "all") # tries to evaluate values, gives object not found errors
    rlang::enquos(..., .ignore_empty = "all")
  )

  # Get column type names for each type from .positions
  .coltypes <- purrr::map(.types, ~ {
    cols <- c()
    for (i in seq_along(.positions)) {
      pos <- .positions[[i]]
      pnames <- names(pos)
      dnames <- names(dat$data[[i]])
      cols <- c(cols, dnames[pos[startsWith(pnames, .x)]])
    }
    unique(cols)
  })
  names(.coltypes) <- .types

  out <- eval(rlang::call2(xpose::set_var_types,
               xpdb = xpdb,
               .problem = .problem,
               !!!.coltypes,
               auto_factor = auto_factor,
               quiet = quiet))
  as_xpdb_x(out)
}


#' Add simulation counter
#'
#' Bugfix for \code{\link[xpose]{irep}}.
#'
#' @description Add a column containing a simulation counter (irep). A new simulation is counted everytime
#' a value in x is different than its previous value and is a duplicate.
#'
#' This version of the function does not require IDs be ascending, but does not work for
#' datasets where IDs are repeated (not in sequence). Both cases are read as separate
#' individuals for NONMEM, but NONMEM does not need to detect repition of ID sequences (for NONMEM,
#' \code{1,1,2,2,3,3,1,1,2,2,3,3} is 6 individuals, regardless of being 2 repeats of 3 individuals).
#' Given the vast majority of datasets use 1 individual per ID, (which cannot be said about IDs
#' always being ascending), only one of these corrections is implemented.
#'
#' @param x The column to be used for computing simulation number, usually the ID column.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @examples
#' data("xpdb_ex_pk", package = "xpose")
#'
#' xpdb_ex_pk_2 <- xpdb_ex_pk %>%
#'  mutate(sim_id = irep(ID), .problem = 2)
#'
#' @export
irep <- function(x, quiet = FALSE) {
  if (missing(x)) stop('argument "x" is missing, with no default', call. = FALSE)
  if (is.factor(x)) x <- as.numeric(as.character(x))
  lagcheck <- dplyr::lag(x, default = x[1]) != x
  dupcheck <- duplicated(x)
  check <- dplyr::if_else(lagcheck & dupcheck, 1, 0, missing = 0)
  ilen <- dplyr::first(which(check==1), default = length(x) + 1) - 1
  x <- rep(1:(length(x)/ilen), each=ilen)
  xpose::msg(c('irep: ', max(x), ' simulations found.'), quiet)
  x
}



### More direct edit_xpose_data
### The current implementation does a bit too many
### checks that disrupt expected behavior of imported
### functions. Currently these require difficult workarounds
### to avoid these issues that.
### This function will need to be called directly if we don't want to
### overwrite the xpose methods (or the functions need to be renamed).
#' Master xpdb editing function
#'
#' @description Generic function used to build dedicated editing functions
#'
#' @param .fun An editing function to be applied to the data.
#' @param .fname The name of the editing function.
#' @param .data An xpose database object.
#' @param .problem The problem from which the data will be modified
#' @param .source The source of the data in the xpdb. Can either be 'data' or an output
#' file extension e.g. 'phi'.
#' @param .where A vector of element names to be edited in special (e.g.
#' \code{.where = c('vpc_dat', 'aggr_obs')} with vpc).
#' @param ... Name-value pairs of expressions. Use \code{NULL} to drop a variable.
#' @param check_quos Check that variables referenced exists. `TRUE` matches the
#' behavior of <[`xpose::edit_xpose_data`]>
#'
#' These arguments are automatically quoted and evaluated in the
#' context of the data frame. They support unquoting and splicing.
#' See the dplyr vignette("programming") for an introduction to these concepts.
#' @keywords internal
#' @export
edit_xpose_data <- function(.fun, .fname, .data, ..., .problem, .source, .where, check_quos = FALSE) {

  # Check input
  xpdb <- .data # Avoids issues with dplyr arguments
  if (missing(.source)) .source <- 'data'
  if (length(.source) > 1) stop('Argument `.source` should be of length 1.', call. = FALSE)
  xpose::check_xpdb(xpdb, check = .source)

  # Direct filter to specified source
  if (.source == 'data') {
    if (missing(.problem)) .problem <- xpose::all_data_problem(xpdb)
    if (!all(.problem %in% xpose::all_data_problem(xpdb))) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['data']]$problem], collapse = ', '),
           ' not found in model output data.', call. = FALSE)
    }

    if (check_quos==TRUE)
      xpose::check_quo_vars(xpdb = xpdb, ..., .source = .source, .problem = .problem)

    # do dplyr operation outside of mutate to avoid problems with n()
    xpdb[['data']]$data <- purrr::map_if(xpdb[['data']]$data, xpdb[['data']]$problem %in% .problem,
                                         # Forward all dots so dplyr pronouns work
                                         .f = function(df) .fun(df, ...))
    xpdb[['data']] <- xpdb[['data']] %>%
      dplyr::mutate(modified = dplyr::if_else(.$problem %in% .problem, TRUE, .$modified))

    if (.fname %in% c('mutate', 'select', 'rename')) {
      xpdb[['data']] <- xpose::xpdb_index_update(xpdb = xpdb, .problem = .problem) # Update index
    }
  } else if (.source == 'special') {
    if (missing(.problem)) {
      .problem <- max(xpdb[['special']]$problem)
      xpose::msg(c('Changes will be applied to `special` $prob no.', .problem), quiet = FALSE)
    }
    if (!all(.problem %in% xpdb[['special']]$problem)) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['special']]$problem], collapse = ', '),
           ' not found in `special` data.', call. = FALSE)
    }

    if (check_quos==TRUE)
      xpose::check_quo_vars(xpdb = xpdb, ..., .source = .source, .problem = .problem)

    xpdb[['special']] <- xpdb[['special']] %>%
      dplyr::group_by_at(.vars = 'problem')

    ## TEMP handling
    if (xpose::tidyr_new_interface()) {
      xpdb[['special']] <- xpdb[['special']] %>%
        tidyr::nest(tmp = -dplyr::one_of('problem')) %>%
        dplyr::ungroup()
    } else {
      xpdb[['special']] <- xpdb[['special']] %>%
        tidyr::nest(.key = 'tmp') %>%
        dplyr::ungroup()
    }
    ## END TEMP

    xpdb[['special']]$tmp <- purrr::map_if(.x = xpdb[['special']]$tmp, .p = xpdb[['special']]$problem %in% .problem,
                                           .f = function(.x, .fun, .where, ...) {
                                             if (.x$method == 'vpc') {
                                               if (any(!.where %in% names(.x$data[[1]]))) {
                                                 warning('elements ', stringr::str_c(.where[!.where %in% names(.x$data[[1]])], collapse = ', '),
                                                         ' not found in ', .x$method, ' ', .x$type, call. = FALSE)
                                               }
                                               .x$data[[1]] <- .x$data[[1]] %>%
                                                 purrr::map_at(.at = .where, .f = .fun, ...)
                                               .x$modified <- TRUE
                                               return(.x)
                                             } else {
                                               stop('edits of `', .x$method, '` data are not yet supported in xpose.', call. = FALSE)
                                             }
                                           }, .fun = .fun, .where = .where, !!!rlang::enquos(...))

    xpdb[['special']] <- xpdb[['special']] %>%
      tidyr::unnest(dplyr::one_of('tmp'))
  } else {
    if (missing(.problem)) .problem <- max(xpdb[['files']]$problem)
    if (!all(.source %in% xpdb[['files']]$extension)) {
      stop('File extension ', stringr::str_c(.source[!.source %in% xpdb[['files']]$extension], collapse = ', '),
           ' not found in model output files.', call. = FALSE)
    }

    if (!all(.problem %in% xpdb[['files']]$problem[xpdb[['files']]$extension %in% .source])) {
      stop('Problem no.', stringr::str_c(.problem[!.problem %in% xpdb[['files']]$problem], collapse = ', '),
           ' not found in model output files.', call. = FALSE)
    }

    if (check_quos==TRUE)
      xpose::check_quo_vars(xpdb = xpdb, ..., .source = .source, .problem = .problem)

    xpdb[['files']]$data <- purrr::map_if(.x = xpdb[['files']]$data, .p = xpdb[['files']]$problem %in% .problem &
                                            xpdb[['files']]$extension %in% .source,
                                          .f = .fun, !!!rlang::enquos(...))
    xpdb[['files']] <- xpdb[['files']] %>%
      dplyr::mutate(modified = dplyr::if_else(.$problem %in% .problem & .$extension %in% .source, TRUE, .$modified))
  }
  xpdb <- xpose::as.xpdb(xpdb)
  if (check_xpdb_x(xpdb, .warn = FALSE)) return(as_xp_xtras(xpdb))
  xpdb
}

#' Add, remove or rename variables in an xpdb
#'
#' @description \code{mutate_x()} adds new variables and preserves existing ones.
#' \code{select()} keeps only the listed variables; \code{rename()} keeps all variables.
#'
#' **Note:** this function uses `xpose.xtras::edit_xpose_data`, but is otherwise
#' the same as <[`xpose::mutate`]>.
#'
#' @inheritParams edit_xpose_data
#'
#' @name modify_xpdb
#' @export
mutate_x <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::mutate, .fname = 'mutate', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}

#' @name modify_xpdb
#' @export
rename_x <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::rename, .fname = 'rename', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}

#' Group/ungroup and summarize variables in an xpdb
#'
#' @description \code{group_by_x()} takes an existing table and converts it into a
#' grouped table where operations are performed "by group". \code{ungroup()} removes grouping.
#' \code{summarize()} reduces multiple values down to a single value.
#'
#' **Note:** this function uses `xpose.xtras::edit_xpose_data`, but is otherwise
#' the same as <[`xpose::group_by`]>.
#'
#' @inheritParams edit_xpose_data
#' @param x Same as .data (used for consistency with dplyr functions).
#'
#' @name summarise_xpdb
#' @export
group_by_x <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::group_by, .fname = 'group_by', .data = .data,
                  .problem = .problem, .source = .source, .where = .where, ...)
}

#' @name summarise_xpdb
#' @export
ungroup_x <- function(.data, ..., .problem, .source, .where) {
  edit_xpose_data(.fun = dplyr::ungroup, .fname = "ungroup",
                  .data = .data, .problem = .problem, .source = .source, .where = .where,
                  ...)
}
