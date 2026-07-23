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
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Passed to <[`set_var_types`][xpose::set_var_types]> after processing.
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
#' @name set_var_types_x
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
    rlang::try_fetch(
      tidyselect::eval_select(dots, env = cenv, data = dat$data[[.x]], error_call = cenv, strict=TRUE),
      error = function(s) {
        rlang::warn(
          sprintf("A selected column doesn't exist in problem %s", .x),
          parent = s
        )
        return(
          tidyselect::eval_select(dots, env = cenv, data = dat$data[[.x]], error_call = cenv, strict=FALSE)
        )
      }
    )
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
  # remove empty .coltypes
  .coltypes <- .coltypes[purrr::map_lgl(.coltypes, function(.x) length(.x)>0)]
  if (length(.coltypes)==0) return(xpdb)

  out <- rlang::exec(xpose::set_var_types,
               xpdb = xpdb,
               .problem = .problem,
               !!!.coltypes,
               auto_factor = auto_factor,
               quiet = quiet)
  as_xpdb_x(out)
}


#' Add simulation counter
#'
#' Bugfix for \code{\link[xpose]{irep}}.
#'
#' @description
#' For `xpose` version > 0.5.0  `r lifecycle::badge("deprecated")`
#'
#' Because this has been fixed in the parent package, the fix will be removed
#' in an upcoming release.
#'
#'
#' Add a column containing a simulation counter (irep). A new simulation is counted every time
#' a value in x is different than its previous value and is a duplicate.
#'
#' This version of the function does not require IDs be ascending, but does not work for
#' datasets where IDs are repeated (not in sequence). Both cases are read as separate
#' individuals for NONMEM, but NONMEM does not need to detect repetition of ID sequences (for NONMEM,
#' \code{1,1,2,2,3,3,1,1,2,2,3,3} is 6 individuals, regardless of being 2 repeats of 3 individuals).
#' Given the vast majority of datasets use 1 individual per ID, (which cannot be said about IDs
#' always being ascending), only one of these corrections is implemented.
#'
#' @param x The column to be used for computing simulation number, usually the ID column.
#' @param quiet Logical, if \code{FALSE} messages are printed to the console.
#'
#' @returns `<numeric>` vector tracking the number of simulations based on unique subject IDs.
#' @examples
#' data("xpdb_ex_pk", package = "xpose")
#'
#' xpdb_ex_pk_2 <- xpdb_ex_pk %>%
#'  mutate(sim_id = irep(ID), .problem = 2)
#'
#' @export
irep <- function(x, quiet = FALSE) {
  if (utils::packageVersion("xpose") >= "0.5.0") {
    lifecycle::deprecate_soft("0.1.0", "irep()", "xpose::irep()")
    # Forward to corrected base version
    return(xpose::irep(x,quiet = quiet))
  }
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



#' Patch condition number extraction
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Bugfix for \code{xpose:::sum_condn}, the internal function `xpose` uses to
#' populate the `'condn'` (condition number) entry of \code{xpdb$summary}.
#'
#' For NONMEM runs with more than one estimation method (e.g. `SAEM` followed
#' by importance sampling), the `.lst` file contains more than one
#' `EIGENVALUES OF COR MATRIX OF ESTIMATE` block. `xpose` always uses the
#' *first* block found, which is not necessarily from the final estimation
#' method, so the reported condition number can be wrong. This patch instead
#' uses the *last* block, matching the value reported by NONMEM-adjacent
#' tools such as PsN's `sumo`.
#'
#' @param xpdb An \code{xpose_data} or \code{xp_xtras} object.
#'
#' @return The \code{xpdb} object, with a corrected `'condn'` entry in
#' \code{xpdb$summary} (unchanged if \code{xpdb} is not from `nonmem`, or if
#' no eigenvalues could be found).
#' @export
#'
#' @examples
#' xpdb_ex_pk <- patch_condn(xpose::xpdb_ex_pk)
#'
patch_condn <- function(xpdb) {
  xpose::check_xpdb(xpdb, check = 'summary')

  if (xpose::software(xpdb) != 'nonmem') return(xpdb)

  xpose::check_xpdb(xpdb, check = 'code')
  rounding <- xpdb$xp_theme$rounding

  # xpose.xtras :: Duplicated from xpose:::sum_condn(), with a fix for
  # multi-estimation-method runs: use the last (rather than the first)
  # 'EIGENVALUES OF COR MATRIX OF ESTIMATE' block found in the .lst file.
  new_condn <- xpdb$code %>%
    dplyr::group_by_at(.vars = 'problem') %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = purrr::map_chr(
      .x = .$data,
      .f = ~{
        ## Find the eigenvalues header(s)
        eigen_header <- stringr::str_which(.x$code, stringr::fixed('EIGENVALUES OF COR'))

        if (length(eigen_header) == 0) return(NA_character_)
        # xpose.xtras :: patch for issue #60 -- use the last estimation
        # method's eigenvalues, not the first
        if (length(eigen_header) > 1) eigen_header <- max(eigen_header)

        # Find numeric values in the format of eigen values
        eigen_rows <- eigen_header - 1 + stringr::str_which(.x$code[eigen_header:length(.x$code)], pattern = "\\d\\.\\d{2}E[+-]?\\d+(?=\\s|$)")

        ## Make sure rows are consecutive to prevent possible false positive match
        diff_rows <- c(1, diff(eigen_rows))
        if (any(diff_rows != 1)) {
          eigen_rows <- eigen_rows[1:(which(diff_rows != 1) - 1)]
        }

        ## Parse the eigen values
        eigen_values <- .x[eigen_rows, ] %>%
          dplyr::pull("code") %>%
          stringr::str_trim(side = 'both') %>%
          paste(collapse = " ") %>%
          stringr::str_split(pattern = '\\s+') %>%
          purrr::flatten_chr() %>%
          as.numeric()

        ## Compute the condition number
        eigen_values %>%
          {max(.)/min(.)} %>%
          round(digits = rounding) %>%
          as.character()
      }
    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(subprob = 0, label = 'condn', descr = 'Condition number') %>%
    dplyr::select(dplyr::one_of('problem', 'subprob', 'label', 'descr', 'value')) %>%
    dplyr::filter(!is.na(.$value))

  if (nrow(new_condn) == 0) return(xpdb)

  xpdb$summary <- xpdb$summary %>%
    dplyr::filter(!(.$label == 'condn' & .$problem %in% new_condn$problem)) %>%
    dplyr::bind_rows(new_condn) %>%
    dplyr::arrange_at(.vars = c('problem', 'label', 'subprob'))

  xpdb
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
#'
#' @returns The modified `xpose_data` object
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

    if (.fname %in% c('mutate', 'select', 'rename', 'left_join')) {
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

    ## TEMP handling (commented as tidyr_new_interface is no longer exported, so probably not relevant)
    # if (exists("tidyr_new_interface", envir = rlang::ns_env("xpose")) &&
    #     xpose::tidyr_new_interface()) {
      xpdb[['special']] <- xpdb[['special']] %>%
        tidyr::nest(tmp = -dplyr::one_of('problem')) %>%
        dplyr::ungroup()
    # } else {
    #   xpdb[['special']] <- xpdb[['special']] %>%
    #     tidyr::nest(.key = 'tmp') %>%
    #     dplyr::ungroup()
    # }
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
#' @returns An updated `xpose` data object
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
#' @returns Group data in an `xpose` data object
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


#' Backfill missing variables via a left join
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' <[`dplyr::left_join`]> wrapper for `xpose_data` (and, by inheritance,
#' `xp_xtras`) objects. Unlike a plain `left_join()`, a column present in
#' both `x` and `y` (other than the join keys) is not duplicated with
#' `.x`/`.y` suffixes: missing (`NA`) values already in `x` are backfilled
#' from the matching value in `y`, while non-missing values already in `x`
#' are left untouched. This makes it straightforward to backfill a variable
#' (or set of variables) that is only partially recorded, from a second data
#' source keyed on the same join variable(s) (e.g. `ID`).
#'
#' `left_join_x()` accepts `xpose_data`/`xp_xtras` objects directly, with an
#' additional `.problem` argument restricting which problem(s) the join is
#' applied to.
#'
#' `left_join()` without `_x` is defined as an S3 method on `xpose_data`, so
#' that the usual <[`dplyr::left_join`]> generic dispatches here
#' automatically (`xp_xtras` objects are handled the same way, via class
#' inheritance).
#'
#' @param x An `xpose_data` or `xp_xtras` object.
#' @param y A data frame (or another object coercible to one) to join in.
#' @param by Join specification, as in <[`dplyr::left_join`]>. If `NULL`, a natural join is performed using variables common to `x` and `y`.
#' @param copy If `x` and `y` are not from the same source and `copy = TRUE`, `y` is copied to bring it into the same source as `x`. See <[`dplyr::left_join`]>.
#' @param suffix Suffixes used internally to disambiguate a column shared by `x` and `y` before it is backfilled into a single column; not visible in the result.
#' @param ... Other parameters passed onto <[`dplyr::left_join`]>.
#' @param keep Passed to <[`dplyr::left_join`]>. Note that duplicate join key columns (`keep = TRUE`) are backfilled together like any other shared column, rather than kept separate.
#' @param .problem The problem number(s) to which the join will be applied. Uses all problems if `NULL`.
#'
#' @return An updated `xpose_data`/`xp_xtras` object.
#' @export
#'
#' @examples
#' # Some subjects are missing an APGR score in the base dataset
#' xpdb_missing <- pheno_base %>%
#'   mutate_x(APGR = dplyr::if_else(ID %in% c("1", "2"), NA, APGR))
#'
#' # A separate table with the (complete) values, keyed on ID
#' apgr_lookup <- xpose::get_data(pheno_base, quiet = TRUE) %>%
#'   dplyr::distinct(ID, APGR)
#'
#' # Existing APGR values are kept; only the missing ones are filled in
#' left_join_x(xpdb_missing, apgr_lookup, by = "ID")
#'
#' @name left_join_x
left_join_x <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL, .problem = NULL) {
  if (is.null(.problem)) {
    edit_xpose_data(
      .fun = join_backfill, .fname = "left_join", .data = x, .source = "data",
      y = y, by = by, copy = copy, suffix = suffix, keep = keep, ...
    )
  } else {
    edit_xpose_data(
      .fun = join_backfill, .fname = "left_join", .data = x, .problem = .problem, .source = "data",
      y = y, by = by, copy = copy, suffix = suffix, keep = keep, ...
    )
  }
}

#' @rdname left_join_x
#' @importFrom dplyr left_join
#' @export
left_join.xpose_data <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL, .problem = NULL) {
  left_join_x(x = x, y = y, by = by, copy = copy, suffix = suffix, ..., keep = keep, .problem = .problem)
}

#' Left join, backfilling shared columns instead of duplicating them
#'
#' @description
#' As <[`dplyr::left_join`]>, but any column present in both `x` and `y`
#' (besides the join keys) is coalesced instead of suffixed: values already
#' present in `x` are kept, and only missing (`NA`) values are filled in from
#' `y`.
#'
#' @inheritParams left_join_x
#'
#' @return A data frame
#' @keywords internal
join_backfill <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL) {
  joined <- dplyr::left_join(x, y, by = by, copy = copy, suffix = suffix, ..., keep = keep)

  suffix_x <- suffix[[1]]
  suffix_y <- suffix[[2]]
  if (!nzchar(suffix_x) || !nzchar(suffix_y)) return(joined)

  x_names <- names(joined)[endsWith(names(joined), suffix_x)]
  shared <- substr(x_names, 1, nchar(x_names) - nchar(suffix_x))
  shared <- shared[paste0(shared, suffix_y) %in% names(joined)]

  for (col in shared) {
    col_x <- paste0(col, suffix_x)
    col_y <- paste0(col, suffix_y)
    joined[[col]] <- dplyr::coalesce(joined[[col_x]], joined[[col_y]])
    joined[[col_x]] <- NULL
    joined[[col_y]] <- NULL
  }

  joined
}


##### Fix for ggplot2 from xpose@cc0e4b2
##### With backwards compatibility considered
#' Draw an xpose_plot object
#'
#' @description This function explicitly draw an xpose_plot and interprets keywords
#' contained in labels.
#'
#' @param x An \code{xpose_plot} object.
#' @param page The page number to be drawn. Can be specified as vector or range
#' of integer values.
#' @param ... Options to be passed on to the ggplot2 print method.
#' @keywords internal
#' @examples
#' my_plot <- xpose::dv_vs_ipred(xpose::xpdb_ex_pk) +
#'             ggplot2::labs(title = 'A label with keywords: @nind individuals & @nobs observations')
#' # Using the print function
#' print(my_plot)
#'
#' # Or simply by writing the plot object name
#' my_plot
#'
#' @exportS3Method print xpose_plot
print.xpose_plot <- function(x, page, ...) {

  # Parse template titles
  if (xpose::is.xpose.plot(x)) {
    if (utils::packageVersion("ggplot2") > "3.5.2") {
      x_labs <- suppressMessages(ggplot2::get_labs(plot = x))

      # Add prefix to title subtitle, caption and tags
      x <- x + ggplot2::labs(
        title    = xpose::append_suffix(x$xpose, x_labs$title, 'title'),
        subtitle = xpose::append_suffix(x$xpose, x_labs$subtitle, 'subtitle'),
        caption  = xpose::append_suffix(x$xpose, x_labs$caption, 'caption'),
        tag      = xpose::append_suffix(x$xpose, x_labs$tag, 'tag')
      )
    } else {
      x$title <- xpose::append_suffix(x$xpose, x$title,
                                      "title")
      x$gg$labs$subtitle <- xpose::append_suffix(x$xpose, x$gg$labs$subtitle,
                                                 "subtitle")
      x$gg$labs$caption <- xpose::append_suffix(x$xpose, x$gg$labs$caption,
                                                "caption")
      if (utils::packageVersion("ggplot2") >= "3.0.0") {
        x$gg$labs$tag <- xpose::append_suffix(x$xpose, x$gg$labs$tag,
                                              "tag")
      }
    }

    # Get the mapping variables keywords and values
    var_map <- x$mapping %>%
      as.character() %>%
      stringr::str_remove(pattern = "^~") %>%

      ## Improve parsing since we now have to use the .data[["var"]] format in aes()
      ifelse(stringr::str_detect(., "\\.data\\[\\[\"\\w+\"]]"),
             yes = stringr::str_remove_all(., "(\\.data\\[\\[\")|(\"]])"),
             no  = .) %>%
      purrr::set_names(names(x$mapping))


    if (utils::packageVersion("ggplot2") > "3.5.2") {

      # Process the keywords
      x <- x + do.call(
        what = ggplot2::labs,
        args = suppressMessages(ggplot2::get_labs(plot = x)) %>%
          purrr::compact() %>%
          purrr::map_if(
            .p = stringr::str_detect(., '@'),
            .f = xpose::parse_title,
            xpdb = x$xpose,
            problem = x$xpose$problem, quiet = x$xpose$quiet,
            ignore_key = c('page', 'lastpage'),
            extra_key = c('plotfun', 'timeplot', names(var_map)),
            extra_value = c(x$xpose$fun,
                            format(Sys.time(), "%a %b %d %X %Z %Y"),
                            var_map)
          )
      )
    } else {
      # Process the keywords
      x$labels <- x$labels %>%
        purrr::map_if(grepl( "@", .),
                      .f = xpose::parse_title, xpdb = x$xpose,
                      problem = x$xpose$problem, quiet = x$xpose$quiet,
                      ignore_key = c('page', 'lastpage'),
                      extra_key = c('plotfun', 'timeplot', names(var_map)),
                      extra_value = c(x$xpose$fun,
                                      format(Sys.time(), "%a %b %d %X %Z %Y"),
                                      var_map))
    }
  }

  # Print multiple pages
  if (class(x$facet)[1] %in% c('FacetWrapPaginate', 'FacetGridPaginate')) {

    # Get total number of pages
    if (utils::packageVersion("ggplot2") > "3.5.2") {
      page_tot <- plot_layout(x)$n_pages
    } else {
      page_tot <- n_pages(x)
    }

    # Get and check the page number to be drawn
    if (!missing(page)) {
      page_2_draw <- page
    } else if (!is.null(x$facet$params$page)) {
      page_2_draw <- x$facet$params$page
    } else {
      page_2_draw <- 1:page_tot
    }

    if (any(page_2_draw > page_tot)) {
      page_2_draw <- page_2_draw[page_2_draw <= page_tot]
      if (length(page_2_draw) == 0) {
        stop('All `page` element exceeded the total (', page_tot, ') number of pages.', call. = FALSE)
      }
      warning('`page` contained elements exceeding the total (', page_tot, ') number of pages. These were ignored.',
              call. = FALSE)
    }

    # Prevent issue with facet_repair when page = NULL
    x$facet$params$page <- page_2_draw

    # Begin multiple page plotting
    n_page_2_draw <- length(page_2_draw)

    if (interactive() && !x$xpose$quiet) {
      message('Rendering ', n_page_2_draw, ' selected page(s) out of ', page_tot, '.')
    }

    if (n_page_2_draw == 1) {
      x %>%
        paginate(page_2_draw, page_tot) %>%
        plot(...)
    } else {
      if (interactive() && !x$xpose$quiet) {
        pb <- utils::txtProgressBar(min = 0, max = n_page_2_draw,
                                    style = 3)   # Create progress bar
      }
      for (p in seq_along(page_2_draw)) {
        x$facet$params$page <- page_2_draw[p]
        x %>%
          paginate(page_2_draw[p], page_tot) %>%
          plot(...)
        if (interactive() && !x$xpose$quiet) {
          utils::setTxtProgressBar(pb, value = p) # Update progress bar
        }
      }
      if (interactive() && !x$xpose$quiet) close(pb)

      # Prevent ggforce from droping multiple pages value
      x$facet$params$page <- page_2_draw
    }
  } else {
    if (!missing(page)) warning('Faceting not set. Ignoring `page` argument.', call. = FALSE)

    # Warn for big plots
    if (utils::packageVersion("ggplot2") > "3.5.2") {
      panel_tot <- plot_layout(x)$n_panels
    } else {
      panel_tot <- n_panels(x)
    }

    if (panel_tot > 20) {
      xpose::msg(c('The faceting resulted in ', panel_tot,
            ' panels. The plot may take a while to render.'),
          quiet = x$xpose$quiet)
    }

    # Print without multiple pages
    x %>%
      paginate(page_2_draw = 1, page_tot = 1) %>%
      plot(...)
  }
}


# Add page number to pages
paginate <- function(plot, page_2_draw, page_tot) {
  if (utils::packageVersion("ggplot2") > "3.5.2") {
    plot + do.call(
      what = ggplot2::labs,
      args = suppressMessages(ggplot2::get_labs(plot)) %>%
        purrr::compact() %>%
        purrr::map_if(
          .p = ~!is.null(.) && stringr::str_detect(., '@(page|lastpage)'),
          .f = xpose::parse_title, xpdb = plot$xpose,
          problem = plot$xpose$problem, quiet = plot$xpose$quiet,
          extra_key = c('page', 'lastpage'),
          extra_value = c(as.character(page_2_draw), page_tot)
        )
    )
  } else {
    plot$labels <- plot$labels %>%
      purrr::map_if(.p = ~!is.null(.) && stringr::str_detect(., '@(page|lastpage)'),
                    .f = xpose::parse_title, xpdb = plot$xpose,
                    problem = plot$xpose$problem, quiet = plot$xpose$quiet,
                    extra_key = c('page', 'lastpage'),
                    extra_value = c(as.character(page_2_draw), page_tot))
    plot
  }
}


# Calculate the total number of pages
plot_layout <- function(plot) {
  plot_str <- suppressMessages(ggplot2::ggplot_build(plot))

  panels   <- plot_str$layout$layout
  n_panels <- ifelse(!is.null(panels), nrow(panels), 0L)

  pages    <- plot_str$layout$layout$page
  n_pages  <- ifelse(!is.null(pages), max(pages), 0L)

  list(n_panels = n_panels, n_pages = n_pages)
}

## From xpose@74780e9
# Calculate the total number of pages
n_pages <- function(plot) {
  if (utils::packageVersion("ggplot2") > "3.5.2")
    cli::cli_abort("Not intended for use in ggplot2 >= 4.0.0")

  if (utils::packageVersion('ggplot2') <= '2.2.1') {
    page <- ggplot2::ggplot_build(plot)$layout$panel_layout$page
  } else {
    page <- ggplot2::ggplot_build(plot)$layout$layout$page
  }
  if (!is.null(page)) {
    max(page)
  } else {
    0L
  }
}

# Calculate the total number of panels
n_panels <- function(plot) {
  if (utils::packageVersion("ggplot2") > "3.5.2")
    cli::cli_abort("Not intended for use in ggplot2 >= 4.0.0")

  if (utils::packageVersion('ggplot2') <= '2.2.1') {
    page <- ggplot2::ggplot_build(plot)$layout$panel_layout
  } else {
    page <- ggplot2::ggplot_build(plot)$layout$layout
  }
  if (!is.null(page)) {
    nrow(page)
  } else {
    0L
  }
}
