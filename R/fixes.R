#' Set variable types
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' \code{\link[xpose]{set_var_types}} wrapper that accepts tidyselect syntax.
#' 
#' @param xpdb An \code{xpose_data} object.
#' @param .problem The problem number to which the edits will be applied.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Passed to \code{\link[xpose]{set_var_types}}.
#' @param auto_factor With \code{set_var_types} only. If \code{TRUE} new columns assigned to the type 'catcov' will be converted to
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
#' xpdb_2 <- set_var_types_x(xpdb_ex_pk, .problem = 1, idv = TAD, catcov = starts_with("MED"), contcov = c(CLCR,AGE))
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
  dots <- rlang::expr(c(...))
  cenv <- rlang::current_env()
  
  # Get positions in the data for each column
  .positions <- purrr::map(seq_along(.problem), ~
    tidyselect::eval_select(dots, env = cenv, data = dat$data[[.x]], error_call = cenv, strict=FALSE)
  )
  
  # get types from ...
  .types <- names(rlang::enquos(...))
  .types <- .types[.types != ''] # Ignoring empty
  
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
  
  eval(rlang::call2(xpose::set_var_types, 
               xpdb = xpdb, 
               .problem = .problem, 
               !!!.coltypes, 
               auto_factor = auto_factor, 
               quiet = quiet))
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
