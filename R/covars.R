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
#' @name covars
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
