# For adding features to xpose_set
# Need to keep in mind that focus should be temporarily removed if select() and mutate(), etc, used

#' Base model for `xpose_set`
#' @rdname set_base_model
#' @order 1
#'
#' @param xpdb_s <`xpose_set`> object
#' @param ... <<[`dynamic-dots`][rlang::dyn-dots]> name of base model
#'
#' @return <`xpose_set`> object with a base model
#' @export
#'
#' @examples
#'
#' w_base <- xpdb_set %>%
#'   set_base_model(mod2)
#' w_base # base model listed in output
#'
#' get_base_model(w_base) # base model name
#'
#' unset_base_model(w_base) # base model no longer in output
#'
#'
set_base_model <- function(xpdb_s, ...) {
  # Validate input
  check_xpose_set(xpdb_s, .warn = FALSE)
  if (rlang::dots_n(...)!=1) {
    rlang::abort("There can only be one base model per set.")
  }
  curr_base <- get_base_model(xpdb_s)
  if (!is.null(curr_base)) {
    cli::cli_alert_info("Base model already set. Overwriting.")
  }


  ## Get index of new base
  base_index <- select_subset(xpdb_s, ...)

  xpdb_s %>%
    # reshape/unreshape to avoid focus conflict
    reshape_set() %>%
    dplyr::mutate(base = ifelse(dplyr::row_number() == base_index, TRUE, FALSE)) %>%
    unreshape_set() %>%
    return()
}

#' @rdname set_base_model
#' @order 2
#' @export
get_base_model <- function(xpdb_s) {
  check_xpose_set(xpdb_s, .warn = FALSE)


  base_index <- purrr::map_lgl(xpdb_s, ~.x$base) %>%
    which()

  if (length(base_index) == 0) {
    #cli::cli_alert_info("No base model found.")
    return()
  }

  return(names(xpdb_s)[base_index])
}

#' @rdname set_base_model
#' @order 3
#' @export
unset_base_model <- function(xpdb_s) {
  check_xpose_set(xpdb_s, .warn = FALSE)

  xpdb_s %>%
    # reshape/unreshape to avoid focus conflict
    reshape_set() %>%
    dplyr::mutate(base = FALSE) %>%
    unreshape_set() %>%
    return()
}

lineage_list <- function(xpdb_s) {
  check_xpose_set(xpdb_s, .warn = FALSE)


}

#' Display deltaOFV values across `xpose_set`
#'
#' @description
#' If no base model is provided, and if lineage is unclear,
#' the first model in the `xpose_set` is used as the base model.
#'
#'
#' @param xpdb_s <`xpose_set`> object
#'
#' @return <`numeric`> vector of deltaOFV values
#' @export
#' @exportS3Method base::diff
#'
#' @examples
#'
#' c()
diff.xpose_set <- function(xpdb_s) {

}



########
# Tables
########



########
# Plots
########

# This is specific enough to not need a generic
sharkfin_plot <- function() {}

# There may need to be a waterfall generic: xset_waterfall
prm_waterfall <- function() {}
eta_waterfall <- function() {}
iofv_waterfall <- function() {}

# These would just create a new xpdb in situ, then mutate, and then use xplot_scatter
ipred_vs_ipred <- function() {}
prm_vs_prm <- function() {}
eta_vs_eta <- function() {}

# This would also just be an in situ xpdb, but there may need to be a function
# for model-averaging
ipred_vs_idv_modavg <- function() {}
pred_vs_idv_modavg <- function() {}

