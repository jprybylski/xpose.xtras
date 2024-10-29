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

#' Display deltaOFV values across `xpose_set`
#'
#' @description
#' If no base model is provided, and if lineage is unclear,
#' the first model in the `xpose_set` is used as the base model.
#'
#'
#' @param x <`xpose_set`> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Passed to <[`xset_lineage`]>.
#' `.spinner=FALSE` can also be set here.
#'
#' @return <`numeric`> vector of deltaOFV values
#' @export
#' @exportS3Method base::diff
#'
#' @examples
#'
#' c()
diff.xpose_set <- function(x, ...) {
  xpdb_s <- x
  lineage <- xset_lineage(xpdb_s, ...)

  exposed_ofv <- expose_property(xpdb_s, ofv)

  dofv_fun <- function(line) {
    exposed_ofv %>%
      select(!!line) %>%
      pull(..ofv) %>%
      diff()
  }

  if (is.list(lineage)) return(purrr::map(lineage, dofv_fun))

  dofv_fun(lineage)
}

#' Determine lineage within a set
#'
#' @param xpdb_s <`xpose_set`> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> labels for
#' models in the set from which to create lineages (will result in a list
#' if multiple labels are used). If empty,
#' lineage from base model will be output; if no base, first listed
#' model will be used. Always used the most senior model in this list.
#' @param .spinner Set to `FALSE` to not show a loading spinner in interactive mode.
#'
#' @return <`character`> vector of `c('base', 'base child',
#' 'base grandchild', ...)` or list thereof, depending on dots arguments.
#' @export
#'
#' @details
#' This function uses a not-especially-optimized tree-searching algorithm
#' to determine the longest lineage starting from whatever is treated as
#' the base model. It is based loosely on <[`pluck_depth`][purrr::pluck_depth]>,
#' but the values at each depth are maintained.
#' As such, for larger sets this function and, more importantly,
#' functions that use it may take some time.
#'
#' @importFrom rlang is_interactive
#'
#' @examples
#' \dontrun{
#'
#' xset_lineage(xpdb_set)
#'
#' set_base_model(xpdb_set, fix1) %>%
#'   xset_lineage()
#'
#' xset_lineage(xpdb_set, fix1)
#'
#' }
xset_lineage <- function(xpdb_s, ..., .spinner=NULL) {
  check_xpose_set(xpdb_s, .warn = FALSE)

  # Check for base model
  basemod <- get_base_model(xpdb_s)

  spinner_test <- is_interactive() && !isFALSE(.spinner)
  sp <- FALSE
  if (is.null(.spinner)) sp <- NULL
  if (spinner_test && is.null(.spinner)) sp <- cli::make_spinner(default_spinner)
  if (spinner_test && !is.null(.spinner)) sp <- .spinner
  if (spinner_test) sp$spin()

  # Process dots
  if (rlang::dots_n(...)>=1) {
    out_list <- select_subset(xpdb_s, ...) %>%
      purrr::map(~{
        if (spinner_test) sp$spin()
        set_base_model(xpdb_s, all_of(.x)) %>%
          xset_lineage(.spinner = FALSE)
      })
    if (spinner_test) sp$finish()
    if (length(out_list)==1) return(out_list[[1]])
    return(out_list)
  } else if (is.null(basemod)) {
    out <- set_base_model(xpdb_s, all_of(1)) %>%
      xset_lineage(.spinner = sp)
    return(out)
  }

  # The default, where base mod is established
  find_child <- child_finder(xpdb_s)
  longest_line <- function(parent, found_parents=NULL) {
    if (spinner_test) sp$spin()
    ch <- find_child(parent)
    if (any(ch %in% found_parents)) ch <- ch[!ch %in% found_parents]
    if (length(ch)==0) return(parent)
    lines <- purrr::map(ch, longest_line, found_parents = c(found_parents, parent))
    line_lens <- purrr::map_int(lines, length)
    longests <- lines[line_lens==max(line_lens)]
    # pick first if tie
    c(parent, longests[[1]])
  }

  out <- longest_line(basemod)

  if (spinner_test) sp$finish()

  return(out)
}
interactive <- NULL

child_finder <- function(xpdb_s) {
  parent_list <- reshape_set(xpdb_s)$parent
  function(parent) {
    # Return the children or null
    children <- purrr::imap_chr(parent_list, ~{
      ifelse(parent %in% .x, .y, NA_character_)
    }) %>%
      na.omit() %>%
      as.character()
    if (length(children)==0) return(NULL)
    if (length(children)==1) return(children)
    return(children)
  }
}

#' Binary check if LHS is parent of LHS
#'
#' @param possible_parent <`xpose_set_item`> object suspected as parent to ...
#' @param possible_child  ... <`xpose_set_item`> object suspected child
#'
#' @export
#'
#' @examples
#'
#' # Detect direct parent
#' pheno_set$run6 %p% pheno_set$run7
#'
#' # Detect non-parentage (does not try to "flip" parentage)
#' pheno_set$run6 %p% pheno_set$run5
#'
#' # Does not detect grand-parentage
#' pheno_set$run6 %p% pheno_set$run13
#'
`%p%` <- function(possible_parent, possible_child) {
  check_xpose_set_item(possible_parent)
  check_xpose_set_item(possible_child)

  all_parents <- possible_child$parent
  if (length(all_parents)==0 || is.na(all_parents)) return(FALSE)

  possible_parent$label %in% all_parents
}

