# This script contains the functions and other information to define a set of xpose objects.
# This set can be used to define relationships between models.
# TODO: Visualize lineage (network diagram, like the cwl package)
# TODO: add method for model comparison table

###


#' Generate a set of `xpdb` objects
#'
#' @description
#'
#' This function generates a set of xpose data (`xpdb`) objects that
#' can be used to define relationships between models. The
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> `xpdb1, xpdb2, ...` A set of `xpdb` objects to be combined into a set.
#' @param .relationships <[`list`]> A list of relationships between the `xpdb` objects. (see Details)
#' @param .as_ordered <[`logical`]> Alternative to `.relationships`, should the set of `xpdb` objects provided be considered a lineage (`grandparent, parent, child, ...`)?
#'
#' @details
#' Beyond just a list of `xpdb` objects, an `xpose_set` adds hierarchical information.
#'
#' When using `.relationships`, these should be expressed as tilde formulas, where the left-hand side is
#' children and the right and side is parents. In the simplest case, this would be `child ~ parent`, but a child can have multiple parents. This
#' syntax expects that the names for models is either declared as argument names in the call, or that the
#' variable names are directly used (i.e., not spliced or passed as an unnamed list).
#'
#'
#' @return A list of class `xpose_set`
#' @export
#'
#' @examples
#' data("xpdb_ex_pk", package = "xpose")
#'
#' # Arbitrary copy
#' xpdb_ex_pk2 <- xpdb_ex_pk
#'
#' # Simplest call
#' set1 <- xpose_set(xpdb_ex_pk, xpdb_ex_pk2)
#'
#' # With predefined relationships
#' set2 <- xpose_set(xpdb_ex_pk, xpdb_ex_pk2,
#'   .relationships = list(xpdb_ex_pk2 ~ xpdb_ex_pk)
#'   )
#'
#' # Alternative predefined relationships
#' set2b <- xpose_set(xpdb_ex_pk, xpdb_ex_pk2,
#'   .as_ordered = TRUE
#'   )
#'
#' # With custom labels
#' set3 <- xpose_set(mod1 = xpdb_ex_pk, mod2 = xpdb_ex_pk2,
#'   .relationships = list(mod2 ~ mod1)
#'   )
#'
#' # Alternative set3 using dyanmic dots
#' mod_list <- list(
#'   mod1 = xpdb_ex_pk,
#'   mod2 = xpdb_ex_pk2
#' )
#' mod_rels <- list(
#'   mod2 ~ mod1
#' )
#' set3b = xpose_set(!!!mod_list, .relationships = mod_rels)
#'
xpose_set <- function(..., .relationships = NULL, .as_ordered = FALSE) {
  # Consume dots
  xpdb_objs <- rlang::dots_list(..., .named = TRUE, .homonyms = "error")
  # If spliced, homonyms check on an unnamed list fails
  # Error noted: https://github.com/r-lib/rlang/issues/1740
  if (any( duplicated( names(xpdb_objs)))) {
    # Give more informative name to duplicated for error message
    orig_names <- names(xpdb_objs)
    names_selector <- duplicated(orig_names) | duplicated(orig_names, fromLast = TRUE)
    names(xpdb_objs)[names_selector] <- "nameless-list-element"
    # trigger the error
    rlang::dots_list(!!!xpdb_objs,.homonyms="error")
  }

  # Validation checks
  ## Make sure at least one argument is in ellipses
  if (rlang::dots_n(...)==0) rlang::abort("No xpdb objects provided.")
  ## Check each xpdb object
  for (obj in xpdb_objs) {
    rlang::try_fetch(
      xpose::check_xpdb(obj),
      error = function(s) rlang::abort("xpdb objects were not valid.", parent=s)
    )
  }
  ## Convenience for relationship error handling
  default_rels <- formals()$.relationships
  default_asord <- formals()$.as_ordered
  ## .relationships and .as_ordered are mutually exclusive
  if (!is.null(.relationships) && !identical(.as_ordered, default_asord)) {
    rlang::warn("Cannot use both .relationships and .as_ordered. Arguments dropped.")
    .relationships <- default_rels
    .as_ordered <- default_asord
  }
  ## .Relationships is expected to be a list of formulas or a single formula
  if (!is.null(.relationships) && !is.list(.relationships) && !rlang::is_formula(.relationships)) {
    rlang::warn(".relationships must be a list of formulas or a single formula. Argument dropped.")
    .relationships <- default_rels
  }
  ## As ordered should be logical
  if (!is.logical(.as_ordered)) {
    rlang::warn(".as_ordered must be logical. Argument dropped.")
    .as_ordered <- default_asord
  }

  # Output
  out <- purrr::imap(
    xpdb_objs,
    ~ {
      item <- rlang::list2(
        xpdb = as_xpdb_x(.x), # <- make sure this xpdb is already ready to be used with this package
        label = .y, # fixed
        parent = NA_character_, # vector of parents
        base = FALSE, # should changes be considered relative to this model?
        focus = FALSE, # editing in place, should changes be applied to this xpdb?
        # Other features
      )
      class(item) = c("xpose_set_item", class(item))
      item
    }
  )
  class(out) = c("xpose_set", class(out))

  # Add relationships (use associated functions to ensure consistency)
  if (!is.null(.relationships)) {
    out <- add_relationship(out, .relationships, .warn = FALSE)
  }
  if (.as_ordered) {
    mods <- names(out)
    for (i in seq_along(mods)) {
      if (i>1) out <- add_relationship(out, as.formula(paste(mods[i], "~", mods[i-1])))
    }
  }

  # Return
  out
}




#' @rdname check_xpose_set
#' @order 1
#'
#' @title Check an `xpose_set` object
#'
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param xpdb_s_i <[`xpose_set_item`][xpose_set]> An xpose_set_item object (element of an xpose_set)
#' @param .warn <`logical`> Display a warning on failure.
#' @param .example <`xpose_set`> Basis of comparison for `xpose_s_i`
#'
#' @return TRUE or error thrown
#' @export
#'
#' @examples
#'
#' check_xpose_set(xpdb_set)
#'
#' check_xpose_set_item(xpdb_set$mod1)
#'
check_xpose_set <- function(xpdb_s, .warn = TRUE) {
  # First check the obvious
  if (!inherits(xpdb_s, "xpose_set")) rlang::abort("Input must be an xpose_set object.")

  # Now make sure top-level elements are as expected
  example_to_check <- xpose_set(mod1=xpose::xpdb_ex_pk) # set example so not dependent on the xpdb_set data item, in case that changes too much
  set_test <- rlang::try_fetch(
    purrr::map(xpdb_s, check_xpose_set_item, .example=example_to_check),
    error = function(s) rlang::abort("xpose_set elements are not valid. Error details below.", parent=s)
  )

  ### Label checks
  llabells <- purrr::map_chr(xpdb_s, ~.x$label)
  # Make sure each label is unique
  if (length(unique(llabells)) != length(xpdb_s)) {
    rlang::abort("xpose_set labels are not unique.")
  }
  # Make sure each element name is its label
  if (!all(names(xpdb_s) == llabells)) {
    rlang::abort("xpose_set element names do not match their labels.")
  }
  # Warn if parents are not in set
  missing_messages <- c()
  for (elem in xpdb_s) {
    if (.warn && !all(elem$parent %in% c(llabells, NA))) {
      missing_parent <- elem$parent[!elem$parent %in% c(llabells, NA)]
      missing_messages <- c(missing_messages,
                            glue::glue("Parent(s) not in {{{{xpose_set}}}} for {cli::col_blue(elem$label)}: {missing_parent}"))
    }
  }
  if (length(missing_messages)>0) {
    cli::cli({
      for (mwarn in missing_messages) cli::cli_alert_warning(mwarn)
    })
  }


  TRUE
}
#' @rdname check_xpose_set
#' @order 2
#' @export
check_xpose_set_item <- function(xpdb_s_i, .example = xpdb_set) {
  # First check the obvious
  if (!inherits(xpdb_s_i, "xpose_set_item")) rlang::abort("Input does not seem to be part of an xpose_set object.")

  # Now make sure top-level elements are as expected
  if (!all(names(.example[[1]]) %in% names(xpdb_s_i))) {
    missing_names <- setdiff(names(.example[[1]]),names(xpdb_s_i))
    cli::cli_abort("xpose_set_item elements are not valid. Missing: {missing_names}")
  }

  # Make sure classes of top-level elements are as expected
  example_i <- .example[[1]]
  tl_elems <- names(example_i)
  for (elem in tl_elems) {
    if (!inherits(xpdb_s_i[[elem]], class(example_i[[elem]]))) {
      cli::cli_abort("xpose_set_item elements are not valid. {elem} class mismatch.")
    }
  }

  TRUE
}


#' Add one or more `xpdb` objects to an `xpose_set`
#'
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more `xpdb` objects to add to the set
#' @param .relationships <[`list`]> A list of relationships between the `xpdb` objects.
#'
#' @return An `xpose_set` object with the new `xpdb` objects added
#' @export
#'
#' @examples
#' data("xpdb_ex_pk", package = "xpose")
#'
#' add_xpdb(xpdb_set, ttt=xpdb_ex_pk)
#'
add_xpdb <- function(xpdb_s, ..., .relationships = NULL) {
  # Create set of dots
  new_set <- xpose_set(...)

  # Combine
  out <- c(xpdb_s, new_set, .relationships = .relationships)

  out
}

# set_parent <- function( xpdb_c, ..., .child_name = NULL) {
#   # Verify inputs
#   if (!is.xpdb(xpdb_c)) rlang::abort("Input must be an xpdb object.")
#   if (rlang::dots_n(...) == 0) rlang::abort("No parents provided.")
#   if (!all(purrr::map_lgl(list(...), is.xpdb))) rlang::abort("Parents must be xpdb objects.")
#
#   # Process
#   parents <- rlang::dots_list(..., .named = TRUE, .homonyms = "error")
# }

#' Add relationship(s) to an xpose_set
#' @rdname add_relationship
#' @order 1
#'
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more formulas that define relationships between models. One list of formulas can also be used, but a warning is generated.
#' @param .warn <[`logical`]> Should warnings be generated for non-formula inputs? (default: `TRUE`)
#' @param .remove <[`logical`]> Should listed relationships be removed? (default: `FALSE`)
#'
#' @return An `xpose_set` object with relationships added
#' @export
#'
#' @examples
#'
#' xpdb_set %>%
#'   add_relationship(mod1~fix2) # ouroboros
#'
#' xpdb_set %>%
#'   remove_relationship(fix1~mod2) # split down the middle
#'
add_relationship <- function(xpdb_s, ..., .warn = TRUE, .remove = FALSE) {


  rel_list <- rlang::list2(...) # List of formulas (hopefully)
  # Allow a list to be passed to ... given .relationship behavior
  if (length(rel_list)>=1 && is.list(rel_list[[1]])) {
    if (.warn) rlang::warn("List should not be used in dots, but is allowed; instead pass as arguments or pass list with !!!list.")
    rel_list <- rel_list[[1]]
  }

  # Validate input
  ## Return base object if no relationships are provided
  if (rlang::dots_n(...)==0) return(xpdb_s)
  ## Check that formulas are valid
  check_relationships(rel_list=rel_list, xpdb_s=xpdb_s)


  # Process
  rel_table <- proc_rels(rel_list)

  out <- xpdb_s %>%
    purrr::imap(~ {
      parents <- rel_table %>%
        dplyr::rowwise() %>%
        dplyr::filter(any(child == .y)) %>%
        dplyr::pull(parent) %>%
        purrr::list_c() %>%
        unique()
      if (length(parents)==0) return(.x)
      .x$parent <- c(.x$parent, parents) %>%
        unique() %>%
        # Drop NA
        .[!is.na(.)]
      if (.remove) .x$parent <- .x$parent[.x$parent != parents]
      if (length(.x$parent)==0) .x$parent <- NA_character_
      .x
    })
  class(out) = c("xpose_set", class(out))

  out
}


#' @rdname add_relationship
#' @order 2
#' @export
remove_relationship <- function(xpdb_s, ...) {
  add_relationship(
    xpdb_s = xpdb_s,
    ...,
    .remove = TRUE
  )
}

# Ensure that rel_list is a list of formulas, and that the formulas associate models within the set
check_relationships <- function(rel_list, xpdb_s) {
  # Check that relationships are valid

  # Confirm list of formulas
  if (
    length(rel_list)==0 ||
    !is_formula_list(rel_list)
  ) {
    rlang::abort("Relationships must be a list of formulas.")
  }

  # All symbols
  sym_tab <- proc_rels(rel_list)
  lhs_syms <- sym_tab$child %>% purrr::list_c() %>% unique()
  rhs_syms <- sym_tab$parent %>% purrr::list_c() %>% unique()

  # Check that all symbols are in the set (only warn)
  if (!all(c(lhs_syms,rhs_syms) %in% names(xpdb_s))) {
    missing_lhs <- lhs_syms[!lhs_syms %in% names(xpdb_s)] %>% unique()
    missing_rhs <- rhs_syms[!rhs_syms %in% names(xpdb_s)] %>% unique()
    if (length(missing_lhs)>0) cli::cli_alert_warning("Child models not in the set: {missing_lhs}")
    if (length(missing_rhs)>0) cli::cli_alert_warning("Parent models not in the set: {missing_rhs}")
  }

}

# Process relationship list
proc_rels <- function(rel_list) {
  purrr::map_dfr(
    rel_list,
    ~ {
      # Extract symbols
      lhs <- all.vars(.x[[2]])
      rhs <- all.vars(.x[[3]])
      # Create a tibble
      tibble::tibble(
        child = list(lhs),
        parent = list(rhs)
      )
    }
  )
}

# Internal
total_relationships <- function(xpdb_s) {
  # Return a count of all relationships in an xpose_set
  # TODO: This is probably slow
  xpdb_s %>%
    reshape_set() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(nn = sum(!is.na(parent))) %>%
    dplyr::ungroup() %>%
    dplyr::pull(nn) %>%
    sum()
}

#' Expose a property of xpdb objects in an xpose_set
#'
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more properties to expose
#' @param .problem <`numeric`> Problem number to apply this relationship.
#' @param .subprob <`numeric`> Problem number to apply this relationship.
#' @param .method <`numeric`> Problem number to apply this relationship.
#'
#' @return An `xpose_set` object with the properties exposed
#'
#' @details
#'
#' The property returned will be top-level, and to avoid conflicting
#' names will be prepended by `..` (e.g., `..descr`).
#'
#' For some properties, transformations are applied automatically to
#' make them more useful. This includes:
#' - `etashk` and `epsshk`: transformed to numeric vectors as in <[`get_shk`]>
#' - `ofv` and other per-problem properties: transformed as
#' needed and pulls from each `xpdb` default problem.
#'
#' @export
#'
#' @seealso [expose_param()]
#'
#' @examples
#'
#' xpdb_set <- expose_property(xpdb_set, descr)
#' xpdb_set$mod1$..descr
#'
#' xpdb_set <- expose_property(xpdb_set, etashk)
#' xpdb_set$mod1$..etashk
#'
expose_property <- function(
    xpdb_s,
    ...,
    .problem=NULL,
    .subprob=NULL,
    .method=NULL
    ) {
  rlang::check_dots_unnamed()
  # Consume dots
  props <- rlang::quos(..., .named = TRUE, .ignore_empty = "all") %>%
    names() %>%
    unique()
  # check for strings and parse if so
  props <- purrr::map_chr(props, ~{
    if (grepl("^[\"'].*[\"']$", .x)) {
      .x <- rlang::try_fetch(
        eval(parse(text=.x)),
        error= function(s) .x
      )
    }
    .x
  })
  # Default properties
  typical_summary <- (xpose::xpdb_ex_pk) %>%
    xpose::get_summary()
  nonprob_props <- typical_summary %>% dplyr::filter(problem==0) %>% dplyr::pull(label) %>% unique()
  prob_props <- typical_summary %>% dplyr::filter(problem!=0) %>% dplyr::pull(label) %>% unique()
  avail_props <- c(nonprob_props, prob_props) %>% unique()

  # Validate input
  ## Basic check
  check_xpose_set(xpdb_s, .warn = FALSE)
  ## Make sure properties can be found
  if (!all(props %in% avail_props)) {
    cli::cli_abort("Properties not available in xpdb objects: {setdiff(props, avail_props)}")
  }
  # problem subproblem check
  for (item in xpdb_s) {
    if (!is.null(.problem) && !.problem %in% xpose::all_data_problem(item$xpdb))
      cli::cli_abort("Problem no. { .problem} not in at least one xpose_data object in set.")
    if (!is.null(.subprob)) this_summ <- xpose::get_summary(item$xpdb)
    use_prob_for_check <- dplyr::coalesce(.problem, xpose::all_data_problem(item$xpdb))
    if (!is.null(.subprob) &&
        !.subprob %in% this_summ[this_summ$problem%in%use_prob_for_check,]$subprob)
      cli::cli_abort("Subproblem no. { .subprob} not associated with problem no(s). { use_prob_for_check} for
                     at least one xpose_data object in set.")
  }

  # Process
  ## Create functions to fetch the needed properties
  get_funs <- purrr::map(
      props,
      ~ {
        prop <- .x
        function(xpdb) {
          ret <- NULL
          if (prop %in% prob_props) {
            # fill problem etc info
            fill_prob_subprob_method(
              xpdb, .problem=.problem, .subprob=.subprob
            )
          }

          if (prop == "etashk") {
            ret <- get_shk(xpdb, wh="eta", .problem = .problem, .subprob = .subprob, .method = .method)
          } else if (prop == "epsshk") {
            ret <- get_shk(xpdb, wh="eps", .problem = .problem, .subprob = .subprob, .method = .method)
          } else if (prop %in% nonprob_props) {
            ret <- get_prop(xpdb, prop, .problem = 0)
          } else if (!is.na(
              suppressWarnings(as.numeric(get_prop(xpdb, prop, .problem = .problem, .subprob = .subprob, .method = .method)))
            )) {
            ret <- as.numeric(get_prop(xpdb, prop, .problem = .problem, .subprob = .subprob, .method = .method))
          }

          if (is.null(ret)) ret <- get_prop(xpdb, prop)
          if (length(ret)>1) ret <- list(ret)

          ret
        }
      }
    ) %>%
    setNames(props)

  p_xpdb_s <- xpdb_s %>%
    reshape_set() %>%
    dplyr::rowwise()

  for (prop in props)
    p_xpdb_s <- p_xpdb_s %>%
    dplyr::mutate(
      !!rlang::sym(paste0("..", prop)) := get_funs[[prop]](.data[["xpdb"]])
    )

  p_xpdb_s %>%
    dplyr::ungroup() %>%
    unreshape_set()
}


#' Expose a model parameter of xpdb objects in an xpose_set
#'
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more parameter
#' to expose, using selection rules from [`add_prm_association`].
#' @param .problem <`numeric`> Problem number to apply this relationship.
#' @param .subprob <`numeric`> Problem number to apply this relationship.
#' @param .method <`numeric`> Problem number to apply this relationship.
#'
#' @returns An `xpose_set` object with the parameter exposed
#'
#' @details
#'
#' The parameter returned will be top-level, and to avoid conflicting
#' names will be prepended by `..` (e.g., `..ome1`). The selector
#' used to fetch the parameter will be used in this `..` name. If
#' a better name is preferred, there are convenient renaming functions
#' from `dplyr` where needed.
#'
#' When using parameter selectors, quotations should be used for more
#' complex names, like `"OMEGA(1,1)"`, since these may be read incorrectly
#' otherwise.
#'
#' The untransformed parameter is used for this exposure. The `get_prm`
#' call uses `transform=FALSE`.
#'
#' @export
#'
#' @seealso [expose_property()]
#'
#' @examples
#'
#' pheno_set %>%
#'   expose_param(the1) %>%
#'   reshape_set()
#'
#'
#' pheno_set %>%
#'   expose_param(RUVADD, "OMEGA(1,1)") %>%
#'   reshape_set()
#'
#' # This function is useful for generating a model-building table
#' pheno_set %>%
#'   # Determine longest lineage
#'   select(all_of(xset_lineage(.))) %>%
#'   # Select key variability parameters
#'   expose_param(RUVADD, "OMEGA(1,1)") %>%
#'   # Make sure all models have descriptions
#'   focus_qapply(desc_from_comments) %>%
#'   # Extract description
#'   expose_property(descr) %>%
#'   # Transform to tibble
#'   reshape_set() # %>% pipe into other processing
#'
expose_param <- function(
    xpdb_s,
    ...,
    .problem=NULL,
    .subprob=NULL,
    .method=NULL
    ) {
  rlang::check_dots_unnamed()
  # Consume dots
  prms <- rlang::quos(..., .named = TRUE, .ignore_empty = "all") %>%
    names() %>%
    unique()
  # check for strings and parse if so
  prms <- purrr::map_chr(prms, ~{
    if (grepl("^[\"'].*[\"']$", .x)) {
      .x <- rlang::try_fetch(
        eval(parse(text=.x)),
        error= function(s) .x
      )
    }
    .x
  })

  # Validate input
  ## Basic check
  check_xpose_set(xpdb_s)
  # problem subproblem check
  for (item in xpdb_s) {
    if (!is.null(.problem) && !.problem %in% xpose::all_data_problem(item$xpdb))
      cli::cli_abort("Problem no. { .problem} not in at least one xpose_data object in set.")
    if (!is.null(.subprob)) this_summ <- xpose::get_summary(item$xpdb)
    use_prob_for_check <- dplyr::coalesce(.problem, xpose::all_data_problem(item$xpdb))
    if (!is.null(.subprob) &&
        !.subprob %in% this_summ[this_summ$problem%in%use_prob_for_check,]$subprob)
      cli::cli_abort("Subproblem no. { .subprob} not associated with problem no(s). { use_prob_for_check} for
                     at least one xpose_data object in set.")
  }

  # Process
  p_xpdb_s <- xpdb_s %>%
    reshape_set() %>%
    dplyr::rowwise()

  if (rlang::is_interactive()) sp <- cli::make_spinner(default_spinner)
  if (rlang::is_interactive()) sp$spin()
  pre_fetched_prms <- purrr::map(xpdb_s, ~{
    if (rlang::is_interactive()) sp$spin()
    get_prm(.x[["xpdb"]], .problem=.problem, .subprob=.subprob, transform=FALSE, quiet = TRUE)
  }) %>%
    setNames(names(xpdb_s))
  for (prm in prms) {
    p_xpdb_s <- p_xpdb_s %>%
      dplyr::mutate(
        !!rlang::sym(paste0("..", prm)) := {
          if (rlang::is_interactive()) sp$spin()
          all_prms <- pre_fetched_prms[[label]]
          rlang::try_fetch(
            pick_prm <- param_selector(prm,prm_tbl = all_prms),
            error = function(s)
              rlang::abort(paste0("Could not fetch parameter with selector `",prm,"` ",
                                  "from `xpose_data` object labeled `",label,"`."),
                           parent = s)
          )
          all_prms$value[pick_prm]
        }
      )
  }
  if (rlang::is_interactive()) sp$spin()
  if (rlang::is_interactive()) sp$finish()

  p_xpdb_s %>%
    dplyr::ungroup() %>%
    unreshape_set()
}

#' Convenience wrapper for tidyselect
#'
#' @description
#'
#' This is intended for use as an internal function to select a subset of xpdb objects from an xpose_set.
#'
#' It is a lower level version of <[`select.xpose_set`]>
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more tidyselect selectors
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#'
#' @return <[`numeric`]> vector of indices for selected xpdb objects
#'
#' @keywords internal
#' @export
#'
#' @examples
#'
#'
#' select_subset(mod2, xpdb_s=xpdb_set)
#'
#' select_subset(dplyr::starts_with("fix"), xpdb_s=xpdb_set)
#'
#'
select_subset <- function(xpdb_s, ...) {
  tidyselect::eval_select(rlang::expr(c(...)),  data = xpdb_s,
                          strict=TRUE,
                          allow_rename = FALSE,
                          allow_empty = TRUE,
                          allow_predicates = TRUE # TODO: Add predicate behavior for where()
                      )
}

#' Alternative to where() for `xpose_set`
#'
#' @noRd
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' For use when <[`where`][tidyselect::where]> might be useful. This is a work in progress.
#'
#' Unlikely to offer any benefits over <[`reshape_set`]> and then using typical <[`where`][tidyselect::where]>.
#'
#' @inheritParams tidyselect::where
#'
#' @examples
#'
#' select_subset(where_xp(~"fix1" %in% parent), xpdb_s=xpdb_set)
NULL
# where_xp <- function(fn) {
#   predicate <- rlang::as_function(fn)
#   call <- rlang::current_call()
#   function(x, ...) {
#     # Want to apply this function over columns of xpdb_set list elements (x)
#     out # TODO: WIP (currently just a copy of where()
#     tidyselect:::check_predicate_output(out, call = call)
#     out
#   }
# }

#' @title Focus on an xpdb object in an xpose_set
#'
#' @rdname focus_xpdb
#' @order 1
#'
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more xpdb objects to focus on
#' @param .add <[`logical`]> Should the focus be added to the existing focus? (default: `FALSE`)
#' @param .mods <`tidyselect`> Model names in set to quick-apply a function. See Details.
#' @param fn <`function`> to apply to focused `xpose_data` objects
#'
#' @description
#' For piping, set is passed, but with S3 method transformations are applied to the focused `xpdb` object.
#'
#' @details
#' While these functions are used internally, it is recognized that they may have value
#' in user scripting. It is hoped these are self-explanatory, but the examples should
#' address common uses.
#'
#' *Note:* `focus_qapply()` (re)focuses as specified in `.mods` and then un-focuses all elements
#' of the set so should only be used in the case where a quick application
#' suffices. Otherwise, focusing with a sequence of `focus_function` calls
#' (or a monolithic single `focus_function` call with a custom function)
#' should be preferred.
#'
#' @return An `xpose_set` object with the focused xpdb object(s)
#' @export
#'
#' @examples
#'
#' # Select two xpdb objects to focus on
#' xpdb_set %>% focus_xpdb(mod2,fix1)
#'
#' # Add a focus
#' xpdb_set %>% focus_xpdb(mod2,fix1) %>% focus_xpdb(mod1, .add=TRUE)
#'
#' # Remove focus
#' xpdb_set %>% focus_xpdb(mod2,fix1) %>% focus_xpdb()
#'
#' # Focus function and tidyselect
#' pheno_set %>%
#'   focus_xpdb(everything()) %>%
#'   # Add iOFV col and iofv type to all xpdbs in set
#'   focus_function(backfill_iofv) %>%
#'   # Show 1... can do all like this, too, but no need
#'   unfocus_xpdb() %>%
#'   select(run6) %>%
#'   {.[[1]]$xpdb} %>%
#'   list_vars()
#'
#' # Quick-apply version of previous example
#' pheno_set %>%
#'   focus_qapply(backfill_iofv) %>%
#'   select(run6) %>%
#'   {.[[1]]$xpdb} %>%
#'   list_vars()
#'
focus_xpdb <- function(xpdb_s, ..., .add = FALSE) {
  # Focus on an xpdb object in an xpose_set
  # ... is the selector for the xpdb object(s?) can be label or index. If multi-focusing is allowed,
  # then have to decide how to handle in S3 method
  # For piping, xpdb_s is passed, but with S3 method transformations are applied to the focused xpdb

  # Consumes dots
  ## Get dots
  focus_on <- names(select_subset(xpdb_s, ...))
  if (.add) focus_on <- c(focused_xpdbs(xpdb_s), focus_on)

  # Process
  out <- xpdb_s %>%
    reshape_set() %>%
    dplyr::mutate(focus = label %in% focus_on) %>%
    unreshape_set()

  out
}

#' @rdname focus_xpdb
#' @order 2
#' @export
unfocus_xpdb <- function(xpdb_s) {
  # Unfocus on an xpdb object in an xpose_set
  focus_xpdb(xpdb_s) # unfocuses
}

# Return a named vector of focused xpdb objects
#' @rdname focus_xpdb
#' @order 3
#' @export
focused_xpdbs <- function(xpdb_s) {
  reshape_set(xpdb_s) %>%
    dplyr::filter(focus) %>%
    dplyr::pull(label)
}

#' @rdname focus_xpdb
#' @order 4
#' @export
focus_function <- function(xpdb_s, fn, ...) {
  focused <- focused_xpdbs(xpdb_s)
  if (length(focused)==0) rlang::abort("No xpdb objects are focused.")

  out <- reshape_set(xpdb_s) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(xpdb = `if`(
      label %in% focused,
      fn(xpdb, ...),
      xpdb
    ) %>% list()) %>%
    dplyr::ungroup() %>%
    unreshape_set()
  return(out)
}

#' @rdname focus_xpdb
#' @order 5
#' @export
focus_qapply <- function(xpdb_s,
                         fn,
                         ...,
                         .mods = everything()) {
  xpdb_s %>%
    focus_xpdb({{.mods}}) %>%
    focus_function(fn = fn, ...) %>%
    unfocus_xpdb()
}

##### Methods

# Need methods for:
# x c() = xpose_set alias or combine two sets. Allow relationships between mods if combining two sets. Should xpose_set have methods?
# diff() = dOFV for lineage(s), if present, (feature)
# x print() = summary of models (n models, parameters, )
# duplicated() = using identical(), determine which xpdbs are duplicates
# Any methods defined for xpose_data so it can be passed through in focus_xpdb, or
#  so when unfocused, the methods can be applied to all xpdbs in the set
# x mutate() = add characteristic to list elements (top-level, like "parent", not to xpdb objects themselves)
#     For passthrough to xpdb objects, use focus_xpdb, or across() inside mutate.
# x Generic function for passing through functions to xpdb objects (filter and mutate are similar)


#' @method `[` xpose_set
#' @export
`[.xpose_set` <- function(x, i) {
  structure(
    NextMethod(),
    options = attr(x, "options"),
    class = class(x)
  )
}

#' @method print xpose_set
#' @export
print.xpose_set <- function(x, ..., n=5) {
  xpdb_s <- x
  if (length(xpdb_s)==0) {
    return(cli::cli_alert_warning("No xpdb objects in the set."))
  }
  # Print summary of xpose_set
  spinner_test <- rlang::is_interactive()
  if (spinner_test) sp <- cli::make_spinner(default_spinner)
  if (spinner_test) sp$spin()
  cli::cli({
    cli::cli_h1("{cli::col_blue('xpose_set')} object")
    cli::cli_ul()
    cli::cli_li("Number of models: {length(xpdb_s)}")
    if (length(xpdb_s)<=n) cli::cli_li("Model labels: {names(xpdb_s)}")
    if (length(xpdb_s)>n)  cli::cli_li("Model labels (truncated): {names(xpdb_s)[1:n]} (...)")
    cli::cli_li("Number of relationships: {total_relationships(xpdb_s)}")
    fnames <- focused_xpdbs(xpdb_s)
    if (length(fnames)<=n) cli::cli_li("Focused xpdb objects: {if (length(fnames)>0) fnames else 'none'}")
    if (length(fnames)>n)  cli::cli_li("Focused xpdb objects (truncated): {fnames[1:n]} (...)")
    if (spinner_test) sp$spin()
    dotnames <- purrr::map(xpdb_s, function(xpdb_s_i) {
        if (spinner_test) sp$spin()
        names(xpdb_s_i)[startsWith(names(xpdb_s_i), "..")]
      }) %>%
      purrr::flatten() %>%
      unique() %>%
      substring(3)
    cli::cli_li("Exposed properties: {if (length(dotnames)>0) dotnames else 'none'}")
    base_mod <- get_base_model(xpdb_s)
    cli::cli_li("Base model: {if (!is.null(base_mod)) cli::col_blue(base_mod) else 'none'}")
    check_xpose_set(xpdb_s, .warn=TRUE)
    # Truncation footer
    if (length(xpdb_s)>n || length(fnames)>n) {
      if (spinner_test) sp$spin()
      truncated <- c("xpdbs","focused")[c(length(xpdb_s)>n , length(fnames)>n)]
      morer <- c(length(xpdb_s) - n,length(fnames) - n)[c(length(xpdb_s)>n , length(fnames)>n)]
      more_items <- stringr::str_c(morer, " more ", truncated)
      cli::cli_bullets(cli::col_grey("# {cli::symbol$info} {more_items}"))
      cli::cli_bullets(cli::col_grey("# {cli::symbol$info} Use `print(n = ...)` to see more than n = {n}"))
    }
    cli::cli_end()
    if (spinner_test) sp$finish()
  })
}

#' @method print xpose_set_item
#' @export
print.xpose_set_item <- function(x, ...) {
  xpdb_s_i <- x
  # Print summary of xpose_set_item
  cli::cli({
    cli::cli_h1("Part of an xpose_set, with label: {cli::col_blue(xpdb_s_i$label)}")
    # Maybe some info about the parent, etc
    cli::cli_ul()
    cli::cli_li("Parent(s): {xpdb_s_i$parent}")
    cli::cli_li("In focus?: {ifelse(xpdb_s_i$focus, cli::col_green('yes'), 'no')}")
    cli::cli_li("Base model?: {ifelse(xpdb_s_i$base, cli::col_green('yes'), 'no')}")
    dotnames <- names(xpdb_s_i)[startsWith(names(xpdb_s_i), "..")]
    for (prop in dotnames) {
      cli::cli_li("{cli::col_cyan(substring(prop,3))} value: {xpdb_s_i[[prop]]}")
    }
    cli::cli_end()
    # Print the xpdb object
    cli::cli_h3("xpdb object (accessible with {cli::col_blue('{xpose_set}$',xpdb_s_i$label,'$xpdb')}):")
    if (!is_xp_xtras(xpdb_s_i$xpdb)) {
      cli::cli_verbatim(capture.output(print(xpdb_s_i$xpdb)))
    } else {
      print(xpdb_s_i$xpdb)
    }
  })
}



#' @method c xpose_set
#' @export
c.xpose_set <- function(..., .relationships = NULL) {
  # Method workaround
  .rel_saved <- .relationships
  .relationships <- NULL

  # Combine xpose_set objects
  basic_c <- NextMethod() # concatenated list
  class(basic_c) <- c("xpose_set", class(basic_c))

  # Check
  check_xpose_set(basic_c)

  # Add relationships
  .relationships <- .rel_saved  # Method workaround
  if (!is.null(.relationships)) {
    if (is.list(.relationships)) basic_c <- add_relationship(basic_c, !!!.relationships)
    if (!is.list(.relationships)) basic_c <- add_relationship(basic_c, .relationships)
  }

  basic_c
}

#' Check if any xpose_data objects are repeated in xpose_set
#'
#' @param x <`xpose_set`>
#' @param incomparables `FALSE`
#' @param ... Must be empty
#'
#' @returns A logical vector or list of logical vectors
#'
#' @method duplicated xpose_set
#' @export
duplicated.xpose_set <- function(x, incomparables=FALSE, ...) {
  rlang::check_dots_empty()
  xpdb_s <- x

  # Check
  purrr::map(xpdb_s, ~.x$xpdb) %>%
    duplicated(incomparables=incomparables)
}

#' @rdname reshape_set
#' @order 1
#'
#' @title Convert xpose_set to a nested list.
#'
#' @description
#'
#' This amounts to a convenience function for tidy manipulations.
#'
#' @param x <[`xpose_set`]> An xpose_set object
#' @param y <[`tibble`][`tibble::tibble`]> A nested table from an xpose_set
#'
#' @return <[`tibble`][`tibble::tibble`]> Nested list, or <[`xpose_set`]>
#' @export
#'
#' @examples
#'
#' rset <- reshape_set(xpdb_set)
#' # Properties (exposed and top-level) can be seen. xpdb objects are nested in the xpdb column.
#' rset %>% dplyr::select(-xpdb) %>% dplyr::glimpse()
#'
#' unreshape_set(rset)
#'
#' # The reversibility of reshaping can be confirmed:
#' identical(xpdb_set,reshape_set(xpdb_set) %>% unreshape_set())
#'
reshape_set <- function(x) {
  check_xpose_set(x, .warn=FALSE) # Warning does not need to be fired every time

  # Transpose set to a list of tibbles of each top-level element
  purrr::map(names(x[[1]]), ~ {
    tl_name <- .x
    # Get list of only the named top-level element
    purrr::map(x, ~.x[[tl_name]]) %>%
      # for single-element elements, unlist (except for special columns)
      `if`(
        all(purrr::map_dbl(., length)==1) &&
          # xpdb is always a list, and parent should be assumed to have multiple elements
          !tl_name %in% c("xpdb", "parent"),
        unlist(., recursive = FALSE, use.names = FALSE),
        .
      ) %>%
      # Transform to tibble column
      dplyr::tibble() %>%
      dplyr::rename(!!tl_name := `.`)
  }) %>%
    # Combine into a single tibble
    purrr::reduce(dplyr::bind_cols) %>%
    # Force sort error
    dplyr::select(!!!names(x[[1]]))
}

#' @rdname reshape_set
#' @order 2
#' @export
unreshape_set <- function(y) {
  # Validation
  if (!tibble::is_tibble(y)) rlang::abort("Input must be a tibble, ideally from reshape_set().")
  if (nrow(y)!=length(unique(y$label))) rlang::abort("Input must have unique labels.")

  # TODO: add identical() unit test

  # Process
  out <- y %>%
    # Index in current order
    dplyr::mutate(grp_key = forcats::as_factor(label)) %>%
    # Split
    dplyr::group_split(grp_key,.keep = TRUE) %>%
    # Cleanup
    purrr::map(~{
      ll <- as.list(.x)
      ll$grp_key <- NULL
      # Columns that are lists should be extracted
      ll <- purrr::map_if(ll, is.list, ~.x[[1]])

      class(ll) <- c("xpose_set_item", class(ll))
      ll
    }) %>%
    rlang::set_names(purrr::map_chr(., ~.x$label))
  class(out) <- c("xpose_set", class(out))

  out
}



#' @title Mutation method for xpose_set
#'
#' @param .data <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Mutations to apply to the xpose_set (passed through to <[`dplyr::mutate`]>)
#' @param .force <[`logical`]> Should top-level elements be allowed to be manipulated? (default: `FALSE`)
#' @param .retest <[`logical`]> Should the xpose_set be retested after mutation? (default: `!force`)
#' @param .rowwise <[`logical`]> Should the mutation be applied rowwise? (default: `FALSE`)
#'
#'
#' @examples
#' xpdb_set %>%
#'   # Adds foo = bar for all objects in the set
#'   mutate(foo = "bar") %>%
#'   # Reshape to visualize
#'   reshape_set()
#'
#' @importFrom dplyr mutate
#' @returns A set with updated top-level data (unless focused)
#' @rdname mutate.xpose_set
#' @export
mutate.xpose_set <- function(.data, ..., .force = FALSE, .retest = !.force, .rowwise = FALSE) {
  xpdb_s <- .data
  # Validate input
  # Basic checks
  check_xpose_set(xpdb_s, .warn = FALSE)
  # Determine if focused
  focused <- focused_xpdbs(xpdb_s)
  # Disallow any top-level elements from being manipulated like this, unless forced
  manipulations <- rlang::quos(..., named=FALSE, .ignore_empty = "all")
  example_xpdb_set <- xpose_set(xpose::xpdb_ex_pk)
  if (!.force &&
      length(focused)==0 && # doesn't matter if focused
      any(
          names(manipulations) %in% names(example_xpdb_set[[1]])
        )
      ) {
    rlang::abort("Top-level elements cannot be manipulated with mutate().")
  }

  # ** Focused output
  if (length(focused)>0) {
    return(focus_function(xpdb_s, xpose::mutate, ...))
  }

  # Typical
  out <- reshape_set(xpdb_s) %>%
    {if (.rowwise) dplyr::rowwise(.) else .} %>%
    dplyr::mutate(...) %>%
    {if (.rowwise) dplyr::ungroup(.) else .} %>%
    unreshape_set()

  # Ensure sound-ness of changes
  if (.retest) check_xpose_set(out)

  out
}

#' @title Selection method for xpose_set
#'
#' @param .data <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> (passed through to <[`select_subset`]>)
#'
#' @examples
#' xpdb_set %>%
#'   select(starts_with("fix"))
#'
#' xpdb_set %>%
#'   select(mod1, fix1)
#'
#' @importFrom dplyr select
#' @returns Subset of `xpose` set
#' @rdname select.xpose_set
#' @export
select.xpose_set <- function(.data, ...) {
  xpdb_s <- .data
  # Validate input
  # Basic checks
  check_xpose_set(xpdb_s, .warn = FALSE)


  # ** Focused output
  focused <- focused_xpdbs(xpdb_s)
  if (length(focused)>0) {
    return(focus_function(xpdb_s, xpose::select, ...))
  }

  out_cols <- select_subset(xpdb_s, ...)
  out <- xpdb_s[out_cols]

  out
}





#' @title Filtration method for xpose_set
#'
#' @param .data <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> (passed through to <[`dplyr::filter`]>)
#' @param .rowwise <[`logical`]> Should the mutation be applied rowwise? (default: `FALSE`)
#'
#' @examples
#' xpdb_set %>%
#'   filter(label=="mod1")
#'
#' xpdb_set %>%
#'   filter(length(parent)>1, .rowwise=TRUE)
#'
#' @returns A filtered `xpose_set`
#'
#' @importFrom dplyr filter
#' @rdname filter.xpose_set
#' @export
filter.xpose_set <- function(.data, ..., .rowwise = FALSE) {
  xpdb_s <- .data
  # Validate input
  # Basic checks
  check_xpose_set(xpdb_s, .warn = FALSE)


  # ** Focused output
  focused <- focused_xpdbs(xpdb_s)
  if (length(focused)>0) {
    return(focus_function(xpdb_s, xpose::filter, ...))
  }

  # Typical
  out <- reshape_set(xpdb_s) %>%
    {if (.rowwise) dplyr::rowwise(.) else .} %>%
    dplyr::filter(...) %>%
    {if (.rowwise) dplyr::ungroup(.) else .} %>%
    unreshape_set()

  out
}


#' @title Renaming method for xpose_set
#'
#' @param .data <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> (passed indirectly to <[`dplyr::mutate`]>)
#'
#' @examples
#' xpdb_set %>%
#'   rename(Mod = mod1)
#'
#'
#' @importFrom dplyr rename
#' @returns Re-labeled set
#' @rdname rename.xpose_set
#' @export
rename.xpose_set <- function(.data, ...) {
  xpdb_s <- .data
  # Validate input
  # Basic checks
  check_xpose_set(xpdb_s, .warn = FALSE)


  # ** Focused output
  focused <- focused_xpdbs(xpdb_s)
  if (length(focused)>0) {
    return(focus_function(xpdb_s, xpose::rename, ...))
  }

  # Typical
  # Make dummy tibble of current labels
  dummy <- names(xpdb_s) %>%
    purrr::map_dfc(rlang::set_names, x = list(logical())) %>%
    # rename
    dplyr::rename(...)

  out <- reshape_set(xpdb_s) %>%
    dplyr::mutate(label = names(dummy)) %>%
    unreshape_set()

  out
}


#' @title Pulling method for xpose_set
#'
#' @param .data <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> (passed through to <[`pull`][dplyr::pull]>)
#'
#' @examples
#'
#' xpdb_set %>%
#'   pull(xpdb)
#'
#' @returns The top-level information for a set requested.
#'
#' @importFrom dplyr pull
#' @rdname pull.xpose_set
#' @export
pull.xpose_set <- function(.data, ...) {
  xpdb_s <- .data
  # Validate input
  # Basic checks
  check_xpose_set(xpdb_s, .warn = FALSE)


  # (no focus check needed because xpose::pull is not a method)

  out <- xpdb_s %>%
    reshape_set() %>%
    dplyr::pull(...)

  out
}
