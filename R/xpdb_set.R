# This script contains the functions and other information to define a set of xpose objects.
# This set can be used to define relationships between models.

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
  if (!is.null(.relationships) && !is.list(.relationships) && !is.formula(.relationships)) {
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
      rlang::list2(
        xpdb = .x,
        parent = NULL,
        label = .y,
        # Implement getter and setter for xpdb object directly
        #descr = xpose::get_summary(xpdb_ex_pk) %>% dplyr::filter(label=="descr") %>% dplyr::pull(value),
        base = FALSE, # should changes be considered relative to this model?
        focus = FALSE, # editing in place, should changes be applied to this xpdb?
        # Other features
      )
    }
  )
  class(out) = c("xpose_set", class(out))

  # Add relationships (use associated functions to ensure consistency)
  if (!is.null(.relationships)) {
    if (is.list(.relationships))    out <- add_relationship(out, !!!.relationships)
    if (is.formula(.relationships)) out <- add_relationship(out, .relationships)
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

add_xpdb <- function(xpdb_s, ..., .parent = NULL) {
  # Add an xpdb object to an xpose_set
}

focus_xpdb <- function(xpdb_s, ...) {
  # Focus on an xpdb object in an xpose_set
  # ... is the selector for the xpdb object(s?) can be label or index. If multi-focusing is allowed,
  # then have to decide how to handle in S3 method
  # For piping, xpdb_s is passed, but with S3 method transformations are applied to the focused xpdb
}

unfocus_xpdb <- function(xpdb_s) {
  # Unfocus on an xpdb object in an xpose_set
}

set_parent <- function( xpdb_c, ...) {
  # Assign one or more parents to an xpose object and return an xpose_set
  # (wrapper for xpose_set with .relationships)
}

#' Add relationship(s) to an xpose_set
#'
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more formulas that define relationships between models. One list of formulas can also be used, but a warning is generated.
#'
#' @return An `xpose_set` object with relationships added
#' @export
#'
#' @examples
#'
#' c()
#'
add_relationship <- function(xpdb_s, ...) {


  rel_list <- rlang::list2(...) # List of formulas (hopefully)
  # Allow a list to be passed to ... given .relationship behavior
  if (length(rel_list)==1 && is.list(rel_list[[1]])) {
    rlang::warn("List should not be used in ..., but is allowed; instead pass as arguments or pass list with !!!list.")
    rel_list <- rel_list[[1]]
  }

  # Validate input
  ## Return base object if no relationships are provided
  if (rlang::dots_n(...)==0) return(xpdb_s)
  ## Check that formulas are valid
  check_relationships(rel_list=rel_list, xpdb_s=xpdb_s)


  # Process
  formulas <- rlang::list2(...) # List of formulas

  rlang::inform("add_relationship not yet implemented. Formulas are:")
  for (form in formulas) rlang::inform(Reduce(paste, deparse(form)))

  xpdb_s
}

check_relationships <- function(rel_list, xpdb_s) {
  # Check that relationships are valid
  ## ensure that rel_list is a list of formulas, and that the formulas associate models within the set
}

# Need methods for:
# c() = xpose_set alias or combine two sets,
# diff() = dOFV for lineage(s), if present,
# print() = summary of models (n models, parameters, )
# duplicated() = using identical(), determine which xpdbs are duplicates
# Any methods defined for xpose_data so it can be passed through in focus_xpdb, or
#  so when unfocused, the methods can be applied to all xpdbs in the set
