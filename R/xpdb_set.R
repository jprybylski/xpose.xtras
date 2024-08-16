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
      item <- rlang::list2(
        xpdb = .x,
        label = .y, # fixed
        parent = NA, # vector of parents
        # Implement getter and setter for xpdb object directly
        #descr = xpose::get_summary(xpdb_ex_pk) %>% dplyr::filter(label=="descr") %>% dplyr::pull(value),
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




#' @rdname check_xpose_set
#' @order 1
#'
#' @title Check an `xpose_set` object
#'
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param xpdb_s_i <[`xpose_set_item`][xpose_set]> An xpose_set_item object (element of an xpose_set)
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
check_xpose_set <- function(xpdb_s) {
  # First check the obvious
  if (!inherits(xpdb_s, "xpose_set")) rlang::abort("Input must be an xpose_set object.")

  # Now make sure top-level elements are as expected
  set_test <- rlang::try_fetch(
    purrr::map(xpdb_s, check_xpose_set_item),
    error = function(s) rlang::abort("xpose_set elements are not valid.", parent=s)
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
  for (elem in xpdb_s) {
    if (!all(elem$parent %in% c(llabells, NA))) {
      missing_parent(s) <- elem$parent[!elem$parent %in% c(llabells, NA)]
      cli::cli_warn("Parent(s) not in {deparse(substitute(xpdb_s))} for {cli::col_blue(elem$label)}: {missing_parent}")
    }
  }

  TRUE
}
#' @rdname check_xpose_set
#' @order 2
check_xpose_set_item <- function(xpdb_s_i) {
  # First check the obvious
  if (!inherits(xpdb_s_i, "xpose_set_item")) rlang::abort("Input does not seem to be part of an xpose_set object.")

  # Now make sure top-level elements are as expected
  if (!all(names(xpdb_set$mod1) %in% names(xpdb_s_i))) {
    rlang::abort("xpose_set_item elements are not valid.")
  }
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

  # Maybe define as
  UseMethod("set_parent") # so for focused set, can define parent
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

#' Expose a property of xpdb objects in an xpose_set
#'
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more properties to expose
#'
#' @return An `xpose_set` object with the properties exposed
#'
#' @details
#'
#' The property returned will be top-level, and to avoid conflicting
#' names will be preprended by `..` (e.g., `..descr`).
#'
#' For some properties, transformations are applied automatically to
#' make them more useful. This includes:
#' - `etashk` and `epsshk`: transformed to numeric vectors as in <[`get_shk`]>
#' - `ofv` and other per-problem properties: transformed as
#' needed and pulls from each `xpdb` default problem.
#'
#' @export
#'
#' @examples
#'
#' xpdb_set <- expose_property(xpdb_set, descr)
#' xpdb_set$mod1$..descr
#'
#' xpdb_set <- expose_property(xpdb_set, etashk)
#' xpdb_set$mod1$..etashk
#'
expose_property <- function(xpdb_s, ...) {
  # Consume dots
  props <- rlang::quos(..., .named = TRUE, .ignore_empty = "all") %>%
    names() %>%
    unique()
  # Default properties
  typical_summary <- xpdb_set[[1]]$xpdb %>%
    xpose::get_summary()
  nonprob_props <- typical_summary %>% dplyr::filter(problem==0) %>% dplyr::pull(label) %>% unique()
  prob_props <- typical_summary %>% dplyr::filter(problem!=0) %>% dplyr::pull(label) %>% unique()
  avail_props <- c(nonprob_props, prob_props) %>% unique()

  # Validate input
  ## Basic check
  check_xpose_set(xpdb_s)
  ## Make sure properties can be found
  if (!all(props %in% avail_props)) {
    cli::cli_abort("Properties not available in xpdb objects: {setdiff(props, avail_props)}")
  }

  # Process
  ## Create functions to fetch the needed properties
  get_funs <- purrr::map(
      props,
      ~ {
        prop <- .x
        function(xpdp) {
          if (prop == "etashk") return(list(get_shk(xpdp, wh="eta")))
          if (prop == "epsshk") return(list(get_shk(xpdp, wh="eps")))
          if (prop %in% nonprob_props) return(get_prop(xpdp, prop))
          if (!is.na(as.numeric(get_prop(xpdp, prop)))) return(as.numeric(get_prop(xpdp, prop)))

          get_prop(xpdp, prop)
        }
      }
    ) %>%
    setNames(props)

  p_xpdb_s <- xpdb_s %>%
    reshape_set() %>%
    # In grouped form, apply functions to individual xpdb objects
    group_by(label)

  for (prop in props)
    p_xpdb_s <- dplyr::mutate(
      p_xpdb_s,
      !!rlang::sym(paste0("..", prop)) := get_funs[[prop]](xpdb[[1]])
    )

  p_xpdb_s %>%
    ungroup() %>%
    unreshape_set()
}

#' Convenience wrapper for tidyselect
#'
#' @description
#'
#' This is intended for use as an internal function to select a subset of xpdb objects from an xpose_set.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more tidyselect selectors
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#'
#' @return <[`numeric`]> vector of indices for selected xpdb objects
#'
#' @keywords internal
#'
#' @examples
#'
#'
#' xpose.xtras:::select_subset(mod2, xpdb_s=xpdb_set)
#'
#' xpose.xtras:::select_subset(dplyr::starts_with("fix"), xpdb_s=xpdb_set)
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
#' @description
#'
#' For use when <[tidyselect::where]> might be useful.
#'
#' @inheritParams tidyselect::where
#'
#' @export
#' @examples
#'
#' xpose.xtras:::select_subset(where.xp(~"fix1" %in% parent), xpdb_s=xpdb_set)
where_xp <- function(fn) {
  predicate <- rlang::as_function(fn)
  call <- rlang::current_call()
  function(x, ...) {
    # Want to apply this function over columns of xpdb_set list elements (x)
    out_xpdb # TODO: WIP
    tidyselect:::check_predicate_output(out, call = call)
    out
  }
}


##### Methods

# Need methods for:
# c() = xpose_set alias or combine two sets. Allow relationships between mods if combining two sets. Should xpose_set have methods?
# diff() = dOFV for lineage(s), if present,
# print() = summary of models (n models, parameters, )
# duplicated() = using identical(), determine which xpdbs are duplicates
# Any methods defined for xpose_data so it can be passed through in focus_xpdb, or
#  so when unfocused, the methods can be applied to all xpdbs in the set
# mutate() = add characteristic to list elements (top-level, like "parent", not to xpdb objects themselves)
#     For passthrough to xpdb objects, use focus_xpdb, or across() inside mutate.

#' @export
print.xpose_set <- function(xpdb_s, ...) {
  # Print summary of xpose_set
  # (number of models, parameters, etc.)
  print(length(xpdb_s))
}

#' @export
`[.xpose_set` <- function(x, i) {
  structure(
    NextMethod(),
    options = attr(x, "options"),
    class = class(x)
  )
}

#' @export
print.xpose_set_item <- function(xpdb_s_i, ...) {
  # Print summary of xpose_set_item
  cli::cli_h1("Part of an xpose_set, with label: {cli::col_blue(xpdb_s_i$label)}")
  # Maybe some info about the parent, etc

  # Print the xpdb object
  cli::cli_h3("xpdb object (accessible with {cli::col_blue('{xpose_set}$',xpdb_s_i$label,'$xpdb')}):")
  print(xpdb_s_i$xpdb)
}


#' @rdname reshape_set
#' @order 1
#'
#' @title Convert xpose_set to a nested list.
#'
#' @description
#'
#' This amounts to a convenience function for tidy manupulations.
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
  check_xpose_set(x)

  # Transpose set to a list of tibbles of each top-level element
  purrr:::map(names(x[[1]]), ~ {
    tl_name <- .x
    # Get list of only the named top-level element
    purrr:::map(x, ~.x[[tl_name]]) %>%
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
    purrr:::reduce(dplyr::bind_cols) %>%
    # Force sort error
    dplyr::select(!!!names(x[[1]]))
}

#' @rdname reshape_set
#' @order 2
#'
unreshape_set <- function(y) {
  # Validation
  if (!tibble::is_tibble(y)) rlang::abort("Input must be a tibble, ideally from reshape_set().")
  if (nrow(y)!=length(unique(y$label))) rlang::abort("Input must have unique labels.")

  # TODO: add identical() unit test

  # Process
  out <- y %>%
    # Index in current order
    mutate(grp_key = forcats::as_factor(label)) %>%
    # Split
    dplyr::group_split(grp_key,.keep = TRUE) %>%
    # Cleanup
    purrr:::map(~{
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
#' @param xpdb_s <[`xpose_set`]> An xpose_set object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Mutations to apply to the xpose_set (passed through to <[`dplyr::mutate`]>)
#' @param .force <[`logical`]> Should top-level elements be allowed to be manipulated? (default: `FALSE`)
#' @param .retest <[`logical`]> Should the xpose_set be retested after mutation? (default: `!force`)
#'
#' @examples
#' xpdb_set %>%
#'   # Adds foo = bar for all objects in the set
#'   mutate(foo = "bar") %>%
#'   # Reshape to visualize
#'   reshape_set()
#'
#' @exportS3Method dplyr::mutate
mutate.xpose_set <- function(xpdb_s, ..., .force = FALSE, .retest = !.force) {
  # Validate input
  # Basic checks
  check_xpose_set(xpdb_s)
  # Disallow any top-level elements from being manipulated like this, unless forced
  if (!.force &&
      any(
          names(rlang::list2(...)) %in% names(xpdb_set[[1]])
        )
      ) {
    rlang::abort("Top-level elements cannot be manipulated with mutate().")
  }

  out <- reshape_set(xpdb_s) %>%
    dplyr::mutate(...) %>%
    unreshape_set()

  # Ensure sound-ness of changes
  if (.retest) check_xpose_set(out)

  out
}
