#' Get shrinkage estimates from model summary
#' @description
#'
#' This function parses shrinkages as they are currently
#' presented in \code{\link[xpose]{get_summary}}, so it
#' is dependent on the current implementation of that function.
#'
#' @param xpdb An \code{xpose_data} object.
#' @param wh The shrinkage to extract (`"eta"` or `"eps"`)
#' @param .problem Problem number to use. Uses the xpose default if not provided.
#'
#' @return A numeric vector of shrinkage estimates.
#' @export
#'
#' @examples
#' data("xpdb_ex_pk", package = "xpose")
#'
#' # eta Shrinkage
#' get_shk(xpdb_ex_pk)
#'
#' # epsilon Shrinkage
#' get_shk(xpdb_ex_pk, wh = "eps")
#'
#'
get_shk <- function(xpdb, wh = "eta", .problem = NULL, .subprob = NULL, .method=NULL) {

  fill_prob_subprob_method(xpdb, .problem = .problem, .subprob=.subprob, .method=.method)

  get_prop(
      xpdb = xpdb,
      prop = stringr::str_c(wh, "shk"),
      .problem = .problem,
      .subprob=.subprob,
      .method=.method
    ) %>%
    stringr::str_split(" \\[\\d+\\],? ?") %>%
    purrr::list_c() %>%
    readr::parse_double() %>%
    purrr::discard(is.na)
}


#' Generic function to extract a property from a model summary
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param prop <`character`> Property to extract
#' @param .problem <`numeric`> Problem number to use. Uses the xpose default if not provided.
#'
#' @return Exact value for the property
#' @export
#'
#' @examples
#'
#' data("xpdb_ex_pk", package = "xpose")
#'
#' get_prop(xpdb_ex_pk, "descr")
get_prop <- function(xpdb, prop, .problem = NULL, .subprob=NULL, .method=NULL) {

  fill_prob_subprob_method(xpdb, .problem = .problem, .subprob=.subprob, .method=.method)

  summ <- xpose::get_summary(xpdb)

  if (length(prop)>1) {
    rlang::abort("Request one property at a time from the xpdb model summary.")
  }
  if (!prop %in% summ$label) {
    cli::cli_abort("{cli::col_cyan(prop)} not in xpdb model summary.")
  }

  summ %>%
    dplyr::filter(label==prop) %>%
    # Include 0 for non-problem values
    dplyr::filter(problem %in% c(0,.problem), subprob  %in% c(0,.subprob)) %>%
    dplyr::pull(value)
}

#' Set a summary property
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> defining which properties to transform.
#' Argument should be valid label.
#' @param .problem <`numeric`> Problem number to use. Uses all problem if not provided.
#'
#' @return `xp_xtras` object
#' @export
#'
#' @details
#' Although one might be tempted to set custom properties using this function,
#' with the intention to maintain cross-functionality with `xpose`, users cannot
#' set a non-existent property with this function. When used internally, workarounds
#' to this semi-limitation are used.
#'
#'
#' @examples
#'
#' set_prop(xpose::xpdb_ex_pk, descr = "New model description") %>%
#'   xpose::get_summary()
#'
set_prop <- function(xpdb, ..., .problem = NULL, .subprob = NULL) {
  summ <- xpose::get_summary(xpdb)

  props_to_set <- rlang::dots_list(..., .homonyms = "error", .ignore_empty = "all")

  # Error checks
  if (any(!names(props_to_set) %in% summ$label)) {
    cli::cli_abort("Cannot set new and non-existant properties, which should be matched to labels: {setdiff(names(props_to_set), summ$label)}")
  }

  # Validate values
  check_sum <- purrr::map_lgl(props_to_set, ~ length(.x)==1)
  if (any(!check_sum)) {
    cli::cli_abort("Properties can only by set to one value. (applies to {names(props_to_set)[!check_sum]})")
  }
  if (is.null(.problem) && !is.null(.subprob))
    rlang::abort("`.problem` is needed if `.subprob` is used.")
  if (!is.null(.problem) && !is.null(.subprob) && length(.problem)<length(.subprob)) {
    # Check if recyclable
    rlang::try_fetch(
      .problem <- vctrs::vec_recycle(.problem, length(.subprob)),
      error = function(s)
        rlang::abort("`.problem` should be recyclable to match `.subprob`.", parent = s)
    )
  }
  if (!is.null(.problem) && !is.null(.subprob) && length(.problem)>length(.subprob)) {
    # Check if recyclable
    rlang::try_fetch(
      .subprob <- vctrs::vec_recycle(.subprob, length(.problem)),
      error = function(s)
        rlang::abort("`.subprob` should be recyclable to match `.problem`.", parent = s)
    )
  }

  # Convert any numeric or factors to characters for convenience
  props_to_set <- purrr::map(props_to_set, ~{
    if (is.numeric(.x) || inherits(.x, "factor")) paste(.x) else .x
  })

  check_chr <- purrr::map_lgl(props_to_set, ~ inherits(.x, "character"))
  if (any(!check_chr)) {
    cli::cli_abort("Properties can only by set to character/string values. (applies to {names(props_to_set)[!check_chr]})")
  }

  # Row update tibble
  ru_tbl <- tibble::tibble(
    label = names(props_to_set),
    value = purrr::list_c(props_to_set)
  )
  if (!is.null(.problem) && is.null(.subprob)) ru_tbl <- purrr::map_dfr(.problem, ~ dplyr::mutate(ru_tbl, problem=.x))
  if (!is.null(.problem) && !is.null(.subprob)) ru_tbl <- purrr::map2_dfr(.problem,.subprob, ~ dplyr::mutate(ru_tbl, problem=.x, subprob=.y))

  new_summ <- summ %>%
    dplyr::rows_update(
      ru_tbl,
      by = c("label", `if`(is.null(.problem), NULL, "problem"), `if`(is.null(.subprob), NULL, "subprob")),
      unmatched = "ignore"
    )
  xpdb$summary <- new_summ
  as_xpdb_x(xpdb)
}

#' Get full index for xpose_data data
#'
#' @rdname get_set_index
#' @order 1
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param .problem <`numeric`> Problem number to use. Uses the all problems if `NULL`
#' @param index <`tibble`> Index to set
#' @param ... Ignored. Here for future expansion
#'
#' @return Tibble of index
#' @export
#'
#' @examples
#' get_index(xpose::xpdb_ex_pk)
get_index <- function(xpdb, .problem=NULL, ...) {
  xpose::check_xpdb(xpdb, check = "data")
  rlang::check_dots_empty0(...)
  xp_d <- xpdb$data
  if (is.null(.problem)) .problem <- xp_d$problem
  xp_d %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      index = list(mutate(index, problem = .env$problem))
    ) %>%
    dplyr::filter(problem %in% .problem) %>%
    dplyr::pull(index) %>%
    dplyr::bind_rows()
}

#' @rdname get_set_index
#' @order 1
#' @export
set_index <- function(xpdb, index, ...) {
  xpose::check_xpdb(xpdb, check = "data")
  new_index <- index %>%
    dplyr::nest_by(problem) %>%
    dplyr::ungroup() %>%
    dplyr::rename(index=data) %>%
    dplyr::mutate(index = as.list(index))
  new_d <- dplyr::rows_update(
    xpdb$data,
    new_index,
    by = "problem"
  )
  xpdb$data <- new_d
  as_xpdb_x(xpdb)
}


#' Convenience functions used in package
#'
#' @rdname convience
#' @order 1
#'
#' @param x object to test
#'
#' @export
is_formula_list <- function(x) {
  if (!is.list(x)) return(FALSE)
  check <- purrr::map_lgl(x, rlang::is_bare_formula)
  if (length(check)==0) return(FALSE)
  all(check)
}

#' Reportable digits for model fit
#'
#' @description
#' An opinionated function where for optimization routines
#' that report number of significant digits (eg, FO-based), only
#' those number of digits are considered reportable.
#'
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param .default <`numeric`> Default number of digits to return if not found
#'
#' @return Number of reportable digits
#' @export
#'
#' @examples
#'
#' reportable_digits(xpdb_x)
#'
reportable_digits <- function(xpdb, .default = 3, .problem, .subprob, .method) {
  xpose::check_xpdb(xpdb, "summary")

  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob, .method=.method)

  digs <- rlang::try_fetch(
    floor(
      as.numeric(
        get_prop(xpdb, "nsig", .problem = .problem, .subprob = .subprob)
      )
    ),
    error = function(x) .default,
    warning = function(x) .default
    )
  if (is.na(digs)) digs <- .default
  digs
}

#' Set an `xpose` option
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Arguments in
#' the form of `option = value`
#'
#' @return `xp_xtras` object
#' @export
#'
#' @examples
#'
#' xpdb_x <- set_option(xpdb_x, quiet = TRUE)
#'

set_option <- function(xpdb, ...) {
  new_opts <- rlang::dots_list(..., .ignore_empty = "all", .homonyms = "error")

  # if cvtype is being updated, verify validity
  if ("cvtype" %in% names(new_opts))
    (function(cvtype) rlang::arg_match(cvtype, values=c("exact","sqrt")))(new_opts$cvtype)

  old_opts <- xpdb$options

  xpdb$options <- utils::modifyList(old_opts, new_opts)

  as_xpdb_x(xpdb)

}

#' Backfill utility for descriptions
#'
#' @description
#' A slightly more generic approach to getting model
#' descriptions.
#'
#'
#' @param xpdb <`xpose_data`> or <`xp_xtras`> object
#' @param start_check Regular expression used to mark start of description.
#' This is tested case-insensitively.
#' @param maxlines If the number of lines after description to the first
#' code block is more than 1, this allows a limit.
#' @param remove By default, the start check and a colon, with optional whitespace.
#' A regex.
#' @param extra_proc Any extra processing that might be desired prior to
#' collapsing the description lines. This should be a vectorized function.
#' @param collapse Character to use when collapsing multiple lines.
#'
#' @return The description-updated <`xpose_data`) object
#' @export
#'
#' @seealso [set_prop()]
#'
#' @examples
#'
#' # This has a description, but it's not visible by default
#' pheno_base
#'
#' # It can be added with the following
#' pheno_base %>%
#'   desc_from_comments()
#'
#' # Extra processing for preference can also implemented
#' pheno_base %>%
#'   desc_from_comments(extra_proc = tolower)
#'
#' # If a run label ($PROB) would make a good description, use the
#' # following instead:
#' pkpd_m3 %>%
#'   set_prop(descr=get_prop(pkpd_m3,"label"))
#'
#'
desc_from_comments <- function(
    xpdb,
    start_check = ".*description",
    maxlines=5,
    remove = paste0(start_check,":\\s*"),
    extra_proc = c,
    collapse = " "
    ) {
  xpose::check_xpdb(xpdb, "code")
  if (!is.function(extra_proc))
    cli::cli_abort("`extra_proc` must be a function, not a {.strong {class(extra_proc)[1]}}")

  code <- xpose::get_code(xpdb)
  first_end <- which(code$subroutine!="oth")[1]-1 # line before end
  first_start <- which(grepl(start_check, code$comment, ignore.case = TRUE))[1]
  if (is.na(first_start) || is.na(first_end) || first_end<first_start) {
    cli::cli_warn("Cannot find a valid description in code.")
    return(xpdb)
  }
  # comments from first start to first end
  start_end_comments <- code$comment[first_start:first_end] %>%
    # strip comment character, if any
    gsub("^;\\s*","", .)
  # Action remove
  start_end_comments[1] <- start_end_comments[1] %>%
    stringr::str_replace(stringr::regex(remove, ignore_case = TRUE), "")
  # Last processing
  new_descr <- start_end_comments %>%
    .[1:min(length(.),maxlines)] %>%
    # Remove empty
    .[!.==""] %>%
    # extra
    extra_proc() %>%
    # Collapse
    paste(collapse=collapse)
  if (is.na(new_descr) || new_descr=="") {
    cli::cli_warn("Cannot find a valid description in code.")
    return(xpdb)
  }

  set_prop(xpdb, descr = new_descr)
}


#' Place .problem, .subprob and .method into environment consistently
#'
#' @description
#' Since this is a common need, it is being functionalized
#' to ensure consistency.
#'
#'
#' @param xpdb <`xpose_data`> or related object
#' @param .problem `NULL` or missing
#' @param .subprob `NULL` or missing
#' @param .method `NULL` or missing
#' @param envir <`environment`> in which to assign the problem info.
#'
#' @keywords Internal
#'
fill_prob_subprob_method <- function(xpdb, .problem, .subprob, .method, envir=parent.frame()) {
  # Do generic checks for .problem, .subprob and .method, push to envir
  xpose::check_xpdb(xpdb, check = "files")

  if (!any(xpdb$files$extension == "ext")) {
    rlang::abort("File extension `ext` needed and is missing.")
  }

  if (missing(.problem) || is.null(.problem))
    .problem <- xpose::last_file_problem(xpdb, ext = "ext")
  if (missing(.subprob) || is.null(.subprob))
    .subprob <- xpose::last_file_subprob(xpdb, ext = "ext", .problem = .problem)
  if (missing(.method) || is.null(.method))
    .method <- xpose::last_file_method(xpdb, ext = "ext", .problem = .problem,
                                       .subprob = .subprob)

  assign(".problem", .problem, envir = envir)
  assign(".subprob", .subprob, envir = envir)
  assign(".method", .method, envir = envir)
  return()
}
