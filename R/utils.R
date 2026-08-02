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
#' @param .subprob <`numeric`> Subproblem number to use. Uses the xpose default if not provided.
#' @param .method <`character`> Method to use. Uses the xpose default if not provided.
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

#' Recalculate eta shrinkage from individual estimates
#'
#' @description
#'
#' Unlike [`get_shk()`], which parses the eta shrinkage NONMEM itself
#' reported in the output file, this recalculates shrinkage directly from
#' the individual (empirical Bayes) eta estimates found in the data, using
#' the standard \eqn{100 \times (1 - SD(\eta)/\omega)} formula, where
#' \eqn{\omega} is the standard deviation implied by the associated
#' diagonal omega estimate.
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> or `xp_xtras` object
#' @param ... <`tidyselect`> Which eta column(s) to recalculate shrinkage
#' for. Defaults to every `eta` column for `.problem`.
#' @param .etastype <`numeric(1)`> `1` (the default) excludes, for each eta,
#' individuals whose estimate is a "true zero" (exactly `0`, as opposed to
#' merely shrunk near it) from the calculation; `0` keeps them. See Details.
#' @param .problem <`numeric`> Problem number to use. Uses the xpose default if not provided.
#' @param .subprob <`numeric`> Subproblem number to use. Uses the xpose default if not provided.
#' @param .method <`character`> Method to use. Uses the xpose default if not provided.
#' @param drop_fixed <`logical`> Drop fixed etas (which have no meaningful
#' shrinkage to recalculate), as in [`xpose::drop_fixed_cols`].
#' @param quiet <`logical`> Silence extra debugging output
#'
#' @details
#' An eta is a "true zero" for an individual when NONMEM never had grounds
#' to move it away from its prior mean of `0`, eg an individual with no
#' observations contributing to the objective function. That is different
#' from an eta that is merely shrunk close to `0` through legitimate
#' estimation, and including "true zero" individuals in the shrinkage
#' calculation biases it, since they carry no information about the actual
#' empirical distribution of etas. `.etastype = 1` (the default) excludes
#' them from the calculation; `.etastype = 0` reproduces the traditional,
#' unadjusted calculation.
#'
#' @return A tibble with one row per eta, reporting the omega used, the
#' number of individuals excluded (if any), and the recalculated
#' shrinkage (as a percentage, to stay consistent with [`get_shk()`]).
#' @export
#'
#' @examples
#' recalc_shk(xpdb_x)
#'
#' # Just a subset of etas...
#' recalc_shk(xpdb_x, ETA1)
#'
#' # Including "true zero" etas in the calculation
#' recalc_shk(xpdb_x, .etastype = 0)
#'
recalc_shk <- function(xpdb, ..., .etastype = 1, .problem = NULL, .subprob = NULL,
                        .method = NULL, drop_fixed = TRUE, quiet) {
  xpose::check_xpdb(xpdb, check = "data")
  if (missing(quiet)) quiet <- xpdb$options$quiet
  checkmate::assert_choice(.etastype, choices = c(0, 1))

  fill_prob_subprob_method(xpdb, .problem = .problem, .subprob = .subprob, .method = .method)

  all_eta_cols <- xpose::xp_var(xpdb, .problem, type = "eta")$col
  if (length(all_eta_cols) == 0) {
    cli::cli_abort("No {.field eta} columns found for problem {.problem}.")
  }

  dots <- rlang::enquos(...)
  if (length(dots) == 0) {
    eta_col <- all_eta_cols
  } else {
    eta_col <- dplyr::select(
      xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
      !!!dots
    ) %>%
      names() %>%
      unique()
  }
  if (drop_fixed) {
    eta_col <- xpose::drop_fixed_cols(xpdb, .problem, cols = eta_col, quiet = quiet)
  }
  if (is.null(eta_col) || length(eta_col) == 0) {
    cli::cli_abort("No usable {.field eta} column found in the xpdb data index.")
  }
  if (any(!eta_col %in% all_eta_cols)) {
    cli::cli_abort("`...` should only select {.field eta} columns, which does not seem to apply to: {setdiff(eta_col, all_eta_cols)}")
  }

  # Match each eta to its diagonal omega. Etas aren't always numbered, and
  # even when they are, the number isn't always meaningful (eg `nlmixr2`
  # eta columns are named after their parameter, like `eta.cl`, and don't
  # relate to `m`/`n` matrix position at all). Try a direct name match
  # first -- this is what makes `nlmixr2` models work, since `get_prm()`
  # reports the eta's own column name in `name` for those -- then fall
  # back to NONMEM's `ETA<k>`/`ETA(k)` <-> `OMEGA(k,k)` numbering
  # convention, which is meaningful there even though `name`/`label`
  # don't otherwise match the eta column name.
  om_diag <- get_prm(xpdb, .problem = .problem, .subprob = .subprob, .method = .method, quiet = TRUE) %>%
    dplyr::filter(type == "ome", diagonal == TRUE)

  om_idx <- match(eta_col, om_diag$name)
  need_num <- is.na(om_idx)
  if (any(need_num)) {
    eta_num <- suppressWarnings(as.integer(stringr::str_extract(eta_col[need_num], "\\d+")))
    om_idx[need_num] <- match(eta_num, om_diag$m)
  }
  if (anyNA(om_idx)) {
    cli::cli_abort(c(
      "Could not associate the following eta column(s) with a diagonal omega: {eta_col[is.na(om_idx)]}",
      "i" = "Matching is tried by column name (eg for {.field nlmixr2} models), then by NONMEM's {.field ETA<k>}/{.field ETA(k)} numbering convention; neither applied here."
    ))
  }
  om_val <- as.numeric(om_diag$value[om_idx])

  id_col <- xpose::xp_var(xpdb, .problem, type = "id")$col[1]
  ind_data <- xpose::get_data(xpdb, .problem = .problem, quiet = TRUE) %>%
    dplyr::distinct(.data[[id_col]], .keep_all = TRUE) %>%
    dplyr::select(dplyr::all_of(eta_col))

  purrr::pmap_dfr(
    list(eta_col, om_val),
    function(col, om) {
      eta_vals <- ind_data[[col]]
      n_total <- length(eta_vals)
      if (.etastype == 1) eta_vals <- eta_vals[eta_vals != 0]
      n_used <- length(eta_vals)
      if (n_used < 2) {
        cli::cli_abort("Not enough non-excluded individuals ({n_used}) to recalculate shrinkage for {.field {col}}.")
      }
      tibble::tibble(
        problem = .problem,
        subprob = .subprob,
        method = .method,
        eta = col,
        omega = om,
        n = n_total,
        n_excluded = n_total - n_used,
        shrinkage = 100 * (1 - stats::sd(eta_vals) / sqrt(om))
      )
    }
  )
}

#' Derive per-individual contribution to eta shrinkage
#'
#' @description
#'
#' Computes, for each selected eta, a diagnostic column highlighting each
#' individual's contribution to shrinkage: \eqn{\log((\eta_i -
#' \bar\eta)^2)}, ie the log of the squared deviation from the population
#' mean eta. Since shrinkage itself is `100 * (1 - SD(eta)/omega)` (see
#' [`recalc_shk()`]), and `SD(eta)^2` is the mean of these per-individual
#' squared deviations, this highlights which individuals are pulling
#' shrinkage down. The `log` spreads out values close to `0`, ie
#' individuals contributing the least (the most heavily shrunk).
#'
#' `derive_shk()` returns the augmented data as a plain data frame, like
#' [`xpose::get_data()`]'s output. `backfill_shk()` joins the new
#' column(s) back into `xpdb` and tags them with the `shk` variable type.
#' This has to be backfilled rather than parsed, since it isn't something
#' NONMEM (or any other supported software) reports directly.
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> or `xp_xtras` object
#' @param ... <`tidyselect`> Which eta column(s) to derive a shrinkage
#' contribution for. Defaults to every `eta` column for `.problem`.
#' @param .problem <`numeric`> Problem number to use. Uses the xpose default if not provided.
#' @param quiet <`logical`> Silence extra debugging output
#'
#' @return For `derive_shk()`, a data frame with one new column per
#' selected eta, named `<eta>_SHK`. For `backfill_shk()`, the updated
#' `xp_xtras` object, with those columns joined in and typed `shk`.
#' @export
#' @rdname derive_shk
#'
#' @examples
#' derive_shk(xpdb_x) %>%
#'   dplyr::select(ID, dplyr::ends_with("_SHK")) %>%
#'   head()
#'
#' xpdb_x %>%
#'   backfill_shk() %>%
#'   list_vars()
#'
derive_shk <- function(xpdb, ..., .problem = NULL, quiet) {
  xpose::check_xpdb(xpdb, check = "data")
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(.problem)) .problem <- xpose::default_plot_problem(xpdb)

  all_eta_cols <- xpose::xp_var(xpdb, .problem, type = "eta")$col
  if (length(all_eta_cols) == 0) {
    cli::cli_abort("No {.field eta} columns found for problem {.problem}.")
  }

  dots <- rlang::enquos(...)
  eta_col <- if (length(dots) == 0) {
    all_eta_cols
  } else {
    dplyr::select(
      xpose::get_data(xpdb, .problem = .problem, quiet = TRUE),
      !!!dots
    ) %>%
      names() %>%
      unique()
  }
  if (any(!eta_col %in% all_eta_cols)) {
    cli::cli_abort("`...` should only select {.field eta} columns, which does not seem to apply to: {setdiff(eta_col, all_eta_cols)}")
  }

  id_col <- xpose::xp_var(xpdb, .problem, type = "id")$col[1]
  dat <- xpose::get_data(xpdb, .problem = .problem, quiet = TRUE)

  shk_col <- stringr::str_c(eta_col, "_SHK")
  dupe <- intersect(shk_col, names(dat))
  if (length(dupe) > 0) {
    cli::cli_abort("Column(s) already present in the data, refusing to overwrite: {dupe}")
  }

  for (i in seq_along(eta_col)) {
    ind_vals <- dplyr::distinct(dat, .data[[id_col]], .data[[eta_col[i]]])[[eta_col[i]]]
    dat[[shk_col[i]]] <- log((dat[[eta_col[i]]] - mean(ind_vals))^2)
  }
  dat
}

#' @rdname derive_shk
#' @export
backfill_shk <- function(xpdb, ..., .problem = NULL, quiet) {
  xpose::check_xpdb(xpdb, check = "data")
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (is.null(.problem)) .problem <- xpose::default_plot_problem(xpdb)

  derived <- derive_shk(xpdb, ..., .problem = .problem, quiet = quiet)
  shk_col <- setdiff(names(derived), names(xpdb$data$data[[.problem]]))

  xpdb$data$data[[.problem]] <- derived
  xpdb$data <- xpose::xpdb_index_update(xpdb = xpdb, .problem = .problem)
  xpdb %>%
    as_xpdb_x() %>%
    set_var_types_x(.problem = .problem, shk = dplyr::all_of(shk_col))
}


#' Generic function to extract a property from a model summary
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param prop <`character`> Property to extract
#' @param .problem <`numeric`> Problem number to use. Uses the xpose default if not provided.
#' @param .subprob <`numeric`> Subproblem number to use. Uses the xpose default if not provided.
#' @param .method <`character`> Method to use. Uses the xpose default if not provided.
#' @param .tail <`numeric`> Length of terminal values to pull when there are more than 1 result
#'
#' @return Exact value for the property
#' @export
#'
#' @examples
#'
#' data("xpdb_ex_pk", package = "xpose")
#'
#' get_prop(xpdb_ex_pk, "descr")
get_prop <- function(xpdb, prop, .problem = NULL, .subprob=NULL, .method=NULL, .tail=1) {
  rlang::try_fetch(
    checkmate::assert_scalar(.problem, null.ok = TRUE),
    error = function(s) rlang::abort("", parent=s)
  )
  rlang::try_fetch(
    checkmate::assert_scalar(.subprob, null.ok = TRUE),
    error = function(s) rlang::abort("", parent=s)
  )
  rlang::try_fetch(
    checkmate::assert_scalar(.method, null.ok = TRUE),
    error = function(s) rlang::abort("", parent=s)
  )
  rlang::try_fetch(
    checkmate::assert_scalar(prop),
    error = function(s) rlang::abort("Request one property at a time from the xpdb model summary.", parent=s)
  )

  summ <- xpose::get_summary(xpdb)


  fill_prob_subprob_method(xpdb, .problem = .problem, .subprob=.subprob, .method=.method, for_summary = TRUE)

  if (!prop %in% summ$label) {
    cli::cli_abort("{cli::col_cyan(prop)} not in xpdb model summary.")
  }

  # Pull subset of summary with the prop
  prop_ <- summ %>%
    dplyr::filter(label==prop)


  # If there is only one value to return, return in
  if (nrow(prop_)==1)
    return(prop_$value)

  # If all problem and subprob numbers are 0, return value
  if (all(prop_$problem==0) && all(prop_$subprob==0))
    return(prop_$value)

  # If not, return property value(s) that match.
  # Include 0 as some properties (eg, for simulations) will have NA subprob
  prop_ %>%
    dplyr::filter(problem == .problem, subprob %in% c(0,.subprob)) %>%
    {
      if (nrow(.)==0) cli::cli_abort("No summary item matching .problem { .problem}, .subprob { .subprob} and .method { .method}")
      .
    } %>%
    dplyr::pull(value) %>%
    tail(.tail)
}

#' Set a summary property
#'
#' @param xpdb <`xpose_data`[xpose::xpose_data]> object
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> defining which properties to transform.
#' Argument should be valid label.
#' @param .problem <`numeric`> Problem number to use. Uses all problem if not provided.
#' @param .subprob <`numeric`> Subproblem number to use. Uses the xpose default if not provided.
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
#' @rdname convenience
#' @order 1
#'
#' @returns `<logical>` `TRUE` if is a list of formulas
#'
#' @param x object to test
#' @keywords internal
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
#' @param .problem <`numeric`> Problem number to use. Uses all problem if not provided.
#' @param .subprob <`numeric`> Subproblem number to use. Uses the xpose default if not provided.
#' @param .method <`character`> Method to use. Uses the xpose default if not provided.
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

  fill_prob_subprob_method(xpdb, .problem=.problem, .subprob=.subprob, .method=.method, for_summary = TRUE)

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
#' @description
#' Sets one or more entries in `xpdb$options`, merged in via
#' [utils::modifyList()] -- which recurses into list-valued options, so
#' setting a single key of an existing named-list option (e.g. one label
#' of `default_labs`, see [set_default_labs()]) leaves its other keys
#' untouched rather than replacing the whole list. This is what
#' [set_default_labs()] and [set_default_watermark()] are built on, and
#' calling `set_option()` directly with `default_labs`/`default_watermark`
#' behaves the same way.
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
  if (xpose::software(xpdb)!="nonmem")
    cli::cli_abort("This function is only relevant to nonmem objects.")
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
#' @param for_summary <`logical`> If used for summary functions, subprob needs to be adjusted for zero-indexing
#'
#' @keywords internal
#'
fill_prob_subprob_method <- function(xpdb, .problem, .subprob, .method, envir=parent.frame(), for_summary = FALSE) {

  summ <- xpose::get_summary(xpdb) %>%
    # Both filling approaches (to match nonmem approach) only work for estimations
    dplyr::filter(label=="method")


  # If subprob is NULL and method is not, determine subprob for method
  if ((missing(.subprob) || is.null(.subprob)) && !(missing(.method) || is.null(.method))) {
    # Find the summary column that matches the method argument
    all_methods <- summ %>%
      dplyr::filter(label=="method") %>%
      dplyr::filter(stringr::str_detect(value, tolower(.method)))
    # If .problem is not null, also filter on that
    if (!(missing(.problem) || is.null(.problem))) all_methods <- dplyr::filter(all_methods, problem==.problem)
    if (nrow(all_methods)==0)
      cli::cli_abort("No method {.method} found in summary.")
    # Pick last subprob
    .subprob <- tail(all_methods$subprob, 1)
    # Need to add 1 unless returning for summary, to be consistent with expectations when .method not provided
    .subprob <- .subprob + !for_summary
  }

  # If not nonmem, just use reasonable defaults that are consistent with nonmem approach
  if (xpose::software(xpdb)!="nonmem") {
    summ <- xpose::get_summary(xpdb) %>%
      # nonmem approach only selects estimations
      dplyr::filter(label=="method")

    if (nrow(summ)==0) {
      cli::cli_warn("Model from {.strong {xpose::software(xpdb)}} may not be compatible with {package_flex()}.")
      assign(".problem", 0, envir = envir)
      assign(".subprob", 0, envir = envir)
      assign(".method", "", envir = envir)
      return()
    }

    if (missing(.problem) || is.null(.problem)) {
      .problem <- tail(summ$problem,1)
      summ <- dplyr::filter(summ, problem==.problem)
    }
    if (missing(.subprob) || is.null(.subprob)) {
      .subprob <- tail(summ$subprob,1)
      summ <- dplyr::filter(summ, subprob==.subprob)
    }
    if (missing(.method) || is.null(.method))
      .method <- tail(summ$value,1)

    assign(".problem", .problem, envir = envir)
    assign(".subprob", .subprob, envir = envir)
    assign(".method", .method, envir = envir)

    return()
  }

  # Do generic checks for .problem, .subprob and .method, push to envir
  xpose::check_xpdb(xpdb, check = "files")

  # Proceed for nonmem
  if (!any(xpdb$files$extension == "ext")) {
    rlang::abort("File extension `ext` needed and is missing.")
  }


  if (missing(.problem) || is.null(.problem))
    .problem <- xpose::last_file_problem(xpdb, ext = "ext")
  if (missing(.subprob) || is.null(.subprob)) {
    .subprob <- xpose::last_file_subprob(xpdb, ext = "ext", .problem = .problem)
    #  If this function interacts with xpose summary (as usual),
    #  .subprob needs to be 0-indexed instead of 1-indexed
    return_subprob <- .subprob - as.integer(for_summary) #
  } else {
    return_subprob <- .subprob
  }
  if (missing(.method) || is.null(.method))
    .method <- xpose::last_file_method(xpdb, ext = "ext", .problem = .problem,
                                       .subprob = .subprob)

  assign(".problem", .problem, envir = envir)
  assign(".subprob", return_subprob, envir = envir)
  assign(".method", .method, envir = envir)
  return()
}


#' Logical instead of exception for xpose data check
#'
#' @inheritParams xpose::check_xpdb
#'
#' @keywords internal
#' @export
#'
test_xpdb <- function(
    xpdb, check="data"
) {
  test_check <- purrr::safely(xpose::check_xpdb)(xpdb=xpdb, check=check)
  is.null(test_check$error)
}


#' Mutate the file table for an xpose data object
#'
#' @param xpdb <`xpose data`> object
#' @param ... Forwarded to [mutate()][dplyr::mutate()]
#'
#' @keywords internal
#'
mutate_files <- function(xpdb, ...) {
  xpose::check_xpdb(xpdb)
  if (!test_xpdb(xpdb, "files")) return(xpdb)
  revert_to_xpdb_fn <- if (is_xp_xtras(xpdb)) as_xp_xtras else xpose::as.xpdb

  xpdb$files <- dplyr::mutate(xpdb$files, ...)
  revert_to_xpdb_fn(xpdb)
}
