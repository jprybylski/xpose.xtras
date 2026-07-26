# Functions related to visualizing and diagnosing colinearity

#' Extract a parameter covariance or correlation matrix
#'
#' @description
#' Pulls the uncertainty (covariance step) matrix for estimated parameters,
#' with built-in support for `nonmem` (via the `.cov`/`.cor` output tables)
#' and `nlmixr2` (via the fit object's covariance matrix) models. This is
#' what feeds [`cormat()`], but is exported separately since it may be
#' useful on its own (eg, for programmatic checks on parameter colinearity).
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param type <`character`> Either `"correlation"` (default) or `"covariance"`
#' @param .problem <`numeric`> Problem number to use. Uses the xpose default
#' if not provided. Ignored for `nlmixr2` models.
#' @param .subprob <`numeric`> Subproblem number to use. Uses the xpose
#' default if not provided. Ignored for `nlmixr2` models.
#' @param .method <`character`> Method to use. Uses the xpose default if not
#' provided. Ignored for `nlmixr2` models.
#' @param drop_fixed <`logical`> Drop fixed (or otherwise not estimated)
#' parameters from the matrix. See Details.
#' @param quiet <`logical`> Silence extra debugging output
#'
#' @details
#' For `nonmem` models, the matrix is built from the `.cor`/`.cov` output
#' tables produced by the `$COV` step. NONMEM includes fixed-effect
#' parameters in these tables as placeholder zeros (since no uncertainty is
#' estimated for them); `drop_fixed` (the default) removes them.
#'
#' For `nlmixr2` models, uncertainty is only calculated for fixed-effect
#' (`theta`) parameters; `nlmixr2` does not report standard errors, and
#' therefore no covariance/correlation, for random-effect (`omega`)
#' elements. `drop_fixed` has no effect here, since parameters without a
#' standard error are already excluded from the fit's covariance matrix.
#'
#' In both cases, if the covariance step was not run, or did not complete
#' successfully, an informative error is raised rather than returning
#' partial or placeholder data.
#'
#' @return A symmetric numeric matrix, with parameter names as `dimnames`.
#' @export
#'
#' @examples
#' get_cov_matrix(xpdb_x)
#' get_cov_matrix(xpdb_x, type = "covariance")
#'
#' \dontrun{
#' xpdb_nlmixr2 <- nlmixr_example("xpdb_nlmixr2")
#' get_cov_matrix(xpdb_nlmixr2)
#' }
get_cov_matrix <- function(
    xpdb,
    type = c("correlation", "covariance"),
    .problem = NULL,
    .subprob = NULL,
    .method = NULL,
    drop_fixed = TRUE,
    quiet) {
  xpose::check_xpdb(xpdb, check = FALSE)
  type <- rlang::arg_match(type, values = c("correlation", "covariance"))
  if (missing(quiet)) quiet <- xpdb$options$quiet

  mat <- if (xpose::software(xpdb) == "nonmem") {
    get_cov_matrix_nonmem(
      xpdb,
      type = type, .problem = .problem, .subprob = .subprob,
      .method = .method, drop_fixed = drop_fixed, quiet = quiet
    )
  } else if (xpose::software(xpdb) == "nlmixr2") {
    get_cov_matrix_nlmixr2(xpdb, type = type, quiet = quiet)
  } else {
    cli::cli_abort("Extracting a covariance/correlation matrix is not implemented for {.strong {xpose::software(xpdb)}} models.")
  }

  # By long-standing NONMEM convention (followed by nlmixr2 too), the
  # diagonal of a .cor-style matrix holds the parameter's standard error,
  # not 1. Fix it up here so callers get an actual correlation matrix.
  if (type == "correlation") diag(mat) <- 1
  mat
}

# Build the matrix from NONMEM's .cor/.cov output tables
get_cov_matrix_nonmem <- function(xpdb, type, .problem, .subprob, .method, drop_fixed, quiet) {
  ext <- switch(type, correlation = "cor", covariance = "cov")
  raw <- rlang::try_fetch(
    xpose::get_file(
      xpdb,
      ext = ext, .problem = .problem, .subprob = .subprob,
      .method = .method, quiet = quiet
    ),
    error = function(cnd) NULL
  )
  if (is.null(raw) || nrow(raw) == 0) {
    cli::cli_abort(c(
      "No {type} matrix available for this model.",
      "i" = "The covariance step ({.code $COV}) may not have been run, or may not have completed successfully."
    ))
  }

  mat <- as.matrix(raw[, colnames(raw) != "NAME"])
  rownames(mat) <- raw$NAME

  if (isTRUE(drop_fixed)) {
    prm_tbl <- get_prm(
      xpdb,
      show_all = TRUE, .problem = .problem, .subprob = .subprob,
      .method = .method, quiet = TRUE
    )
    keep <- !prm_tbl$fixed[match(rownames(mat), prm_tbl$name)]
    keep[is.na(keep)] <- TRUE # be permissive if a name fails to match
    mat <- mat[keep, keep, drop = FALSE]
  }
  mat
}

# Build the matrix from an nlmixr2 fit's covariance/correlation matrix
get_cov_matrix_nlmixr2 <- function(xpdb, type, quiet) {
  assert_nlmixr2fit(xpdb)
  # Check $cov first: unlike $cor, it is a plain stored value (NULL if the
  # covariance step didn't run) rather than something computed on access, so
  # it's safe to check before ever touching $cor (which errors, rather than
  # returning NULL, if there is no covariance matrix to derive it from).
  if (is.null(xpdb$fit$cov)) {
    cli::cli_abort(c(
      "No {type} matrix available for this model.",
      "i" = "The covariance step may not have been run (see {.arg covMethod} in {.fun nlmixr2est::foceiControl}), or may not have completed successfully."
    ))
  }
  mat <- switch(type, correlation = xpdb$fit$cor, covariance = xpdb$fit$cov)
  as.matrix(mat)
}

#' Parameter correlation/covariance matrix heatmap
#'
#' @description
#' Visualizes the parameter correlation (or covariance) matrix as a heatmap,
#' filling a gap left behind in translation from `xpose4`. Values come from
#' [`get_cov_matrix()`], which has built-in support for `nonmem` and
#' `nlmixr2` models; rendering is done with the generic [`xplot_heatmap()`]
#' template.
#'
#' @param xpdb <`xp_xtras`> or <`xpose_data`> object
#' @param type <`character`> Either `"correlation"` (default) or `"covariance"`
#' @param .problem <`numeric`> Problem number to use. Uses the xpose default if not provided.
#' @param .subprob <`numeric`> Subproblem number to use. Uses the xpose default if not provided.
#' @param .method <`character`> Method to use. Uses the xpose default if not provided.
#' @param drop_fixed <`logical`> Passed to [`get_cov_matrix()`]
#' @param digits Number of significant digits to display in cell labels. Defaults to [`reportable_digits()`]
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param caption Plot caption
#' @param tag Plot tag
#' @param quiet Silence extra debugging output
#' @param ... Additional aesthetics, passed to [`xplot_heatmap()`]
#'
#' @details
#' Only the upper triangle of the matrix is drawn, as it is symmetric.
#' Fixed-effect (`theta`) and random-effect (`omega`/`sigma`) parameters are
#' both included for `nonmem` models, when available and not fixed. For
#' `nlmixr2` models, only fixed effects are included, as `nlmixr2` does not
#' report uncertainty for random effects. See [`get_cov_matrix()`] for
#' further details on availability; if the covariance step was not run, or
#' did not complete successfully, an informative error is raised.
#'
#' @return The desired plot
#' @export
#'
#' @examples
#' cormat(xpdb_x)
#' cormat(xpdb_x, type = "covariance")
cormat <- function(
    xpdb,
    type = c("correlation", "covariance"),
    .problem = NULL,
    .subprob = NULL,
    .method = NULL,
    drop_fixed = TRUE,
    digits,
    title,
    subtitle = "Ofv: @ofv, Condition number: @condn",
    caption = "@dir",
    tag = NULL,
    quiet,
    ...) {
  xpose::check_xpdb(xpdb, check = FALSE)
  type <- rlang::arg_match(type, values = c("correlation", "covariance"))
  if (missing(quiet)) quiet <- xpdb$options$quiet
  if (missing(title)) title <- paste0(stringr::str_to_title(type), " matrix | @run")
  if (missing(digits)) {
    digits <- reportable_digits(xpdb, .problem = .problem, .subprob = .subprob, .method = .method)
  }

  # Fetch cov/cor matrix (errors informatively if unavailable)
  mat <- get_cov_matrix(
    xpdb,
    type = type, .problem = .problem, .subprob = .subprob,
    .method = .method, drop_fixed = drop_fixed, quiet = quiet
  )
  if (nrow(mat) < 2) {
    rlang::abort("At least two estimated parameters are required to plot a correlation/covariance matrix.")
  }

  prm_tbl <- get_prm(
    xpdb,
    show_all = TRUE, .problem = .problem, .subprob = .subprob,
    .method = .method, quiet = TRUE
  )
  pretty_name <- function(nm) {
    lbl <- prm_tbl$label[match(nm, prm_tbl$name)]
    typ <- prm_tbl$type[match(nm, prm_tbl$name)]
    ifelse(
      is.na(lbl) | lbl == "",
      nm,
      sprintf("%s[%s]", toupper(typ), lbl)
    )
  }
  dimnames(mat) <- list(pretty_name(rownames(mat)), pretty_name(colnames(mat)))

  # Only the upper triangle is needed, since the matrix is symmetric
  mat[lower.tri(mat, diag = TRUE)] <- NA_real_

  fill_limits <- if (type == "correlation") c(-1, 1) else max(abs(mat), na.rm = TRUE) * c(-1, 1)
  fill_name <- stringr::str_to_title(type)

  xp <- xplot_heatmap(
    xpdb,
    mat = mat,
    digits = digits,
    limits = fill_limits,
    midpoint = 0,
    legend_name = fill_name,
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    plot_name = "cormat",
    quiet = quiet,
    ...
  )

  xp$xpose$problem <- attr(prm_tbl, "problem")
  xp$xpose$subprob <- attr(prm_tbl, "subprob")
  xp$xpose$method <- attr(prm_tbl, "method")
  xp
}
