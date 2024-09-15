xset_waterfall <- function(
    xpdb_s,
    ...,
    .inorder=FALSE,
    type = "plt",
    .col = NULL,
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    tag      = NULL,
    ylab = NULL,
    xlab = NULL,
    opt,
    facets = NULL,
    .problem,
    .subprob,
    .method,
    quiet
) {

  # process dots
  two_set_dots(xpdb_s, ..., .inorder=.inorder)
  # Now have mod1 and mod2

  # Default to the settings for mod1
  if (missing(.problem)) .problem <- xpose::default_plot_problem(mod1$xpdb)
  if (missing(.subprob))
    .subprob <- xpose::last_file_subprob(mod1$xpdb, ext = "ext", .problem = .problem)
  if (missing(.method))
    .method <- xpose::last_file_method(mod1$xpdb, ext = "ext", .problem = .problem,
                                       .subprob = .subprob)
  if (missing(quiet)) quiet <- mod1$xpdb$options$quiet

  # Selected column
  sel_col <- xpdb_f$data %>%
    dplyr::select({{.col}}) %>%
    names()
  if (length(sel_col)>1)
    rlang::abort("Can only select one column in a waterfal plot.")

  # Get combined xpdb
  xpdb_f <- franken_xpdb(
    mod1$xpdb,
    mod2$xpdb,
    .cols = .col,
    problem = .problem,
    prop_transforms = function(xpdb_f, xpdb_list, problem) {
      # Combine dir
      franken_prop(
        xpdb_f = xpdb_f,
        xpdb_list = xpdb_list,
        prop = "dir")
    }
  )

  # Data options
  combined_columns <- paste0(sel_col, "_", 1:2)

  ##### V   Below has not been updated

  # Check if user tries to pass symbol to facets instead of character
  if ((rlang::quo_is_symbol(rlang::enquo(facets)) &&
       !exists(deparse(substitute(facets)), envir = parent.frame())) ||
      (!is.character(facets) &&
       !is.null(facets))) {
    cli::cli_abort("Facets should be a simple character vector.")
  }
  post_processing_cov <- function(x) x
  if (facets %in% xp_var(xpdb_f, .problem = .problem, type = "catcov")$col) {
    # TODO: use apply_lul_wide here for facets, but will have to be sure the new name is captured
  }
  post_processing <- function(df) {
    df %>%
      dplyr::mutate(
        dOFV = .data[[ofv_columns[2]]] - .data[[ofv_columns[1]]]
      ) %>%
      # Ensure sort
      dplyr::arrange(-abs(dOFV)) %>%
      # Sum (maybe faceted) total dOFV
      dplyr::group_by(across(dplyr::any_of(facets))) %>%
      dplyr::group_modify(~{
        dplyr::mutate(.x,
                      total_dOFV = sum(dOFV)
        )
      }) %>%
      dplyr::group_by(dOFV>0, .add = TRUE) %>%
      dplyr::mutate(
        nind = 1:dplyr::n(),
        OFV = total_dOFV - cumsum(dOFV)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        nind = ifelse(dOFV==0, 0, nind),
        OFV = ifelse(dOFV==0, total_dOFV, OFV)
      )
  }
  def_opt <- xpose::data_opt(.problem = .problem, .subprob = .subprob, .method = .method,
                             filter = xpose::only_distinct(xpdb_f, .problem, facets, quiet),
                             post_processing = post_processing)
  if (missing(opt)) opt <- def_opt
  # Only some data options should be used
  data <- xpose::fetch_data(xpdb_f, quiet = quiet, .problem = def_opt$problem, .subprob = def_opt$subprob,
                            .method = def_opt$method, .source = opt$source, simtab = opt$simtab,
                            filter = def_opt$filter, tidy = FALSE, index_col = NULL,
                            value_col = NULL, post_processing = def_opt$post_processing)
  if (is.null(data) || nrow(data) == 0) {
    rlang::abort('No data available for plotting. Please check the variable mapping and filering options.')
  }

}

# There may need to be a waterfall generic: xset_waterfall
prm_waterfall <- function() {}
eta_waterfall <- function() {}
iofv_waterfall <- function() {}
