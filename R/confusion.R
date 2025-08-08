
#' Calculate all binary confusion matrix indices
#'
#' @param test_vec <`double`> Vector of probabilities
#' @param true_vec <`integer`> Vector true values
#' @param ... For future extension
#' @param threshold <`double`> Number that defines probability as positive.
#' @param pos_val <`integer`> Positive value in `true_vec`
#' @param prepend <`character`> Preprend column names with this to prevent name conflicts
#'
#' @returns dataframe of confusion matrix indices
#' @keywords internal
confmatr_by_threshold <- function(
    test_vec,
    true_vec,
    ...,
    threshold=0.5,
    pos_val=1,
    prepend = "",
    cols = dplyr::everything()) {
  checkmate::assert_numeric(test_vec)
  checkmate::assert_integerish(true_vec)
  checkmate::assert_numeric(threshold)
  checkmate::assert_count(pos_val)
  checkmate::assert_string(prepend)
  dplyr::bind_rows(
    purrr::map(threshold, function(.x) {
      dplyr::tibble(
        threshold = .x,
        P = sum(true_vec==pos_val),
        TP = sum(test_vec>=.x & true_vec==pos_val),
        FN = sum(test_vec<.x & true_vec==pos_val),
        N = sum(true_vec!=pos_val),
        FP = sum(test_vec>=.x & true_vec!=pos_val),
        TN = sum(test_vec<.x & true_vec!=pos_val)
      )
    })
  ) %>%
    dplyr::mutate(
      TPR = TP/P,
      SEN = TPR,
      FNR = 1 - TPR,
      FPR = FP/N,
      TNR = 1-FPR,
      SPC = TNR,
      BM = TPR+TNR-1,
      PT = (sqrt(TPR*FPR) - FPR)/(TPR - FPR),
      Prevalence = P/(P+N),
      PPV = TP/(TP + FP),
      NPV = TN/(TN+FN),
      LRp=TPR/FPR,
      LRn=FNR/TNR,
      ACC = (TP+TN)/P+N,
      FDR = 1 - PPV,
      FOR = 1 - NPV,
      MK = PPV + NPV - 1,
      deltaP = MK,
      DOR = LRp/LRn,
      BA = (TPR + TNR)/2,
      F_1 = (2*PPV*TPR)/(PPV + TPR),
      FM = sqrt(PPV*TPR),
      MCC = sqrt(TPR*TNR*PPV*NPV) - sqrt(FNR*FPR*FOR*FDR),
      TS = TP/(TP+FN+FP),
      CSI = TS
    ) %>%
    dplyr::select({{ cols }}) %>%
    dplyr::rename_with(~paste0(prepend,.x))
}

# pre-planned plots
# ROC plot faceted by levels of catdv
roc_plot <- function(xpdb,
                        mapping  = NULL,
                        cutpoint = 1,
                        group    = 'ID',
                        type     = 'ca',
                        title    = 'ROC curve | @run',
                        subtitle = 'Ofv: @ofv, Eps shrink: @epsshk',
                        caption  = '@dir',
                        tag      = NULL,
                        guide    = TRUE,
                        facets,
                        .problem,
                        quiet,
                        ...) {
  # Check input
  xpose::check_xpdb(xpdb, check = 'data')
  if (missing(.problem)) .problem <- xpose::default_plot_problem(xpdb)
  xpose::check_problem(.problem, .subprob = NULL, .method = NULL)
  if (missing(quiet))   quiet <- xpdb$options$quiet
  if (missing(facets))  facets <- xpdb$xp_theme$facets

  # Get relevant columns (or throw error)
  dvprob_cols <- xp_var(xpdb, .problem, type = 'dvprobs')$col
  catdv_cols <- xp_var(xpdb, .problem, type = 'catdv')$col
  if (length(catdv_cols)>1) {
    cli::cli_warn("Only one categorical DV will be used ({catdv_cols[1]}).")
    catdv_cols <- catdv_cols[1]
  }

  cp <- make_catdv_cutpoint(xpdb, .problem, catdv_cols, cutpoint)
  # Use catdv_vs function but with a slight modification
  post_processing <- function (df) {
    cp$post_process(df) %>%
      dplyr::mutate(!!catdv_cols := as.numeric(.data[[catdv_cols]])-1)
  }


  ## For some reason the summary data for the xpdb is getting lost
  ## Not @dir, but @ofv and @epsshk
  xplot_rocplot(
    xpdb = xpdb, group = NULL, quiet = quiet,
    opt = xpose::data_opt(
      .problem = .problem,
      filter = xpose::only_obs(xpdb, .problem, quiet),
      post_processing = post_processing
    ),
    mapping = mapping,
    type = type, facets = facets,
    title = title, subtitle = subtitle, caption = caption,
    tag = tag, plot_name = stringr::str_remove(deparse(match.call()[[1]]), "(\\w+\\.*)+::"),
    like_col = cp$prob_col,
    obs_col = catdv_cols,
    obs_target = 1,
    ...
  )
}

# ROC space plot where oper chars per individual are plotted at the default threshold
roc_space <- function() {}

# Curve (faceted) or space (no default facet) plot comparing models
roc_by_mod <- function() {}

### Other confusion matrix ideas
### Confusion indexes as new var type
### Confusion index versus cov or eta
