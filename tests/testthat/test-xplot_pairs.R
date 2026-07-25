test_that("xplot_pairs", {

  data("xpdb_ex_pk", package = "xpose", envir = environment())

  # Declare postprocessing so only columns needed are plotted
  opt_xp <- xpose::data_opt(.problem = 1,
                              filter = xpose::only_distinct(xpdb_ex_pk, 1, NULL, TRUE),
                              post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED")))
  opt_xtra <- xpose::data_opt(.problem = 1,
                              filter = xpose::only_distinct(xpdb_ex_pk %>% as_xpdb_x(), 1, NULL, TRUE),
                              post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED")))

  # test both xpdb_x xpdb_ex_pk produce the same plot (d/t filling theme)
  wo_xpx <- xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE)
  w_xpx <- xpdb_ex_pk %>% as_xpdb_x() %>% xplot_pairs(opt=opt_xtra, quiet = TRUE)

  ## Other
  opt_xtra <- xpose::data_opt(.problem = 1,
                              filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
                              post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED")))

  suppressMessages(expect_equal(
    xpdb_x$options$quiet,
    xplot_pairs(xpdb_x, opt=opt_xtra)$xpose$quiet
  ))
  expect_message(
    xplot_pairs(xpdb_x, opt=opt_xtra),
    "Using data from"
  )

  ## Check error catching
  expect_error(
    xplot_pairs(xpdb_x, opt=opt_xtra, quiet = TRUE,
                contcont_opts = list(other_fun=1)),
    "function.*not a.*numeric"
  )
  expect_error(
    xplot_pairs(xpdb_x, opt=opt_xtra, quiet = TRUE,
                catcont_opts = list(other_fun=1)),
    "function.*not a.*numeric"
  )

  #### vdiffr tests to skip on CRAN
  skip_on_cran()
  skip_if(!is.null(getOption("ignore_vdiff")) && getOption("ignore_vdiff"))
  skip_on_covr()
  skip()
  library(vdiffr)

  expect_doppelganger("from xpose_data", wo_xpx) # expect same as snapshot
  expect_doppelganger("from xp_xtra", w_xpx) # expect same as snapshot
  expect_doppelganger("from xpose_data", w_xpx) # expect same as xpose_data snapshot

  ## Expect options to make plot change (not dissecting specific changes)

  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               cont_opts=list(guide=TRUE))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               cont_opts=list(type="p"))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               dist_opts=list(type="d"))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               cat_opts=list(type="vl"))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               contcont_opts = list(stars=TRUE))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               catcont_opts = list(stars=TRUE))
  ))
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               catcat_opts = list(use_rho=FALSE))
  ))


  # test ggtheme basic check
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               gg_theme = xpose::theme_bw2())
  ))

  # xp-theme basic check
  expect_failure(expect_doppelganger(
    "from xpose_data",
    xpdb_ex_pk %>% xplot_pairs(opt=opt_xp, quiet = TRUE,
                               xp_theme = xpose::theme_xp_xpose4())
  ))


})

test_that("xplot_pairs errors on empty data", {
  opt_empty <- xpose::data_opt(post_processing = function(x) x[0, ])
  expect_error(
    xplot_pairs(xpdb_x, opt = opt_empty, quiet = TRUE),
    "No data available"
  )
})

test_that("xplot_pairs validates that _opts arguments are lists", {
  opt_xtra <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED"))
  )

  expect_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, cont_opts = "notalist"),
    "`cont_opts` must be a list"
  )
  expect_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, dist_opts = "notalist"),
    "`dist_opts` must be a list"
  )
  expect_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, cat_opts = "notalist"),
    "`cat_opts` must be a list"
  )
  expect_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, contcont_opts = "notalist"),
    "`contcont_opts` must be a list"
  )
  expect_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, catcont_opts = "notalist"),
    "`catcont_opts` must be a list"
  )
  expect_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, catcat_opts = "notalist"),
    "`catcat_opts` must be a list"
  )
})

test_that("xplot_pairs falls back to xpose::data_opt() when opt is missing", {
  # Restrict to a handful of continuous/categorical columns directly on the
  # xpdb (rather than through `opt$post_processing`) so that omitting `opt`
  # entirely (hitting the `missing(opt)` branch) doesn't blow up on the
  # high-cardinality ID column present in the full dataset.
  xpdb_small <- xpdb_x
  xpdb_small$data$data[[1]] <- xpdb_small$data$data[[1]] %>%
    dplyr::select(ETA1, ETA2, ETA3, MED1, MED2)

  p <- expect_no_error(xplot_pairs(xpdb_small, quiet = TRUE))
  expect_s3_class(p, "xp_xtra_plot")
})

test_that("xplot_pairs accepts gg_theme and xp_theme overrides and renders", {
  opt_xtra <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED"))
  )

  p_gg <- expect_no_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, gg_theme = xpose::theme_bw2())
  )
  p_xp <- expect_no_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, xp_theme = xpose::theme_xp_xpose4())
  )

  # Actually rendering the grid (rather than just constructing the lazy
  # ggmatrix) is what exercises the wrapped_scatter/wrapped_dist/wrapped_box
  # panel functions and print.xp_xtra_plot() itself.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p_gg))
  expect_no_error(print(p_xp, page = 1)) # exercise the (no-op) `page` arg path
})

test_that("xplot_pairs renders with a valid contcont_opts$other_fun", {
  opt_xtra <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED"))
  )
  myfun <- GGally::wrap("cor")

  p <- expect_no_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, contcont_opts = list(other_fun = myfun))
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))
})

test_that("xplot_pairs catcont_opts$other_fun is honored in the upper combo cell", {
  opt_xtra <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED"))
  )
  # Must accept the combo (categorical x, continuous y) signature that
  # GGally::ggpairs() passes to upper-panel cat/cont cells; GGally::wrap("cor")
  # is not combo-compatible since stats::cor() requires numeric x.
  myfun <- GGally::wrap("statistic", text_fn = function(x, y) {
    as.character(round(stats::cor.test(as.numeric(y), as.numeric(x), method = "kendall")$estimate, 2))
  }, title = "Kendall tau")

  p <- expect_no_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, catcont_opts = list(other_fun = myfun))
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))
})

test_that("xplot_pairs catcat_opts$use_rho = FALSE uses the count upper-panel and renders", {
  opt_xtra <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED"))
  )

  p <- expect_no_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, catcat_opts = list(use_rho = FALSE))
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))
})

test_that("xplot_pairs strips a NULL other_fun default when contcont_opts/catcont_opts are partially specified", {
  # `use_upt()` (via modifyList) only carries over the *default* other_fun =
  # NULL entry (rather than dropping it outright) when the user's `_opts`
  # list omits "other_fun" entirely, e.g. by supplying some other field. This
  # exercises the `contcont_opts <- within(contcont_opts, rm(other_fun))`
  # cleanup branch (and its catcont_opts analog).
  opt_xtra <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED"))
  )

  expect_no_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, contcont_opts = list(stars = TRUE))
  )
  expect_no_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, catcont_opts = list(title = "rho"))
  )
})

test_that("xplot_pairs renders catcont_opts$stars = TRUE (rho_fun star annotation)", {
  opt_xtra <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED"))
  )

  p <- expect_no_error(
    xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE, catcont_opts = list(stars = TRUE))
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))
})

test_that("xplot_pairs falls back to xp_theme$labeller when pairs_labeller is absent", {
  opt_xtra <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED"))
  )

  xpdb_mod <- xpdb_x
  xpdb_mod$xp_theme$pairs_labeller <- NULL
  expect_false("pairs_labeller" %in% names(xpdb_mod$xp_theme))

  p <- expect_no_error(xplot_pairs(xpdb_mod, opt = opt_xtra, quiet = TRUE))
  expect_s3_class(p, "xp_xtra_plot")
})

test_that("xplot_pairs wrapped_box hits both combo orientations depending on column order", {
  # By default (ETA columns before MED columns) the lower-triangle combo
  # panels always have the continuous variable on x and the categorical
  # variable on y (var_x numeric / var_y factor). Reordering the columns so a
  # categorical variable precedes a continuous one flips a lower-triangle
  # panel to var_x factor / var_y numeric, exercising the other branch of
  # wrapped_box()'s orientation logic.
  opt_reordered <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("MED"), starts_with("ETA"))
  )

  p <- expect_no_error(xplot_pairs(xpdb_x, opt = opt_reordered, quiet = TRUE))

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))
})

test_that("print.xp_xtra_plot delegates to NextMethod() for non-ggmatrix objects", {
  x <- structure(1:3, class = c("xp_xtra_plot", "integer"))
  expect_output(print(x), "1 2 3")
})

test_that("print.xp_xtra_plot exercises the legacy ggplot2 (<= 3.5.2) label branch", {
  # Mirrors the packageVersion() mocking approach used in
  # test-xplot_boxplot.R to reach the pre-3.5.2 ggplot2 code path, which is
  # otherwise unreachable with the ggplot2 version installed in this
  # environment.
  opt_xtra <- xpose::data_opt(
    .problem = 1,
    filter = xpose::only_distinct(xpdb_x, 1, NULL, TRUE),
    post_processing = function(x) dplyr::select(x, starts_with("ETA"), starts_with("MED"))
  )
  p <- xplot_pairs(
    xpdb_x, opt = opt_xtra, quiet = TRUE,
    title = "T", subtitle = "S", caption = "C", tag = "G"
  )

  testthat::local_mocked_bindings(
    packageVersion = function(pkg) {
      if (identical(pkg, "ggplot2")) package_version("3.5.0") else utils::packageVersion(pkg)
    },
    .package = "utils"
  )

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(print(p))

  # Without any title/subtitle/caption/tag supplied, tr_vals()'s `xx` is
  # NULL, exercising its early-return guard.
  p_notitle <- xplot_pairs(xpdb_x, opt = opt_xtra, quiet = TRUE)
  expect_true(is.null(p_notitle$title))
  expect_no_error(print(p_notitle))
})
