# breaks test_coverage. comment for now
.onAttach <- function(...) {
  if (!is_attached("xpose")) {
    cli::cli_inform(c("i"="{.strong {cli::col_blue('xpose')}} is not currently attached."), class = "packageStartupMessage")
  }

  if (!is_loading_for_tests()) {
    conflicted::conflict_prefer_all("xpose.xtras", c("xpose","stats"), quiet=TRUE)
  }


}

# These functions are from tidyverse
is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

is_loading_for_tests <- function() {
  !rlang::is_interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "xpose.xtras")
}


# Remove CRAN note on no visible binding for global variable
utils::globalVariables(c(
  '.',
  "problem",
  "subprob",
  "method",
  ".env",
  ".data",
  "variable",
  "rn",
  "this_lvls",
  "value",
  "old_name",
  "everything",
  "where",
  "index",
  "extension",
  "data",
  ":=",
  "type",
  "xpdb_set",
  "na.omit",
  "label",
  "ofv",
  "..ofv",
  "mod1",
  "mod2",
  "m1col",
  "m2col",
  "focus",
  "param",
  "omega",
  "thnums",
  "cv",
  "m",
  "n",
  "pdf",
  "dev.off",
  "mods",
  "fixed",
  "across",
  "model",
  "extension",
  "modifyList",
  "sd",
  "value",
  "variable",
  "value_weight",
  "id_order",
  "probs",
  "xpdb",
  "dOFV",
  "total_dOFV",
  "nind",
  "OFV",
  "parent",
  "nn",
  "grp_key",
  "new_variable",
  "%RSE",
  "SE",
  "diagonal",
  "est",
  "eta",
  "etatrans",
  "fix",
  "formula",
  "ignore",
  "n",
  "x",
  "name",
  "neta1",
  "neta2",
  "ntheta",
  "plogis",
  "prm_assoc_formula",
  "probit",
  "probitInv",
  "qlogis",
  "rse",
  "se",
  "theta",
  "thetatrans",
  "xpdb_x",
  "n.x"
))
