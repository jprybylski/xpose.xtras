# breaks test_coverage. comment for now
.onAttach <- function(...) {
  if (!is_attached("xpose")) {
    cli::cli_alert_info(paste("{.strong {cli::col_blue('xpose')}} is not currently attached."))
  }

  if (!is_loading_for_tests()) {
    conflicted::conflict_prefer_all("xpose.xtras", "xpose", quiet=TRUE)
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
utils::globalVariables('.')
