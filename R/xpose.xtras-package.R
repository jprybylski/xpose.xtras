#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom ggplot2 %+%
#' @importFrom ggplot2 aes
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom lifecycle deprecated
#' @importFrom stats as.formula
#' @importFrom stats formula
#' @importFrom stats na.omit
#' @importFrom stats plogis
#' @importFrom stats qlogis
#' @importFrom stats quantile
#' @importFrom stats rnorm
#' @importFrom stats sd
#' @importFrom stats setNames
#' @importFrom utils capture.output
#' @importFrom utils data
#' @importFrom utils modifyList
#' @importFrom utils tail
## usethis namespace: end
NULL


default_spinner <- "dots"

package_flex <- cli::col_magenta(paste(cli::style_bold("~"), "xp_xtras"))
