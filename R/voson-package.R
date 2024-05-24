#' @name voson-package
#' @aliases voson-package voson
#' @docType package
#' @keywords internal
"_PACKAGE"
#' @importFrom data.table data.table setkey :=
#' @importFrom methods new
#' @importFrom rlang .data check_installed
#' @importFrom stats na.omit runif setNames
#' @importFrom utils capture.output flush.console read.table tail
#' @importFrom stringr str_glue
utils::globalVariables(c("collect_log", "merge_log"))
NULL
