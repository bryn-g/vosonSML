#' @title Reddit authentication
#'
#' @description Reddit API authentication. This version of the `voson` package uses a non-authenticated collection
#'   method that only accesses public Reddit API endpoints. As such, this function is generally not required except in
#'   limited circumstances and is primarily a placeholder for future authenticated methods.
#'
#' @param ua Character vector. An optional identifying User-Agent (UA string) to use for API requests. This is not
#'   required and should only be used to assist with access issues. Default is \code{NULL} to use the standard `voson`
#'   software package identifier.
#'
#' @return A named list of authentication values.
#'
#' @examples
#' \dontrun{
#' # specify a ua-string to use for public API collection requests
#' auth <- authenticate_reddit(ua = "voson r package")
#' }
#'
#' @export
authenticate_reddit <-
  function(ua = NULL,
           verbose = FALSE) {

  if (verbose) cli::cli_h3("authenticate_reddit")

  # create auh object
  auth <- list(name = "voson_auth", type = "reddit")

  # check ua
  if (!is.null(ua)) {
    ua <- check_chr(ua, param = "ua", min = 1)

    if (verbose) cli::cli_alert_info("assigning the specified ua-string: {ua}.")
    auth$ua <- ua

    if (verbose) cli::cli_alert_success("done.")
  }

  auth
}
