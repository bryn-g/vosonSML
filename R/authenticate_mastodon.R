#' @title Mastodon authentication
#'
#' @description Instance API authentication. Defaults to non-authenticated access, or alternatively wraps the creation
#'   of an `rtoot` bearer token object via `rtoot::auth_setup`.
#'
#' @param instance Character vector. Mastodon instance to authenticate with. Default is \code{NULL} for no
#'   authentication.
#' @param type Character vector. Type of authentication, can be \code{"public"} or \code{"user"} Default is
#'   \code{"public"}
#' @param token An `rtoot` bearer token object. Assign a bearer token directly. Default is \code{NULL}.
#' @param verbose Logical. Display verbose messages. Default is \code{FALSE}.
#' @inheritDotParams auth_setup::auth_setup -instance -type -verbose
#'
#' @return A named list of authentication values.
#'
#' @examples
#' \dontrun{
#' # use anonymous API access to servers
#' auth <- authenticate_mastodon()
#'
#' # get API authentication token via web browser
#' auth <- authenticate_mastodon("mastodon.social")
#'
#' # assign an existing rtoot bearer token directly
#' token <- rtoot::auth_setup("mastodon.social", "public")
#' auth <- authenticate_mastodon(token = token)
#' }
#'
#' @export
authenticate_mastodon <-
  function(instance = NULL,
           type = "public",
           token = NULL,
           verbose = FALSE,
           ...) {

    msg("authenticate_mastodon", .x = "h3")

    # create auh object
    auth <- list(name = "voson_auth", type = "mastodon")

    if (!is.null(token)) {
      # check token
      token <- check_chr(token, param = "token", min = 1)

      msg("assigning specified bearer token.", .x = "info")

      auth$token <- token
      auth$instance <- check_chr(token$instance, param = "token instance", min = 1)

      msg("done.", .x = "success")

      return(auth)
    }

    # anonymous access
    if (is.null(instance)) {
      msg("no instance specified, assuming anonymous access.", .x = "info")
      msg("done.", .x = "success")
      return(auth)
    }

    # check req packages
    pkgs_install_or_stop("rtoot", "authenticate_mastodon")

    # get bearer token
    instance <- check_chr(instance, param = "instance", min = 1)
    type <- check_chr(type, param = "type", accept = c("public", "user"))

    msg("create auth token", .x = "prog_step")
    auth$token <- rtoot::auth_setup(
      instance = instance,
      type = type,
      verbose = verbose,
      ...
    )

    auth
  }
