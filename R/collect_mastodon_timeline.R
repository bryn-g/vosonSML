#' @title Mastodon timeline collection
#'
#' @description Collection of instance timeline data.
#'
#' @param auth List. Mastodon authentication. If is \code{NULL} anonymous API access will be assumed.
#' @param instance Character string. Server to collect timeline posts from. If left \code{NULL} auth token instance will
#'   be used.
#' @param account Character string. Collect posts from a specified account username or id. Default is \code{NULL}.
#' @param hashtag Character string. Specifies a hashtag filter e.g \code{"rstats"}. Default is \code{NULL} to return all
#'   unfiltered timeline posts.
#' @param local Logical. If \code{TRUE} Search only the local server timeline, or global federated timeline if \code{FALSE}. Not used if specifying a user account. Default is \code{FALSE}.
#' @param limit Numeric. Specify the collection limit for timeline posts. Default is \code{100}.
#' @param anonymous Logical. Collect public posts without authenticating. Default is \code{TRUE}.
#' @param extract Logical. Extract metadata into seperate dataframes. Default is \code{TRUE}.
#' @param file Logical or Character. Write the collected data to file. If \code{TRUE} will generate a file name and
#'   write to default path. A file path can be specified. Default is \code{FALSE}.
#' @param verbose Logical. Display verbose messages. Default is \code{FALSE}.
#' @inheritDotParams rtoot::get_timeline_hashtag -hashtag -limit -local -instance -token -verbose -parse
#'
#' @return A named list of tibbles containing mastodon collection data.
#'
#' @examples
#' \dontrun{
#' # collect timeline posts
#' data <- collect_mastodon_timeline(instance = "mastodon.social")
#' }
#'
#' @export
collect_mastodon_timeline <-
  function(auth = NULL,
           instance = NULL,
           account = NULL,
           hashtag = NULL,
           local = FALSE,
           limit = 100,
           anonymous = TRUE,
           extract = TRUE,
           file = FALSE,
           verbose = FALSE,
           ...) {

    msg("mastodon data collection", .x = "h2")

    # check required packages
    pkgs_install_or_stop("rtoot")

    # create data object
    data <- list(name = "voson_data", type = "mastodon")

    # get access token
    if (is.null(auth)) anonymous <- TRUE
    token <- auth$token

    # if instance null use token instance
    if (!is.null(token$instance) && is.null(instance)) {
      instance <- token$instance
    } else {
      instance <- check_chr(instance, param = "instance")
    }

    section_tl <- msg_par()
    msg("collect timeline", .x = "h3")

    # check account
    if (!is.null(account)) {
      if (grepl("[0-9]", account)) {
        account <- as.character(account)
      } else {
        user <- get_mastodon_user(instance, account, verbose)
        account_id <- unlist(user$id)

        if (grepl("[0-9]", account_id)) {
          account <- as.character(account_id)
          data$account <- user
        } else {
          stop(gum("unable to retrieve account id for user {account}."), call. = FALSE)
        }
      }
    }

    hashtag <- check_chr(hashtag, param = "hashtag", null.ok = TRUE)

    msg("get timeline posts (limit:{limit})", .x = "alert")
    msg("instance: {instance}", .x = "info")
    if (!is.null(account)) msg("account: {account}", .x = "info")
    if (!is.null(hashtag)) msg("hashtag: {hashtag}", .x = "info")

    # collection params
    params <- list(
      token = token,
      instance = instance,
      local = local,
      limit = limit,
      anonymous = anonymous,
      verbose = FALSE
    )
    params <- append(params, substitute(...()))

    # timeline collection
    params$hashtag <- hashtag

    # get server posts
    if (is.null(account) && is.null(hashtag)) {
      posts <- do.call(rtoot::get_timeline_public, params)

      # get server posts filtered by specified hashtag
    } else if (is.null(account) && !is.null(hashtag)) {
      posts <- do.call(rtoot::get_timeline_hashtag, params)

      # get server posts by specified account, also filters by hashtag param if value is not null
    } else {
      params$local <- NULL
      params$id <- account
      posts <- do.call(rtoot::get_account_statuses, params)
    }

    data$extract <- extract
    data$posts <- posts

    msg("done. {nrow(data$posts)} posts.", .x = "success")
    msg_end(section_tl)

    # return data without any processing
    if (!extract) return(data)

    # extract metadata
    msg("extract metadata", .x = "h3")
    data <- extract_metadata(data, verbose)

    # data summary
    if (verbose) print_mastodon_summary(data, verbose)

    # write file
    if (file) {
      msg("save data to file", .x = "prog_step")
      write_output_file(data, name = "mtdn_timeline", verbose = verbose)
    }

    msg("done.", .x = "success")
    data
  }

print_mastodon_summary <- function(data, verbose = FALSE) {
  summary <- tryCatch({
    x <- get_mastodon_summary(data)
    knitr::kable(x, "simple")
  }, error = function(err) {
    msg("failed to create data summary:\n{err}", .x = "warning")
    NULL
  })

  if (!is.null(summary)) {
    msg(paste0(" ", summary, collapse = "\n"), .x = "info")
  }

  invisible()
}

get_mastodon_summary <- function(data) {
  tibble::enframe(
    list(
      posts = nrow(data$posts),
      reblogs = nrow(dplyr::filter(data$posts, !is.na(.data$reblog.id))),
      tags = {
        tags <- tidyr::unnest(data$tags, cols = "tags")
        ifelse("name" %in% colnames(tags), dplyr::n_distinct(tags$name), 0)
      },
      authors = dplyr::n_distinct(data$users$account.id),
      mentioned = dplyr::n_distinct(data$mentions$mentions.id),
      "first post" = gum("{format(min(data$posts$created_at))} UTC"),
      "last post" = gum("{format(max(data$posts$created_at))} UTC")
    ))
}

# get mastodon user from account username
get_mastodon_user <- function(instance, username, verbose = FALSE) {
  msg("get user id for @{username} from {instance}", .x = "alert")

  user <- tryCatch({
    req_headers <- c("Accept-Charset" = "UTF-8", "Cache-Control" = "no-cache")
    resp <- httr::GET(
      url = stringr::str_glue("https://{instance}/api/v1/accounts/lookup?acct={username}"),
      httr::add_headers(.headers = req_headers)
    )

    # json content
    user_json <- jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = TRUE)

    # return as tibble
    user_json |>
      purrr::discard(function(x) is.null(x) || length(x) == 0) |>
      list() |> purrr::transpose() |> tibble::as_tibble()

  }, error = function(err) {
    msg("failed to get user info:\n{err}", .x = "warning")
    NULL
  })

  #cli_alert_success
}

# return a named list of tibbles for each data and metadata type
extract_metadata <- function(data, verbose = FALSE) {
  msg_par()

  x <- tryCatch({
    x <- data
    # extract and add reblogs to posts
    x$posts <- extract_reblogs(data$posts)

    # extract post account ids for convenience
    x$posts <- x$posts |> tidyr::hoist("account", account.id = "id")
    x$posts <- x$posts |>
      dplyr::left_join(
        x$posts |> dplyr::filter(.data$reblog.post) |>
          dplyr::select(reblog.id = "id", reblog.account.id = "account.id"), by = "reblog.id")

    # extract users
    x$users <- extract_users(x$posts)
    x$posts <- x$posts |> dplyr::select(!c("account"))

    # extract mentions
    x$mentions <- extract_mentions(x$posts)
    x$posts <- x$posts |> dplyr::select(!c("mentions"))

    # extract tags
    x$tags <- extract_tags(x$posts)
    x$posts <- x$posts |> dplyr::select(!c("tags"))

    # extract text
    x$text <- extract_text(x$posts)
    x$posts <- x$posts |> dplyr::select(!c("content", "spoiler_text", "text", "language", "emojis"))

    x$extract <- TRUE

    x
  }, error = function(err) {
    msg("failed to extract metadata from posts:\n{err}", .x = "warning")
  })

  msg_end()

  if (is.null(x)) {
    data$extract <- FALSE
    return(data)
  }

  x
}

extract_reblogs <- function(posts) {
  posts <- posts |>
    dplyr::mutate(reblog.post = FALSE) |>
    tidyr::hoist("reblog", reblog.id = "id", .remove = FALSE)

  reblogs <- posts |>
    dplyr::filter(!is.na(reblog.id)) |>
    dplyr::select("reblog") |>
    tidyr::unnest("reblog") |>
    dplyr::mutate(reblog.post = TRUE)

  posts <- posts |> dplyr::bind_rows(reblogs) |> dplyr::select(!c("reblog"))

  posts
}

extract_users <- function(posts) {
  users <- posts |>
    dplyr::select(post.id = "id", "account.id", "account") |>
    tidyr::unnest_wider("account", names_sep = ".")

  # convert html user notes to text
  users <- users |>
    dplyr::mutate(
      account.note.text = ifelse(!is.na(.data$account.note), ensure_tags(.data$account.note), NA_character_)) |>
    html_text2("account.note.text")
}

extract_mentions <- function(posts) {
  mentions <- posts |>
    dplyr::select(post.id = "id", "account.id", "mentions", "in_reply_to_account_id") |>
    tidyr::unnest_wider("mentions", names_sep = ".") |>
    tidyr::unnest_longer(tidyr::starts_with("mentions."))
}

extract_tags <- function(posts) {
  tags <- posts |>
    dplyr::select(post.id = "id", "tags") |>
    dplyr::filter(purrr::map_lgl(.data$tags, ~!rlang::is_empty(.x)))
}

extract_text <- function(posts) {
  text <- posts |> dplyr::select(post.id = "id", "content", "spoiler_text", "text", "language", "emojis")

  # convert html post content to text
  text <- text |>
    dplyr::mutate(content.text = ensure_tags(.data$content)) |>
    html_text("content.text")
}
