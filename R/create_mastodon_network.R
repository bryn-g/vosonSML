#' @title Mastodon network creation
#'
#' @description Mastodon networks from collected data.
#'
#' @param data List. Named list containing data.
#' @param type Character. Type of network to create.
#' @param verbose Logical. Display verbose messages. Default is \code{FALSE}.
#' @param ... Additional parameters to pass to graph function.
#'
#' @return A named list with network as a tidygraph network graph object
#'
#' @export
create_mastodon_network <- function(data = NULL, type = "activity", text = FALSE, verbose = FALSE, ...) {

    msg("create_mastodon_network", .x = "h3")

    # check data
    check_chr(data$name, param = "data", accept = c("voson_data"))
    check_chr(data$type, param = "data", accept = c("mastodon"))
    if (check_df_n(data$posts) < 1) stop("post data has no observations.", call. = FALSE)

    # check params
    check_chr(type, param = "type", accept = c("activity", "actor", "server", "tag"))

    # create network object
    network <- list(name = "voson_network", type = "mastodon", subtype = type)

    network$net <- switch(type,
      activity = create_mastodon_activity_network(data),
      actor = create_mastodon_actor_network(data)
    )

    network
}

create_mastodon_activity_network <- function(data) {
  edges <- data$posts |>
    dplyr::select(
      from = "id",
      from.account.id = "account.id",
      "in_reply_to_id",
      "in_reply_to_account_id",
      "reblog.id",
      "reblog.account.id",
      "created_at"
    ) |>
    dplyr::filter(!is.na(.data$in_reply_to_id) | !is.na(.data$reblog.id)) |>
    dplyr::mutate(
      type = data.table::fcase(
        !is.na(.data$reblog.id), "reblog",
        !is.na(.data$in_reply_to_id), "reply",
        default = NA_character_),
      to = data.table::fcase(
        .data$type == "reblog", .data$reblog.id,
        .data$type == "reply", .data$in_reply_to_id,
        default = NA_character_
      ),
      to.account.id = data.table::fcase(
        .data$type == "reblog", .data$reblog.account.id,
        .data$type == "reply", .data$in_reply_to_account_id,
        default = NA_character_
      )
    ) |>
    dplyr::select(-c("in_reply_to_id", "reblog.id", "in_reply_to_account_id", "reblog.account.id")) |>
    dplyr::relocate("from", "to", "from.account.id", "to.account.id")

  nodes <- data$posts

  list(nodes = nodes, edges = edges, node_key = "id")
}

create_mastodon_actor_network <- function(data) {
  edges <- data$posts |>
    dplyr::select(
      from = "account.id",
      post.id = "id",
      "in_reply_to_account_id",
      "reblog.account.id",
      post.created_at = "created_at"
    ) |>
    dplyr::filter(!is.na(.data$in_reply_to_account_id) | !is.na(.data$reblog.account.id)) |>
    dplyr::mutate(
      type = data.table::fcase(
        !is.na(.data$reblog.account.id), "reblog",
        default = "reply"),
      to = data.table::fcase(
        !is.na(.data$reblog.account.id), .data$reblog.account.id,
        !is.na(.data$in_reply_to_account_id), .data$in_reply_to_account_id,
        default = NA_character_)
    ) |>
    dplyr::select("from", "to", "type", "post.id", "post.created_at")

  nodes <- data$users

  if (nrow(data$mentions)) {
    mentions <- data$mentions |>
      dplyr::select(from = "account.id", to = "mentions.id", type = "mention", "post.id") |>
      dplyr::left_join(edges |> dplyr::select("post.id", "post.created_at"), by = "post.id")

    edges <- edges |> dplyr::bind_rows(mentions) |> dplyr::relocate("account.id")

    mentions <- data$mentions |>
      dplyr::rename_with(~ paste0("blocks.", .x)) |> dplyr::relocate(instance)
  }

  list(nodes = nodes, edges = edges, node_key = "id")
}
