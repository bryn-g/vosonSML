#' @title Create a network graph object
#'
#' @description Create a specified type of graph object from a tibble of network edges and a tibble of network nodes.
#'
#' @param network Named list. Network object returned from create network function.
#' @param type Character. Type of graph object to return, can be \code{"tidygraph"} or \code{"igraph"}. Default is
#'   \code{"tidygraph"}.
#' @param file Logical or Character. Write the collected data to file. If \code{TRUE} will generate a file name and
#'   write to default path. A file path can be specified. Default is \code{FALSE}.
#' @param verbose Logical. Display verbose messages. Default is \code{FALSE}.
#' @param ... Additional parameters to pass to \pkg{igraph} or \pkg{tidygraph} graph function.
#'
#' @return A network graph object
#'
#' @export
create_network_graph <- function(network, type = "tidygraph", file = FALSE, verbose = FALSE, ...) {

  if (verbose) cli::cli_h3("create_network_graph")

  check_chr(network$name, param = "network", accept = c("voson_network"))
  check_chr(network$type, param = "network", accept = c("mastodon", "reddit", "youtube", "web"))
  if (check_df_n(network$edges) < 1) stop("network edges tibble has no observations.", call. = FALSE)

  network$edges <- network$edges |> dplyr::relocate("from", "to")

  if (type == "igraph") {
    g <- igraph::graph_from_data_frame(
      d = network$edges,
      vertices = network$nodes,
      ...
    )
    g <- igraph::set_graph_attr(g, "type", network$type)
  } else {
    g <- tidygraph::tbl_graph(
      nodes = network$nodes,
      edges = network$edges,
      node_key = network$node_key,
      ...
    )
  }

  if (file) {
    # write to file
  }

  g
}
