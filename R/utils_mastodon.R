# add any referenced nodes in edge list that are absent from node list
add_absent_nodes <- function(nodes, edges, id = "post.id") {
  # find any absent nodes
  x <- c(edges |> dplyr::pull("from"), edges |> dplyr::pull("to")) |> unique()
  y <- nodes |> dplyr::pull({{ id }})

  absent_nodes <- tibble::tibble("{id}" := setdiff(x, y), absent = TRUE) |>
    tidyr::drop_na({{ id }})

  # add nodes that are absent
  nodes <- nodes |> dplyr::mutate(absent = FALSE) |> dplyr::bind_rows(absent_nodes)

  nodes
}

# html to text conversion
html_text <- function(data, cols) {
  data |>
    dplyr::rowwise() |>
    dplyr::mutate_at(cols, ~ if (!is.na(.x)) rvest::html_text2(rvest::read_html(.x))) |>
    dplyr::ungroup()
}

html_text2 <- function(data, col) {
  data |>
    dplyr::rowwise() |>
    dplyr::mutate("{{col}}" = ifelse(!is.na(.data[[col]]), rvest::html_text2(rvest::read_html(.data[[col]])), NA_character_))
}

# ensure html text is wrapped in html tags
ensure_tags <- function(x, ...) {
  dplyr::if_else(stringr::str_detect(x, "^<p>.*", negate = TRUE), paste0("<p>", x, "</p>"), x, ...)
}
