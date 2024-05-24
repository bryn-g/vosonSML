#' @title Reddit thread collection
#'
#' @description Collection of reddit subreddit comment threads.
#'
#' @param auth List. Reddit authentication. Authentication placeholder that can also be used to specify a ua-string if
#'   required. Default is \code{NULL}.
#' @param urls Character vector. Subreddit thread urls to collect comments from.
#' @param sort Character vector. Specify the thread comment sort order. Comment collection is limited so this can be
#'   useful to focus a thread collection. Options are \code{"best"}, \code{"top"}, \code{"new"}, \code{"controversial"},
#'   \code{"old"}, and \code{"qa"}. Default is \code{NA} for deafult API behaviour.
#' @param sleep Numeric vector. Allows the sleep period taken between requests to be specified. Reddit monitors the rate of
#'   requests that are made to the API and will block clients that exceed limits. If there are access problems, or
#'   API limit changes, the sleep parameter can be used to help avoid limit problems. Default is \code{c(6, 8)} for a
#'   random sleep period of between 6 and 8 seconds. The minimum value conforms to a limit of 10 requests per minute.
#' @param file Logical or Character. Write the collected data to file. If \code{TRUE} will generate a file name and
#'   write to default path. A file path can be specified. Default is \code{FALSE}.
#' @param verbose Logical. Display verbose messages. Default is \code{FALSE}.
#'
#' @return A named list of tibbles containing reddit thread collection data.
#'
#' @examples
#' \dontrun{
#' # collect subreddit thread comments
#' url <- c("https://www.reddit.com/r/xxxxxx/comments/xxxxxx/x_xxxx_xxxxxxxxx/")
#'
#' data <- collect_reddit_threads(urls = url)
#' }
#'
#' @export
collect_reddit_threads <-
  function(auth = NULL,
           urls = NULL,
           sort = NA,
           sleep = c(6, 8),
           file = FALSE,
           verbose = FALSE) {

    if (verbose) cli::cli_h3("collect_reddit_threads")

    # create data object
    data <- list(name = "voson_data", type = "reddit")

    ua <- vsml_ua()
    if (!is.null(auth)) {
      if (!is.null(auth$ua)) ua <- check_chr(auth$ua, param = "auth ua", min = 5)
    }

    invisible(check_chr(urls, param = "urls", min = 20))

    # check sort
    sort_opts <- c("best", "top", "new", "controversial", "old", "qa", NA)
    invisible(cmp_values(sort, sort_opts, param = "sort", n = length(urls)))

    if (length(sort) == 1) sort <- rep(sort, length(urls))
    sort <- tolower(sort)

    sleep <- check_wait_range_secs(sleep, param = "sleep", def_min = 3, def_max = 10)

    if (verbose) {
      cli::cli_alert_info("{format(Sys.time())} starting collection")
      cli::cli_alert_info("request sleep period between {sleep[1]} and {sleep[length(sleep)]} seconds.")
    }

    # invisible(set_collection_opts(ua = ua))

    comments <- get_reddit_threads(urls, sort, sleep, ua, verbose)

    if (is.null(comments)) {
      if (verbose) cli::cli_alert_warning("no data was returned.")
    } else {
      if (nrow(comments) < 1) {
        if (verbose) cli::cli_alert_warning("no comments were collected.")
      } else {
        # summary
        data$comments <- comments

        # try to manage similarly to mastodon
        # cli::cli_alert_info("HTML decoding comments.")
        # threads_df$comment <- textutils::HTMLdecode(threads_df$comment)

        # summary
        # results_df <- threads_df |>
        #   dplyr::group_by(.data$thread_id) |>
        #   dplyr::summarise(
        #     title = paste0(unique(.data$title), collapse = ","),
        #     subreddit = paste0(unique(.data$subreddit), collapse = ","),
        #     count = dplyr::n()
        #   ) |>
        #   dplyr::ungroup()
        #
        # results_df$title <-
        #   ifelse(nchar(results_df$title) > 42,
        #          paste0(strtrim(results_df$title, 42), "..."),
        #          results_df$title)
        # msg(print_summary(results_df))
        #
        # msg(paste0("Collected ", nrow(threads_df), " total comments.\n"))
      }
    }

    # class(threads_df) <- append(c("datasource", "reddit"), class(threads_df))

    # meta_log <- c(collect_log, paste0(format(Sys.time(), "%a %b %d %X %Y")))

    # if (writeToFile) write_output_file(threads_df, "rds", "RedditData", verbose = verbose)

    # msg("Done.\n")
    if (verbose) cli::cli_alert_success("done.")

    data
  }

get_reddit_threads <- function(urls, sort, sleep, ua, verbose = FALSE) {

  # process each thread
  threads <- list()
  for (thread_i in seq_along(urls)) {

    url <- urls[thread_i]
    url_sort <- sort[thread_i]
    if (length(urls) > 1 & (url != urls[1])) Sys.sleep(sample(sleep, 1))

    thread_json <- get_reddit_comments(url, url_sort, sleep, ua, verbose = verbose)
    branch_df <- parse_reddit_data(thread_json, url, verbose)

    # loop protection
    prev_value <- NULL

    extra_threads <- dplyr::filter(branch_df, grepl("Listing:", .data$comm_id))
    while (nrow(extra_threads) > 0) {
      row_i <- 1 # top row

      # loop protection
      if (!is.null(prev_value) && extra_threads[row_i, "comm_id"] == prev_value) {
        msg("loop protection following continue threads. exiting loop.")
        break
      }
      prev_value <- extra_threads[row_i, "comm_id"]

      Sys.sleep(sample(sleep, 1))

      # get continue thread comment position info
      cont_i <- as.numeric(extra_threads[row_i, "id"])
      depth <- as.numeric(gsub(".*_(\\d)_\\d$", "\\1", extra_threads[row_i, "structure"]))
      struct <- gsub("_\\d_\\d$", "", extra_threads[row_i, "structure"])

      row_comm_id <- extra_threads[row_i, "comm_id"]
      cont_thread_id <- gsub("Listing:t1_", "", row_comm_id)

      # set continue thread comment rm flag to true
      branch_df <- branch_df |>
        dplyr::mutate(rm = ifelse(.data$comm_id %in% c(cont_thread_id, row_comm_id), TRUE, .data$rm))

      # if trailing slash missing from url
      if (!grepl("/$", url)) url <- paste0(url, "/")

      # get continue thread
      cont_json <- get_reddit_comments(
        url = paste0(url, cont_thread_id),
        url_sort = url_sort,
        sleep = sleep,
        ua = ua,
        cont = cont_thread_id,
        verbose = verbose)

      cont_df <- parse_reddit_data(cont_json, url, depth = depth, verbose = verbose)

      # if comments returned
      if (nrow(cont_df)) {
        cont_df <- cont_df |> dplyr::mutate(structure = paste0(struct, "_", .data$structure)) # append structure

        # insert new comments into thread dataframe using position
        if (cont_i == 1) {
          branch_df <- dplyr::bind_rows(cont_df, branch_df)
        } else {
          pre_df <- dplyr::bind_rows(branch_df[1:cont_i - 1, ], cont_df)
          branch_df <- dplyr::bind_rows(pre_df, branch_df[cont_i:nrow(branch_df), ])
        }
      }

      extra_threads <- extra_threads[-row_i, ] # not needed
      extra_threads <- dplyr::filter(branch_df, grepl("Listing:", .data$comm_id), .data$rm == FALSE)

    } # end while

    if (!is.null(branch_df) && nrow(branch_df) > 0) {

      branch_df$thread_id <- extract_thread_id(branch_df$url)
      branch_df <- branch_df |>
        dplyr::filter(.data$rm == FALSE) |> # remove continue thread entries
        dplyr::arrange(.data$thread_id, .data$id)

      branch_df$id <- seq_along(branch_df$id) # re-index
    }

    threads[[thread_i]] <- branch_df
  }

  threads_df <- dplyr::bind_rows(threads) |> dplyr::select(-.data$rm)

  threads_df
}

# based on method developed by @ivan-rivera for RedditExtractoR
get_reddit_comments <- function(url, url_sort, sleep, ua, cont = NULL, verbose = FALSE) {

    if (is.null(url) || length(url) == 0 || !is.character(url)) stop("invalid url value")

    req_url <- create_thread_url(url, url_sort)
    req_tid <- get_thread_id(req_url, TRUE)

    if (is.null(cont)) {
      msg(paste0("Request thread: ", req_tid, " - sort: ", url_sort, "\n"))
    } else {
      msg(paste0("Continue thread: ", req_tid, " - ", cont, "\n"))
    }

    req_data <- get_json(req_url, ua) # ua

    if (is.null(req_data$data)) {
      Sys.sleep(sample(sleep, 1))

      if (is.null(cont)) {
        msg(paste0("Retry thread: ", req_tid, "\n"))
      } else {
        msg(paste0("Retry continue thread: ", req_tid, " - ", cont, "\n"))
      }

      req_data <- get_json(req_url, ua) # ua
    }

    if (is.null(req_data$status) || as.numeric(req_data$status) != 200) {
      msg(paste0("Failed: ", url, ifelse(is.null(req_data$status), "", paste0(" (", req_data$status, ")")), "\n"))
    }

    req_data$data
  }

# based on method developed by @ivan-rivera for RedditExtractoR
reddit_values_list  <- function(node, feature) {
  attr <- node$data[[feature]]

  if (is.null(attr)) attr <- NA
  if (feature == "id") {
    if (attr == "_") attr <- paste0("Listing:", node$data$parent_id)
  }

  reply_nodes <- NULL
  replies <- node$data$replies
  if (is.list(replies)) reply_nodes <- replies$data$children

  attrs <- list(attr, lapply(reply_nodes, function(x) reddit_values_list(x, feature)))

  attrs
}

# based on method developed by @ivan-rivera for RedditExtractoR
reddit_struct_list <- function(node, depth = 0) {
  if (is.null(node)) return(list())

  reply_nodes <- NULL
  replies <- node$data$replies
  if (is.list(replies)) reply_nodes <- replies$data$children

  # depth is converted to char to prevent col type to integer with no-depth threads
  structures <- list(
    as.character(depth),
    lapply(1:length(reply_nodes),
      function(x) reddit_struct_list(reply_nodes[[x]], paste0(depth, "_", x))
    )
  )

  structures
}

# based on method developed by @ivan-rivera for RedditExtractoR
parse_reddit_data <- function(raw_data, req_url, depth = 0, verbose = FALSE) {

  if (is.null(raw_data)) return(data_extract)

  meta_node <- raw_data[[1]]$data$children[[1]]$data
  main_node <- raw_data[[2]]$data$children

  if (min(length(meta_node), length(main_node)) > 0) {
    structures_list <- unlist(lapply(1:length(main_node), function(x) {
      reddit_struct_list(main_node[[x]], depth = ifelse(depth != 0, depth, x))
    }))

    data <- tibble::tibble(
      id = NA_integer_,
      structure = as.character(structures_list),
      post_date = as.character(lubridate::as_datetime(as.numeric(meta_node$created_utc), tz = "UTC")),
      post_date_unix = as.numeric(meta_node$created_utc),
      comm_id = unlist(lapply(main_node, function(x) reddit_values_list(x, "id"))),
      comm_date = as.character(
        lubridate::as_datetime(as.numeric(unlist(
          lapply(main_node, function(x) reddit_values_list(x, "created_utc"))
        )), tz = "UTC")
      ),
      comm_date_unix = as.numeric(unlist(lapply(main_node, function(x) reddit_values_list(x, "created_utc")))),
      num_comments = as.numeric(meta_node$num_comments),
      subreddit = ifelse(is.null(meta_node$subreddit), "-", as.character(meta_node$subreddit)),
      upvote_prop = as.numeric(meta_node$upvote_ratio),
      post_score = as.numeric(meta_node$score),
      author = as.character(meta_node$author),
      user = unlist(lapply(main_node, function(x) reddit_values_list(x, "author"))),
      comment_score = unlist(lapply(main_node, function(x) reddit_values_list(x, "score"))),
      controversiality = unlist(lapply(main_node, function(x) reddit_values_list(x, "controversiality"))),
      comment = unlist(lapply(main_node, function(x) reddit_values_list(x, "body"))),
      title = as.character(meta_node$title),
      post_text = as.character(meta_node$selftext),
      link = as.character(meta_node$url),
      domain = as.character(meta_node$domain),
      url = as.character(req_url),
      rm = FALSE
    )

    data$id <- 1:nrow(data)
  }

  data
}

# set options that may be used by collection dependencies
set_collection_opts <- function(ua = ua, envir = parent.frame()) {
  op <- options(
    encoding = "UTF-8",
    HTTPUserAgent = ua
  )
  sys_tz <- Sys.timezone()

  withr::defer({
    options(op)
    Sys.setenv(TZ = sys_tz)
  }, envir = envir)

  Sys.setenv(TZ = "Etc/UTC")
}

extract_thread_id <- function(url) {
  gsub("^(.*)?/comments/([0-9A-Za-z]{2,})?/{0,1}(.*)?/{0,1}$", "\\2", url,
    ignore.case = TRUE, perl = TRUE, useBytes = TRUE)
}
