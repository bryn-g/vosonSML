# voson.cat option set to TRUE uses the cat function for messages
use_cat <- function(opt = "voson.cat", default = FALSE) {
  opt_cat <- getOption(opt)
  if (is.logical(opt_cat) && !is.na(opt_cat)) return(opt_cat) # return opt setting
  default
}

msg_glue <- function(x, .xenvir = parent.frame()) {
  params <- as.list(unname(x))
  params$.envir <- .xenvir

  # string interpolation
  if (length(x) > 1) {
    # not ideal
    params$.sep <- "\n"
    x_ <- do.call(gum, params)
    x_ <- unlist(strsplit(x_, "\n"))
  } else {
    x_ <- do.call(gum, params)
  }

  x_
}

# output messages with cat
msg_cat <- function(x, ..., .x = "line", .xenvir = parent.frame()) {

  # string interpolation
  x_ <- msg_glue(x, .xenvir = .xenvir)

  if (!.voson_state$cli) {
    cat(x_, ..., appendLF = TRUE)

  } else {
    f <- list(line = cli::cat_line, bullets = cli::cat_bullet)
    if (!.x %in% names(f)) .x <- "line"

    do.call(f[[.x]], list(c(x_), substitute(...())))
  }

  flush.console()
  Sys.sleep(0.05)

  invisible(x)
}

# outputs messages if the calling function has verbose set
# warning and danger messages will still be printed, users can suppress these with suppressMessages
# messsages will be passed to cat output if options voson.msg is used and set to false
msg <- function(x, ..., .x = "-", .xenvir = parent.frame()) {

  # if not verbose only continue if warning or danger message
  if (is.null(.xenvir$verbose) || .xenvir$verbose == FALSE) {
    if (!.x %in% c("warning", "danger")) return(invisible(x))
  }

  # if cat output
  if (use_cat()) {
    msg_cat(x, ..., .x = .x, .xenvir = .xenvir)

  } else {
    if (!.voson_state$cli) {
      message(msg_glue(x, .xenvir = .xenvir), ...)

    } else {
      # map some cli package functions
      switch(.x,
             "text" = cli::cli_text(x, .envir = .xenvir),
             "alert" = cli::cli_alert(x, ..., .envir = .xenvir),
             "info" = cli::cli_alert_info(x, ..., .envir = .xenvir),
             "warning" = cli::cli_alert_warning(x, ..., .envir = .xenvir),
             "danger" = cli::cli_alert_danger(x, ..., .envir = .xenvir),
             "success" = cli::cli_alert_success(x, ..., .envir = .xenvir),
             "bullets" = cli::cli_bullets(x, ..., .envir = .xenvir),
             "bullets_raw" = cli::cli_bullets_raw(x, ...),
             "pluralize" = cli::pluralize(x, ..., .envir = .xenvir),
             "h2" = cli::cli_h2(x, ..., .envir = .xenvir),
             "h3" = cli::cli_h3(x, ..., .envir = .xenvir),
             "prog_step" = cli::cli_progress_step(x, ..., .envir = .xenvir),
             message(msg_glue(x, .xenvir = .xenvir), ...)
      )
    }
  }

  invisible(x)
}

msg_par <- function(..., .xenvir = parent.frame()) {
  if (.voson_state$cli) {
    return(cli::cli_par(..., .envir = .xenvir))
  }

  msg("", .x = "-")
  invisible()
}

msg_end <- function(x = "") {
  if (.voson_state$cli) {
    cli::cli_end(x)
    return(invisible(x))
  }

  msg("", .x = "-")
  invisible()
}

msg_prog_update <- function(..., .xenvir = parent.frame()) {
  if (.voson_state$cli & !use_cat()) cli::cli_progress_update(..., .envir = .xenvir)
  invisible()
}

# shorter glue aliases
gum <- stringr::str_glue
gum_data <- stringr::str_glue_data

# check packages and prompt to install if interactive
pkgs_install_or_stop <- function(pkgs, fn_name = NULL) {
  if (is.null(fn_name)) fn_name <- rlang::caller_call(n = 1)

  rlang::check_installed(pkgs, paste("for", fn_name))
  stop_if_missing_pkgs(pkgs, fn_name = fn_name)

  invisible(pkgs)
}

# check for required packages and stop with a message if missing
stop_if_missing_pkgs <- function(pkgs, fn_name = NULL) {
  if (is.null(fn_name)) fn_name <- rlang::caller_call(n = 1)

  # check namespaces
  req <- sapply(pkgs, function(x) requireNamespace(x, quietly = TRUE))

  if (any(req == FALSE)) {
    pkgs <- names(which(req == FALSE))
    stop(msg("please install {pkgs} package{?s} before calling {fn_name}.", .x = "pluralize"), call. = FALSE)
  }

  invisible(pkgs)
}
