# create a file name with system time as prefix
sys_time_filename <- function(name, ext, t = Sys.time(), fmt = "%Y-%m-%d_%H%M%S", clean = FALSE) {
  if (clean) {
    name <- stringr::str_replace_all(name, "[\\s:]", "_")
    ext <- stringr::str_remove_all(ext, "[\\s:\\.]")
  }
  gum("{format(t, fmt)}-{name}.{ext}")
}

# get opt data path if set
data_path <- function(x = getOption("voson.data")) {
  if (is.null(x)) return(NULL)

  # change to forward slashes and ensure one trailing
  path <- gsub("\\\\", "/", x)
  path <- sub("(\\/)+$", "", path)
  path <- paste0(path, "/")

  # if data directory does not exist create it
  if (!dir.exists(path)) dir.create(path, showWarnings = TRUE)

  # return path if exists
  if (dir.exists(path)) return(path)

  NULL
}

# generally defaults to working directory if no path set
output_file_path <- function(filename, path = data_path()) {
  if (!is.null(path)) return(gum("{path}{filename}"))
  filename
}

# write data to file in supported file format
write_output_file <- function(data, type = "rds", name = "voson_file", datetime = TRUE, verbose = FALSE) {

  supported <- c("graphml", "csv", "rds")
  if (!type %in% supported) {
    supported <- paste(supported, collapse = ",")
    msg("unable to write a file of type: {type} (ext: {supported})", .x = "warning")
    return(invisible())
  }

  if (type == "csv") stop_if_missing_pkgs("readr")
  if (datetime) name <- sys_time_filename(name, ext = type)

  name <- output_file_path(name)

  result <- list()
  # write rds
  if (type == "rds") {
    result <- tryCatch({
      saveRDS(data, output_file_path(name))
      list(status = "ok")
    }, error = function(e) {
      list(status = "failed", error = e)
    })

  # write graphml or csv to file connection
  } else {
    con <- tryCatch({
      file(name, open = "wb", encoding = "native.enc")
    }, error = function(e) {
      list(status = "failed", error = e)
    })

    if (inherits(con, "connection")) {
      result <- tryCatch({
        switch(type,
               "graphml" = igraph::write_graph(data, file = con, format = "graphml"),
               "csv" = readr::write_csv(data, file = con))
        list(status = "ok")

      }, error = function(e) {
        list(status = "failed", error = e)
      })

      close(con)
    } else {
      result <- con
    }
  }

  if (!is.null(result$status)) {
    if (result$status != "ok") {
      msg("unable to write file: {name}.", .x = "warning")
      if (!is.null(result$error)) msg("error: {result$error}.", .x = "warning")
    } else {
      msg("file written: {name}.", .x = "success")
    }
  } else {
    msg("unable to write file: {name}.", .x = "warning")
  }

  invisible(data)
}

# write a simple log file
log_to_file <- function(x, name = "voson_file") {

  log_path <- gum("{output_file_path(name)}.txt")
  result <- tryCatch({
    con <- file(log_path)
    writeLines(x, con)
    close(con)
    list(status = "ok")
  }, error = function(e) {
    list(status = "failed", error = e)
  })

  if (result$status != "ok") {
    msg("unable to write file: {name}.", .x = "warning")
    msg("error: {result$error}.", .x = "warning")
  }

  invisible(x)
}
