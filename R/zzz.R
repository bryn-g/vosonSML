.voson_state <- new.env()

.onLoad <- function(libname, pkgname) {
  .voson_state$cli <- requireNamespace("cli", quietly = TRUE)
  invisible()
}
