## set default options
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.scienceverse <- list(
    scienceverse.verbose = TRUE
  )
  toset <- !(names(op.scienceverse) %in% names(op))
  if(any(toset)) options(op.scienceverse[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  # paste(
  #   "\n************",
  #   "Welcome to papercheck, a scienceverse package.",
  #   "For support and examples visit:",
  #   "http://scienceverse.github.io/papercheck/",
  #   "************",
  #   sep = "\n"
  # ) %>% packageStartupMessage()
}
