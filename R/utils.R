#' Less scary green messages
#'
#' @param ... message components (see \code{\link[base]{message}})
#' @param domain (see \code{\link[base]{message}})
#' @param appendLF append new line? (see \code{\link[base]{message}})
#'
#' @return TRUE
#' @keywords internal
#'
message <- function (..., domain = NULL, appendLF = TRUE) {
  #if (is.null(knitr::opts_knit$get('rmarkdown.pandoc.to'))) {
  if (options("scienceverse.verbose")[[1]]) {
    if (interactive()) {
      # not in knitr environment
      base::message("\033[32m", ..., "\033[39m",
                    domain = domain, appendLF = appendLF)
    } else {
      base::message(..., domain = domain, appendLF = appendLF)
    }
  }
}


#' Check if values are NULL, NA, blank after trimming, or an empty list
#'
#' @param x vector or list to test
#' @param test_for values to test for ("null" replaces NULL values, "na", replaces NA values, "trim" replaces empty strings after trimws(), "empty" replaces empty lists)
#'
#' @return vector or list of logical values
#' @keywords internal
#'
#' @examples
#' x <- list(NULL, NA, " ", list())
#' is_nowt(x)
#' is_nowt(x, test_for = "null")
#' is_nowt(x, test_for = "na")
#' is_nowt(x, test_for = "trim")
#' is_nowt(x, test_for = "empty")
#'
is_nowt <- function(x, test_for = c("null", "na", "trim", "empty")) {
  # NULL is no longer atomic as of 2023
  if (is.null(x) & "null" %in% test_for) return(TRUE)

  # only handles atomic vectors and lists
  if (!is.atomic(x) & !is.list(x)) return(FALSE)

  if (length(x) > 1) {
    args <- list(X = x, FUN = is_nowt,
                 test_for = test_for)
    func <- ifelse(is.list(x), lapply, sapply)
    y <- do.call(func, args)
    return(y)
  }

  nowt <- FALSE
  if ("null" %in% test_for)
    nowt <- nowt | isTRUE(is.null(x))
  if ("na" %in% test_for)
    nowt <- nowt | isTRUE(is.na(x))
  if ("trim" %in% test_for)
    nowt <- nowt | isTRUE(trimws(x) == "")
  if ("empty" %in% test_for)
    nowt <- nowt | (is.list(x) & length(x) == 0)

  return(nowt)
}


#' Replace values if NULL, NA, blank after trimming, or an empty list
#'
#' @param x vector or list to test
#' @param replace value to replace with
#' @param test_for values to test for ("null" replaces NULL values, "na", replaces NA values, "trim" replaces empty strings after trimws(), "empty" replaces empty lists)
#'
#' @return vector or list with replaced values
#' @keywords internal
#'
#' @examples
#' if_nowt(NULL)
#' if_nowt(NA)
#' if_nowt("   ")
#' if_nowt(c(1, 2, NA), replace = 0)
#' x <- list(NULL, NA, " ", list())
#' if_nowt(x) %>% str()
#' if_nowt(x, test_for = "null") %>% str()
#' if_nowt(x, test_for = "na") %>% str()
#' if_nowt(x, test_for = "trim") %>% str()
#' if_nowt(x, test_for = "empty") %>% str()
if_nowt <- function(x, replace = "", test_for = c("null", "na", "trim", "empty")) {
  if (length(x) > 1) {
    args <- list(X = x, FUN = if_nowt,
                 replace = replace,
                 test_for = test_for)
    func <- ifelse(is.list(x), lapply, sapply)
    y <- do.call(func, args)
    return(y)
  }

  if (is_nowt(x, test_for)) {
    return(replace)
  } else {
    return(x)
  }
}
