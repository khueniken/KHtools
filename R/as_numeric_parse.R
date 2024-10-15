#' Coerce to Numeric with Helpful Messages
#'
#' This function is a wrapper for as.numeric(). It takes a vector as input and
#' returns a numeric vector, while printing to the console every entry that
#' cannot be coerced.
#' @param x a string or vector to coerce to numeric
#' @param ... additional arguments passed to `as.numeric()
#' @keywords KHtools
#' @export
#' @examples
#' as_numeric_parse()

as_numeric_parse <- function(x, ...){

  if (!class(x) %in% c("logical","numeric","double","integer","character","Date")) {
    stop("Invalid data type")
  }

  y <- suppressWarnings(as.numeric(x, ...))
  noparse <- x[!is.na(x) & is.na(y)]

  if (length(noparse) > 0) {
    noparse_warn <- paste0("Entry ", which(!is.na(x) & is.na(y)), ", '", noparse, "'")
    message("The following entries were converted to NA values:")

    for (i in 1:length(noparse_warn)) message(noparse_warn[i])
  }
  return(y)
}
