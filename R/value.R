#' Extract the value from a monad instance
#' 
#' @param x an instance of a monad
#' @param ... ignored
#' @return the value wrapped by the monad
#'
#' @export
value <- function(x, ...) {
  stopifnot("x should be a Monad instance" = inherits(x, "R6") & any(grepl("Monad", class(x))))
  x$value
}
