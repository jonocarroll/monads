#' Result Monad
#'
#' @description Creates a `Result` monad instance which can be used with the flatMap 
#' operator `%>>=%`. See the `Monads in R` vignette for more details.
#'
#' This container, when flatMapped, always returns a value, either of type 
#' `Err` (if an error was raised or the result was `NULL`), or `Ok`.
#'
#' @importFrom R6 R6Class
#' @seealso [Err()], [is_err()], [Ok()], [is_ok()]
#'
#' @export
Result <- R6::R6Class(
  c("ResultMonad"),
  public = list(
    #' @field value value contained in the instance
    value = NULL,
    #' @description create a new `Result` instance
    #' @param value value to be contained
    #' @return a new `Result` instance
    initialize = function(value) {
      if (rlang::is_quosure(value)) {
        self$value <- rlang::eval_tidy(value)
      } else {
        self$value <- value
      }
    },
    #' @description bind when using flatMap (`%>>=%`)
    #' @param f function to apply
    #' @param expr expression of the function
    bind = function(f, expr) {
      result <- tryCatch(f(self$value), error = function(e) e)
      if (inherits(result, "error") | is.null(self$value)) {
        result <- Result$new(Err(result$message, paste0("previously: ", self$value)))
      } else {
        result <- Result$new(Ok(result))
      }
      result
    }
  )
)

#' Create a new `Result` instance
#'
#' @param value value to be contained
#'
#' @returns a `Result` monad instance
#' @export
resultM <- function(value) {
  v <- rlang::enquo(value)
  Result$new(v)
}

#' @export
print.ResultMonad <- function(x, ...) {
  print(value(x))
}

#' Create an `Err` instance
#'
#' @param ... error messages to be contained 
#'
#' @family result
#' @returns an `Err` instance
#' @export
Err <- function(...) {
  structure(paste(c(...), collapse = "; "), class = "err")
}

#' Test for `Err`
#'
#' @param x object to be tested
#'
#' @family result
#' @returns logical; whether `x` is an `Err` instance
#' @export
is_err <- function(x) {
  inherits(x, "ResultMonad") & inherits(value(x), "err")
}

#' @export
print.err <- function(x, ...) {
  cat("Error:\n")
  print(unclass(x))
}

#' Create an `Ok` instance
#'
#' @param x value to be contained 
#'
#' @family result
#' @returns an `Ok` instance
#' @export
Ok <- function(x) {
  structure(x, class = "ok")
}

#' Test for `Ok`
#'
#' @param x object to be tested
#'
#' @family result
#' @returns logical; whether `x` is an `Ok` instance
#' @export
is_ok <- function(x) {
  inherits(x, "ResultMonad") & inherits(value(x), "ok")
}

#' @export
print.ok <- function(x, ...) {
  cat("OK:\n")
  print(unclass(x))
}

