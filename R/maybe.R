#' Maybe Monad
#'
#' @description Creates a `Maybe` monad instance which can be used with the flatMap 
#' operator `%>>=%`. See the `Monads in R` vignette for more details.
#'
#' This container, when flatMapped, always returns a value, either of type 
#' `Nothing` (if an error was raised or the result was `NULL`), or `Just`.
#'
#' @importFrom R6 R6Class
#' @seealso [Nothing()], [is_nothing()], [Just()], [is_just()]
#'
#' @export
Maybe <- R6::R6Class(
  c("MaybeMonad"),
  public = list(
    #' @field value value contained in the instance
    value = NULL,
    #' @description create a new `Maybe` instance
    #' @param value value to be contained
    #' @return a new `Maybe` instance
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
        maybe <- Maybe$new(Nothing())
      } else {
        maybe <- Maybe$new(Just(result))
      }
      maybe
    }
  )
)

#' Create a new `Maybe` instance
#'
#' @param value value to be contained
#'
#' @returns a `Maybe` monad instance
#' @export
maybeM <- function(value) {
  v <- rlang::enquo(value)
  Maybe$new(v)
}

#' @export
print.MaybeMonad <- function(x, ...) {
  print(value(x))
}

#' Create an `Nothing` instance
#'
#' @family maybe
#' @returns a `Nothing` instance
#' @export
Nothing <- function() {
  structure(list(), class = "nothing")
}

#' Test for `Nothing`
#'
#' @param x object to be tested
#'
#' @family maybe
#' @returns logical; whether `x` is a `Nothing` instance
#' @export
is_nothing <- function(x) {
  inherits(x, "MaybeMonad") & inherits(value(x), "nothing")
}

#' @export
print.nothing <- function(x, ...) {
  cat("Nothing\n")
  invisible()
}

#' Create a `Just` instance
#'
#' @param x value to be contained
#'
#' @family maybe
#' @returns a `Just` instance
#' @export
Just <- function(x) {
  structure(x, class = "just")
}

#' Test for `Just`
#'
#' @param x object to be tested
#'
#' @family maybe
#' @returns logical; whether `x` is a `Just` instance
#' @export
is_just <- function(x) {
  inherits(x, "MaybeMonad") & inherits(value(x), "just")
}

#' @export
print.just <- function(x, ...) {
  cat("Just:\n")
  print(unclass(x))
}



