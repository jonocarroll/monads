#' Timer Monad
#'
#' Creates a `Timer` monad instance which can be used with the flatMap 
#' operator `%>>=%`. See the `Monads in R` vignette for more details.
#'
#' This container, when flatMapped, records the evaluation time of the 
#' expressions used at each step, and these can be recovered from the object, 
#' along with the expressions themselves.
#'
#' @section Usage:
#' \preformatted{x <- Timer$new(value)
#'
#' x$bind()
#' 
#' x$value
#' x$times
#' }
#'
#' @section Details:
#' \code{$new(value)} creates a new instance wrapping `value`.
#'
#' \code{$bind()} is used within `%>>=%` and re-wraps as a new `Timer`.
#'
#' \code{$value} extracts the `value` field. `value(x)` is a wrapper for this.
#' \code{$times} extracts the `times` field. `times(x)` is a wrapper for this.
#'
#' @importFrom R6 R6Class
#' @seealso [times()]
#'
#' @export
Timer <- R6::R6Class(
  c("TimerMonad"),
  public = list(
    #' @field value value contained in the instance
    value = NULL,
    #' @field times evaluation times
    times = NULL,
    #' @description create a new `Timer` instance
    #' @param value value to be contained
    #' @return a new `Timer` instance
    initialize = function(value) {
      if (rlang::is_quosure(value)) {
        self$value <- rlang::eval_tidy(value)
        self$times <- data.frame(expr = rlang::quo_text(value), time = 0)
      } else {
        self$value <- value
        self$times <- data.frame(expr = rlang::quo_text(substitute(value)), time = 0)
      }
    },
    #' @description bind when using flatMap (`%>>=%`)
    #' @param f function to apply
    #' @param expr expression of the function
    bind = function(f, expr) {
      time <- system.time(
        result <- tryCatch(f(self$value), error = function(e) e)
      )
      if (inherits(result, "error") | is.null(self$value)) {
        timer <- Timer$new(NULL)
      } else {
        timer <- Timer$new(result)
      }
      timer$times <- rbind(
        self$times, 
        data.frame(expr = rlang::quo_text(expr), time = time[["elapsed"]])
      )
      timer
    }
  )
)

#' Create a new `Timer` instance
#'
#' @param value value to be contained
#'
#' @returns a `Timer` monad instance
#' @export
timerM <- function(value) {
  v <- rlang::enquo(value)
  Timer$new(v)
}

#' @export
print.TimerMonad <- function(x, ...) {
  print(value(x))
}

#' Extract the times
#'
#' @param x a `Timer` monad instance
#' @param digits minimum number of digits to print 
#'
#' @returns a `data.frame` of the commands and their timings
#' @export
times <- function(x, digits = 6) {
  stopifnot("Input should be a `Timer` instance" = inherits(x, "TimerMonad"))
  print(x$times, digits = digits)
}


