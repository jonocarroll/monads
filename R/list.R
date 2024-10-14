#' List Monad
#'
#' @description Creates a `List` monad instance which can be used with the flatMap 
#' operator `%>>=%`. See the `Monads in R` vignette for more details.
#'
#' This container, when flatMapped, unpacks the contained values, concatenates 
#' them, passes this to the next expression, then rewraps the result in a new 
#' `List`.
#'
#' @importFrom R6 R6Class
#' @seealso [times()]
#'
#' @export
List <- R6::R6Class(
  c("ListMonad"),
  public = list(
    #' @field value value contained in the instance
    value = NULL,
    #' @description create a new `List` instance
    #' @param ... values to be contained
    #' @return a new `List` instance
    initialize = function(...) {
      if (rlang::is_quosures(...)) {
        self$value <- Map(rlang::eval_tidy, ...)
      } else {
        self$value <- list(...)
      }
    },
    #' @description bind when using flatMap (`%>>=%`)
    #' @param f function to apply
    #' @param expr expression of the function
    bind = function(f, expr) {
      result <- tryCatch(unlist(lapply(unlist(self$value), f)), error = function(e) e)
      if (inherits(result, "error") | is.null(self$value)) {
        l <- List$new()
      } else {
        l <- List$new(result)
      }
      l
    }
  )
)

#' Create a new `List` instance
#'
#' @param ... values to be contained
#'
#' @returns a `List` monad instance
#' @export
listM <- function(...) {
  v <- rlang::enquos(..., .named = NULL)
  List$new(v)
}

#' @export
print.ListMonad <- function(x, ...) {
  print(value(x))
}

