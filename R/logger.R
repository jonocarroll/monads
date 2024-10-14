#' Logger Monad
#'
#' @description Creates a `Logger` monad instance which can be used with the flatMap 
#' operator `%>>=%`. See the `Monads in R` vignette for more details.
#'
#' This container, when flatMapped, records the expressions used at each step, 
#' and these can be recovered from the object, potentially for re-evaluation.
#'
#' @importFrom R6 R6Class
#' @seealso [rerun()], [logger_log()]
#'
#' @export
Logger <- R6::R6Class(
  c("LoggerMonad"),
  public = list(
    #' @field value value contained in the instance
    value = NULL,
    #' @field log log of the evaluated expressions
    log = NULL,
    #' @description create a new `Logger` instance
    #' @param value value to be contained
    #' @return a new `Logger` instance
    initialize = function(value) {
      if (rlang::is_quosure(value)) {
        self$value <- rlang::eval_tidy(value)
        self$log <- rlang::quo_text(value)
      } else {
      self$value <- value
      self$log <- rlang::quo_text(substitute(value))
      }
    },
    #' @description bind when using flatMap (`%>>=%`)
    #' @param f function to apply
    #' @param expr expression of the function
    bind = function(f, expr) {
      result <- tryCatch(f(self$value), error = function(e) e)
      if (inherits(result, "error") | is.null(self$value)) {
        logger <- Logger$new(NULL)
        logger$log <- c(self$log, paste0("[E] ", rlang::quo_text(expr)))
      } else {
        logger <- Logger$new(result)
        logger$log <- c(self$log, rlang::quo_text(expr))
      }
      logger
    }
  )
)

#' Create a new `Logger` instance
#'
#' @param value value to be contained
#'
#' @returns a `Logger` monad instance
#' @export
loggerM <- function(value) {
  v <- rlang::enquo(value)
  Logger$new(v)
}

#' @export
print.LoggerMonad <- function(x, ...) {
  print(value(x))
}

#' (Re)-evaluate the logged commands as a regular pipeline
#'
#' @param x a `Logger` monad instance
#'
#' @family logger
#' @returns the evaluated output of the logged pipeline
#' @export
rerun <- function(x) {
  stopifnot("Input should be a `Logger` instance" = inherits(x, "LoggerMonad"))
  logs <- paste(x$log, collapse = " %>% ")
  eval(parse(text = logs))
}

#' @keywords internal
pipeline <- function(x, combined = FALSE) {
  out <- list()
  out <- c(out, paste0(x$log[[1]], " %>%\n"))
  len <- length(x$log)
  if (len > 2) {
    for (op in 2:(len-1)) {
      out <- c(out, paste0("   ", x$log[[op]], " %>%\n"))
    }
  }
  out <- c(out, paste0("   ", x$log[[len]]))
  if (combined) return(paste(out, collapse = ""))
  out
}

#' Extact the log
#'
#' @param x a `Logger` monad instance
#' @param as_text (logical) should the log be returned as text?
#'
#' @family logger
#' @returns if `as_text`, a string, otherwise the log is printed to console
#' @export
logger_log <- function(x, as_text = FALSE) {
  stopifnot("Input should be a `Logger` instance" = inherits(x, "LoggerMonad"))
  p <- pipeline(x, combined = TRUE)
  if (grepl("[E]", p, fixed = TRUE)) {
    cli::cli_inform(c(x = "Log of {length(x$log)-1} operations: [ERROR]"))
  } else {
    cli::cli_inform(c(v = "Log of {length(x$log)-1} operations:"))
  }
  if (as_text) {
    p
  } else {
    cat("\n", p)
  }
}

