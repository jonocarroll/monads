#' Square a Number
#'
#' @param x number to be squared
#'
#' @family helpers
#' @returns `x^2`
#' @export
square <- function(x) {
  stopifnot('x should be numeric' = is.numeric(x))
  x ^ 2
}

#' Add `n` to a Number
#'
#' @param x number to which `n` should be added
#' @param n number to add to `x`
#'
#' @family helpers
#' @returns `x+n`
#' @export
add_n <- function(x, n) {
  stopifnot("x should be numeric" = is.numeric(x))
  stopifnot("n should be numeric" = is.numeric(n))
  x + n
}

#' Double a Number
#'
#' @param x number to be doubled
#'
#' @family helpers
#' @returns `2*x`
#' @export
timestwo <- function(x) {
  stopifnot("x should be numeric" = is.numeric(x))
  2 * x
}

#' Return `NULL`
#'
#' @family helpers
#' @returns `NULL`
#' @export
ret_null <- function() NULL

#' Raise an Error but Return a Value
#'
#' @param x value to return
#' @param msg error message to raise
#'
#' @family helpers
#' @returns `x`, with an error
#' @export
ret_err <- function(x, msg) stop(msg)

#' Sleep but Return a Value
#'
#' @param x value to return
#' @param n number of seconds to sleep for
#'
#' @family helpers
#' @returns `x` after sleeping for `n` seconds
#' @export
sleep_for <- function(x, n) {
  stopifnot("n should be numeric" = is.numeric(n))
  Sys.sleep(n)
  x
}
