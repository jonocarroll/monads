#' flatMap
#'
#' Applies the `rhs` as an argument to `lhs` in the context of the `lhs` monad. 
#' 
#' Essentially calls `lhs$bind(rhs)` which for most cases, will return `rhs(lhs)` 
#' with whatever context the monad provides.
#'
#' @param lhs left side of infix
#' @param rhs right side of infix
#'
#' @returns `rhs(lhs)` accounting in the (monad) context of `lhs`
#' @export
`%>>=%` <- function(lhs, rhs) {
  
  eval_call <- function(verb, args) {
    function(data) {
      eval(rlang::call2(verb, data, !!!args))
    }
  }
  
  rhs_quo <- rlang::enquo(rhs)
  rhs_expr <- rlang::quo_get_expr(rhs_quo)
  
  verb <- rlang::call_name(rhs_expr)
  args <- rlang::call_args(rhs_expr)
  .call <- eval_call(verb, args)
  
  if (inherits(lhs, "R6") & any(grepl("Monad", class(lhs)))) {
    lhs$bind(.call, rhs_quo)
  } else {
    stop("lhs must be a Monad class; e.g. Logger, Maybe, Result, or List")
  }
  
}
