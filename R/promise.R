if (FALSE) {
  Promise <- R6::R6Class(
    c("Monad"), # future doesn't like multiple names and I 
                # can't figure out how to change environmentName()
    public = list(
      value = NULL,
      env = NULL,
      initialize = function(value, env = parent.frame(2L)) {
        print(ls(env))
        self$value <- value
        self$env <- env
      },
      bind = function(f, expr) {
        if (!requireNamespace("future", quietly = TRUE)) {
          stop("Using a Promise requires the {future} package")
        }
        if (!isNamespaceLoaded("future")) attachNamespace("future") # for %<-%
        future::futureAssign("res", {f; f(self$value)}, envir = self$env, assign.env = parent.frame())
        Promise$new(res)
      }
    )
  )
  
  print.Promise <- function(x, ...) {
    if (!future::resolved(future::futureOf(x$value))) {
      cat("UNresolved")
      invisible()
    } else {
      print(future::value(x$value))
    }
  }
  
  # doesn't quite work - can't find sleep_for despite capturing env
  future::plan(future::multisession)
  x <- Promise$new(10) %>>=%
    sleep_for(3) %>>=%
    add_n(2)
  x
  str(x)
  x$value
  future::value(x$value)
}
