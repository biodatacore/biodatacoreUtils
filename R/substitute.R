
# Environment -------------------------------------------------------------

to_env <- function(x, quiet = FALSE) {
    if (is.environment(x)) {
        x
    } else if (is.list(x)) {
        list2env(x)
    } else if (is.function(x)) {
        environment(x)
    } else if (length(x) == 1 && is.character(x)) {
        if (!quiet) message("Using environment ", x)
        as.environment(x)
    } else if (length(x) == 1 && is.numeric(x) && x > 0) {
        if (!quiet) message("Using environment ", search()[x])
        as.environment(x)
    } else {
        stop("Input can not be coerced to an environment", call. = FALSE)
    }
}



# Substitute --------------------------------------------------------------



#' A version of substitute that evaluates its first argument.
#'
#' This version of substitute is needed because \code{substitute} does not
#' evaluate it's first argument, and it's often useful to be able to modify
#' a quoted call.
#'
#' @param x a quoted call
#' @param env an environment, or something that behaves like an environment
#'   (like a list or data frame), or a reference to an environment (like a
#'   positive integer or name, see \code{\link{as.environment}} for more
#'   details)
#' @export
#' @examples
#' x <- quote(a + b)
#' substitute(x, list(a = 1, b = 2))
#' substitute_q(x, list(a = 1, b = 2))
substitute_q <- function(x, env) {
    stopifnot(is.language(x))
    env <- to_env(env)

    call <- substitute(substitute(x, env), list(x = x))
    eval(call)
}



