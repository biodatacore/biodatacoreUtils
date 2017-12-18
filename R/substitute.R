
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

# TODO write tests
#' Crosses a list of quoted calls with a list of environments, performing quoted
#' substitution each time.
#'
#' Useful for generating lists of model formulas. `f_cross_sub` is a version
#' that enforces formulas.
#'
#'
#' @param exprs a list of quoted calls
#' @param fos a list of formulas
#' @param envs a list of environments, or things that behaves like an
#'   environment (like a list or data frame), or references to an environment
#'   (like a positive integer or name, see \code{\link{as.environment}} for more
#'   details)
#' @param trans a function to be applied to each value in an environment before
#'   it is passed to `substitute_q`. A common vlaue is \code{as.name}, to
#'   substitute in character values as names.
#'
#' @details f_cross_sub maps environments across created calls.
#'
#' @return a nested list of calls
#' @export
#'
cross_sub <- function(exprs, envs, trans = identity) {
    stopifnot(rlang::is_list(exprs) && rlang::is_list(envs))
    stopifnot(rlang::is_function(trans))

    purrr::map(exprs, function(expr) {
        purrr::map(envs, function(env) {
            substitute_q(expr, purrr::map(env, trans))
        })
    })
}

#' @rdname cross_sub
#' @export
f_cross_sub <- function(fos, envs, trans = str_fo_parse) {
    stopifnot(rlang::is_list(fos))
    purrr::walk(fos, ~stopifnot(rlang::is_formula(.)))

    fo_envs <- purrr::map(fos, rlang::f_env)

    call_list <- cross_sub(fos, envs, trans)

    purrr::map2(call_list, fo_envs, function(calls, env) {
        purrr::map(calls, function(call) {
            rlang::f_env(call) <- env
            call
        })
    })
}


#' Parses character vectors for substitution
#'
#' @keywords internal
#'
str_fo_parse <- function(x) {
    parse(text = paste(x, collapse = ' + '))[[1]]
}
