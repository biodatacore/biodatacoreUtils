# Internal version of map_lgl() that works with logical vectors
probe <- function(.x, .p, ...) {
    if (rlang::is_logical(.p)) {
        stopifnot(length(.p) == length(.x))
        .p
    } else {
        purrr::map_lgl(.x, .p, ...)
    }
}

# opposite of `which`
inv_which <- function(x, sel) {
    if (is.character(sel)) {
        names <- names(x)
        if (is.null(names)) {
            stop("character indexing requires a named object", call. = FALSE)
        }
        names %in% sel
    } else if (is.numeric(sel)) {
        seq_along(x) %in% sel
    } else {
        stop("unrecognised index type", call. = FALSE)
    }
}

#' Fill does things
#'
#' @param .f A function, formula, or scalar atomic. Must return a scalar atomic.
#'
#'   If a function, it is used as is.
#'
#'   If a formula, e.g. ~ .x + 2, it is converted to a function. There are three
#'   ways to refer to the arguments: For a single argument function, use . For a
#'   two argument function, use .x and .y For more arguments, use ..1, ..2, ..3
#'   etc.
#'
#'   This syntax allows you to create very compact anonymous functions.
#'
#'   If scalar character or scalar numeric, it is converted to a formula, which
#'   is then converted to a function as above. The end result is that the value
#'   is filled in in the selected locations.
#'
#' @param ... Additional arguments passed on to methods.
#' @export
as_filler <- function(.f, ...) {
    if (rlang::is_scalar_atomic(.f)) {
        .f <-
            rlang::lang('~', .f) %>%
            stats::as.formula(.) # simply calling `~` does not make it a function

        # Making things the same as if you had called `~` from the parent env.
        rlang::f_env(.f) <- rlang::caller_env()
    }

    .f <- rlang::as_closure(.f)

    .f
}



#' Fill locations in a vectors with a single value
#'
#' Fill fills in selected indices of that vector with the scalar result of a
#' function applied to that vector. Types are coerced upwards if the fill value
#' is of a different type from the original vector. e.g. logical -> integer ->
#' double -> character.
#'
#' @param .x atomic vector
#' @inheritParams as_filler
#' @param .p A single predicate function, a formula describing such a predicate
#'   function, or a logical vector of the same length as .x. Alternatively, if
#'   the elements of .x are themselves lists of objects, a string indicating the
#'   name of a logical element in the inner lists. Only those elements where .p
#'   evaluates to TRUE will be modified.
#' @param .at A character vector of names or a numeric vector of positions. Only
#'   those elements corresponding to .at will be modified.
#' @param ... Additional arguments passed on to .f.
#'
#' @return atomic vector of same length as .x
#'
#' @name fill
NULL

#' @rdname fill
#' @export
fill_if <- function(.x, .p, .f, ...) {
    stopifnot(rlang::is_atomic(.x))
    sel <- probe(.x, .p)
    .f <- as_filler(.f)

    fill <- .f(.x, ...)
    stopifnot(rlang::is_scalar_atomic(fill))

    .x[sel] <- fill
    .x
}

#' @rdname fill
#' @export
fill_at <- function(.x, .at, .f, ...) {
    stopifnot(rlang::is_atomic(.x))
    sel <- inv_which(.x, .at)
    .f <- as_filler(.f)

    fill <- .f(.x, ...)
    stopifnot(rlang::is_scalar_atomic(fill))

    .x[sel] <- fill
    .x
}



#' Fills NAs with 0s
#'
#' @param x vector
#'
#' @return vector
#' @export
#' @seealso zero_to_na
na_to_zero <- function(x) {
    x[rlang::are_na(x)] <- 0
}

#' Fills 0 with NAs
#'
#' @param x vector
#'
#' @return vector
#' @export
#' @seealso na_to_zero
zero_to_na <- function(x) {
    x[x == 0] <- NA
}

# ? modify for changing dataframe columns in an apply like way.
