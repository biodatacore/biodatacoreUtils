
#' Given an object and a character or integerish vector, return a logical vctor
#' of positions.
#'
#' Used for `_at` type functions.
#'
#' @keywords internal
#'
#' @param x lits or atomic vector
#' @param sel A character vector of names or a numeric vector of positions
#'
#' @return vector logical of same length as x
#'
#' @export
inv_which <- function(x, sel) {
    if (is.character(sel)) {
        names <- names(x)
        if (is.null(names)) {
            stop("character indexing requires a named object", call. = FALSE)
        }
        names %in% sel
    } else if (is.numeric(sel)) {
        if (any(sel < 0)) {
            !seq_along(x) %in% abs(sel)
        } else {
            seq_along(x) %in% sel
        }

    } else {
        stop("unrecognised index type", call. = FALSE)
    }
}


#' Internal version of `map_lgl()`` that works with logical vectors
#'
#' Used for `_if` type functions
#'
#' @keywords internal
#'
#' @param x lits or atomic vector
#' @param .p predicate function
#'
#' @return vector logical of same length as x
#'
#' @export
probe <- function(.x, .p, ...) {
    if (rlang::is_logical(.p)) {
        stopifnot(length(.p) == length(.x))
        .p
    } else {
        purrr::map_lgl(.x, .p, ...)
    }
}
