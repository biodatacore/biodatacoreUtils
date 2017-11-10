#' fo_to_char
#'
#' Converts a formula to character representaion.
#'
#' `fo_to_char`` is a function provided for use to interface with
#' `as.character.formula`. If 'biodatacoreUtils' is loaded, then `as.character`
#' should automatically find the `.formula` method. `deparse_to_str` is the
#' underlying function that does the conversion.
#'
#' @param x formula
#' @param ... arguments passed to and from other methods.
#'
#' @return A character vector
#' @export
fo_to_char <- function(x, ...) {
    stopifnot(rlang::is_formula(x))
    as.character.formula(x, ...)
}

#' @rdname fo_to_char
#' @aliases as.character as.character.formula
#' @export
as.character.formula <- function(x, ...) {
    deparse_to_str(x)
}
