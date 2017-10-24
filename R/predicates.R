
# Predicates --------------------------------------------------------------

#' Check if object is scalar (length 1) numeric
#'
#' @param x object to be checked
#'
#' @return scalar logical
#' @export
#'
#' @family predicates
is_scalar_numeric <- function(x) {
    is.numeric(x) && length(x) == 1
}


#' Checks if an object is/contains only whole numbers
#'
#' @param x object to be checked.
#' @param tol tolerance within which a whole number will be declared
#'
#' @return scalar logical
#'
#' @family predicates
#' @export
#'
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    is.numeric(x) && all(abs(x - round(x)) < tol)
}

#' @rdname is_wholenumber
#' @export
is_scalar_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    is_wholenumber(x, tol) && length(x) == 1
}
