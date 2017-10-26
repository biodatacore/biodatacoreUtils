
# Predicates --------------------------------------------------------------

#' Check if object is scalar (length 1) numeric
#'
#' @family predicates
#'
#' @param x object to be checked
#'
#' @return scalar logical
#' @export
#'
is_scalar_numeric <- function(x) {
    is.numeric(x) && length(x) == 1
}


#' Checks if an object is/contains only whole numbers
#'
#' @family predicates
#'
#' @param x object to be checked.
#' @param tol tolerance within which a whole number will be declared
#'
#' @return scalar logical
#'
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


#' Checks if a an object can be converted to a number without error
#'
#' @family predicates
#'
#' @param x object to be checked
#' @param ... argument passed to \code{\link{anyNA}}
#' @return scalar logical
#' @export
#'
#' @examples
#' is_numeric_like(c("5", "1.23", "-3.14", "hello", "?"))
is_numeric_like <- function(x, ...) {
    suppressWarnings(!anyNA(as.numeric(x), ...))
}



#' is_binary_valued
#'
#' Checks if a vector has only two unique entries
#'
#' @family predicates
#'
#' @param x object to be checked
#' @param na.rm scalar logical : Whether or not to include NA in the unique calculation
#'
#' @return scalar logical
#' @export
#'
#' @examples
#' is_binary_valued(c(1, 2))
#' is_binary_valued(c("1", "1", NA))
#' is_binary_valued(c("1", "1", NA), na.rm = FALSE)
is_binary_valued <- function(x, na.rm = TRUE) {
    f <- as.factor(x)

    if (!na.rm) {
        nval <- length(unique(f))
    } else {
        nval <- length(levels(f))
    }

    return(nval == 2)
}
