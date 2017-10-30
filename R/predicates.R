
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
#' @param tol scalar double: tolerance within which a whole number will be declared
#' @param n scalar wholenumber: expected length of vector
#'
#' @return scalar logical
#'
#' @export
#'
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5, n = NULL) {
    if (!is.numeric(x))
        return(FALSE)
    if (is.numeric(x) && !all(abs(x - round(x)) < tol))
        return(FALSE)
    if (!rlang::is_null(n) && length(x) != n)
        return(FALSE)
    return(TRUE)
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
#' @param na_rm scalar logical : Whether or not to include NA as a value in the
#'   unique calculation
#'
#' @return scalar logical
#' @export
#'
#' @examples
#' is_binary_valued(c(1, 2))
#' is_binary_valued(c("1", "1", NA))
#' is_binary_valued(c("1", "1", NA), na_rm = FALSE)
is_binary_valued <- function(x, na_rm = TRUE) {
    fact <- as.factor(x)

    if (!na_rm) {
        nval <- length(unique(fact))
    } else {
        nval <- length(levels(fact))
    }

    return(nval == 2)
}


#' Checks if a vector is positive numeric
#'
#'
#' @family predicates
#'
#' @param x object to be checked
#' @param n scalar wholenumber: expected length of vector
#' @return scalar logical
#' @export
is_positive_numeric <- function(x, n = NULL) {
    if (!is.numeric(x))
        return(FALSE)
    if (is.numeric(x) && !all(x > 0, na.rm = TRUE))
        return(FALSE)
    if (!rlang::is_null(n) && length(x) != n)
        return(FALSE)
    return(TRUE)
}

#' Checks if a vector is non-negative numeric
#'
#'
#' @family predicates
#'
#' @param x object to be checked
#' @param n scalar wholenumber: expected length of vector
#' @return scalar logical
#' @export
is_non_negative_numeric <- function(x, n = NULL) {
    if (!is.numeric(x))
        return(FALSE)
    if (is.numeric(x) && !all(x >= 0, na.rm = TRUE))
        return(FALSE)
    if (!rlang::is_null(n) && length(x) != n)
        return(FALSE)
    return(TRUE)
}
