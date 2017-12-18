# Predicate functions on which there is not a typed NA for that property should return NA if NA encountered



# is_numeric --------------------------------------------------------------

#' Bare type predicates
#'
#' These predicates check for a given type but only return TRUE for bare R
#' objects. Bare objects have no class attributes. For example, a data frame is a
#' list, but not a bare list.
#'
#' @family predicates
#'
#' @inheritParams rlang::is_bare_numeric
#' @export
#'
is_scalar_bare_numeric <- function(x) {
    rlang::is_bare_numeric(x, n = 1)
}



# numeric_like ------------------------------------------------------------
#' @rdname is_numeric_like
#' @export
are_numeric_like <- function(x) {
    if (!rlang::is_vector(x)) {
        rlang::abort("`x` must be a vector")
    }

    if (is.factor(x))
        x %<>% as.character()

    # NA preserving check. NAs -> TRUE, non-numeric like -> FALSE
    x %>%
    purrr::map_lgl(~ifelse(rlang::is_na(.), TRUE, suppressWarnings(!rlang::is_na(as.numeric(.)))))
}

#' Checks if a an object can be converted to a number without error
#'
#' @family predicates
#'
#' @template is_predicate
#' @export
#'
is_numeric_like <- function(x, n = NULL, na_rm = FALSE) {

    if (!rlang::is_atomic(x))
        return(FALSE)
    if (!rlang::is_null(n) && length(x) != n)
        return(FALSE)

    all(are_numeric_like(x), na.rm = na_rm)
}


#' @rdname is_numeric_like
#' @export
is_scalar_numeric_like <- function(x) {
    is_numeric_like(x, n = 1)
}

# binary_valued -----------------------------------------------------------


#' Check if a vector has only two unique entries
#'
#' @template is_predicate
#' @export
#'
is_binary_valued <- function(x, n = NULL, na_rm = FALSE) {

    if (!rlang::is_atomic(x))
        return(FALSE)
    if (!rlang::is_null(n) && length(x) != n)
        return(FALSE)

    uniques <- unique(x)

    if (na_rm && anyNA(uniques)) {
        nval <- length(uniques) - 1
    } else {
        nval <- length(uniques)
    }

    nval == 2

}


# whole_number ----------------------------------------------------------

#' @rdname is_whole_number
#' @export
are_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
    if (!rlang::is_vector(x)) {
        rlang::abort("`x` must be a vector")
    }
   abs(x - round(x)) < tol
}

#' Checks if an object is a whole number
#'
#' Checks if an object is equal to its rounded form, minus some tolerance.
#' @template is_predicate
#' @param tol scalar numeric: tolerance within which a whole number will be
#'   declared
#'
#' @export
is_whole_number <- function(x, tol = .Machine$double.eps^0.5, n = NULL, na_rm = FALSE) {

    if (!is.numeric(x))
        return(FALSE)
    if (!rlang::is_null(n) && length(x) != n)
        return(FALSE)
    if (all(rlang::are_na(x)) || rlang::is_empty(x)) {
        return(NA)
    }

    all(are_whole_number(x, tol), na.rm = na_rm)

}

#' @rdname is_whole_number
#' @export
is_scalar_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
    is_whole_number(x, tol, n = 1, na_rm = FALSE)
}


# sign --------------------------------------------------------------------
#' @rdname is_positive
#' @export
are_positive <- function(x) {
    if (!rlang::is_vector(x)) {
        rlang::abort("`x` must be a vector")
    }
    x > 0
}

#' Checks if a vector is positive numeric
#'
#' @template is_predicate
#' @export
is_positive <- function(x, n = NULL, na_rm = FALSE) {
    if (!is.numeric(x))
        return(FALSE)
    if (!rlang::is_null(n) && length(x) != n)
        return(FALSE)
    if (all(rlang::are_na(x)) || rlang::is_empty(x)) {
        return(NA)
    }

    all(are_positive(x), na.rm = na_rm)
}

#' @rdname is_non_negative
#' @export
are_non_negative <- function(x) {
    if (!rlang::is_vector(x)) {
        rlang::abort("`x` must be a vector")
    }
    x >= 0
}

#' Checks if a vector is non-negative numeric
#'
#' @template is_predicate
#' @export
is_non_negative <- function(x, n = NULL, na_rm = FALSE) {
    if (!is.numeric(x))
        return(FALSE)
    if (!rlang::is_null(n) && length(x) != n)
        return(FALSE)
    if (all(rlang::are_na(x)) || rlang::is_empty(x)) {
        return(NA)
    }

    all(are_non_negative(x), na.rm = na_rm)
}



# zero --------------------------------------------------------------------


#' Checks if a vector is non-negative numeric
#'
#'
#' @family predicates
#'
#' @param x object to be checked
#' @return vector logical of same length as x
#' @export
are_zero <- function(x) {
    if (!rlang::is_vector(x)) {
        rlang::abort("`x` must be a vector")
    }
    x == 0
}

#' Checks if a vector is non-negative numeric
#'
#' @template is_predicate
#' @export
is_zero <- function(x, n = NULL, na_rm = FALSE) {
    if (!is.numeric(x))
        return(FALSE)
    if (!rlang::is_null(n) && length(x) != n)
        return(FALSE)
    if (all(rlang::are_na(x)) || rlang::is_empty(x)) {
        return(NA)
    }
    all(are_zero(x), na.rm = na_rm)
}


# NULL --------------------------------------------------------------------

#' Checks if object is not null
#'
#' @template is_predicate
#'
#' @export
is_not_null <- function(x) {
    !rlang::is_null(x)
}
