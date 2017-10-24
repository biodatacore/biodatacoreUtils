#' A Function To Reverse The Role Of Names And Values In A List.
#'
#' Given a list with names `x` and values in a set `y` this function returns a
#' list with names in `y` and values in `x`.
#'
#' @param lst list: A named list with values that are vectors.
#'
#' @details First the list is unrolled to provide a two long vectors, names are
#'   repeated, once for each of their values. Then the names are split by the
#'   values. This turns out to be useful for inverting mappings between one set
#'   of identifiers and an other.
#' @return list: A list with length equal to the number of distinct values in the
#'   input list and values from the names of the input list.
#' @seealso \code{\link{split}}
#' @note This function is taken from the \code{Biobase} package
#' @export
#'
#' @examples l1 = list(a=1:4, b=c(2,3), d=c(4,5))
#' reverse_split(l1)
reverse_split = function(lst) {

    if (length(lst) == 0) {
        return(lst)
    }
    lens <- sapply(lst, length)
    nms <- rep(names(lst), lens)
    vals <- unlist(lst)
    split(nms, vals)
}


#' Converts a dataframe to a matrix where a column of the dataframe
#' becomes the rownames of the matrix
#'
#' @param x data frame
#' @param nm_col int: which column should be removed and used as row names?
#'
#' @return matrix
#' @export
#' @keywords internal
dat_to_mat <- function(x, nm_col = 1) {
    stopifnot(is.data.frame(x))
    stopifnot(is_scalar_wholenumber(nm_col))

    mat <- as.matrix(x[, -nm_col])
    rownames(mat) <- x[[nm_col]]

    return(mat)
}

#' Helper functions to compute negative logs.
#'
#' @param x vector numeric
#' @param base scalar numeric: the base with respect to which logarithms are
#'   computed. Defaults to exp(1).
#'
#' @return vector numeric
#' @export
#'
neg_log <- function(x, base = exp(1)) {
    -1*log(x, base)
}

#' @rdname neg_log
#' @export
neg_log_10 <- function(x) {
    -1*log10(x)
}
