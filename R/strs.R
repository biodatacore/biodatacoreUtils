#' Expression Deparsing
#'
#' Turn unevaluated expressions into character strings. With additional
#' operations to make it a little neater.
#'
#' @param expr any R expression
#' @param ... arguments passed to \code{\link{deparse}}
#'
#' @return scalar? character
#' @export
deparse2 <- function(expr, ...) {
    x <- paste(deparse(expr, ...), collapse = " ")
    x <- gsub("\\s+", " ", x, perl = FALSE) # remove multiple spaces
    return(x)
}
