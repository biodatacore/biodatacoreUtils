#' Deparse an object to a string
#'
#' Mostly used for pulling language and call objects to strings.
#'
#' @param x object
#'
#' @return scalar? character
#' @export
deparse_to_str <- function(x) {
    x <- paste(deparse(x), collapse = " ")
    x <- gsub("\\s+", " ", x, perl = FALSE) # remove multiple spaces
    return(x)
}
