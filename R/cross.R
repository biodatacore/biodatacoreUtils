#' Crosses some vectors in a list, splicing other elements into each cross
#' Useful for generating lists of substitutions
#'
#' @param .l list of vectors
#' @param .at A character vector of names or a numeric vector of positions
#' @param ... arguments passed to \code{\link[purrr]{cross}}
#' @return list
#' @export
#'
cross_at <- function(.l, .at, ...) {
    sel <- inv_which(.l, .at)

    purrr::cross(.l[sel], ...) %>%
        purrr::map(~c(., .l[!sel]))
}

