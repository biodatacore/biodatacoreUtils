#' Crosses some vectors in a list, splicing other elements into each cross Useful for generating lists of substitutions
#'
#' @param .l list of vectors
#' @param .at A character vector of names or a numeric vector of positions
#'
#' @return list
#' @export
#'
cross_some <- function(.l, .at) {
    sel <- inv_which(.l, .at)

    purrr::cross(.l[sel]) %>%
        purrr::map(~c(., .l[!sel]))
}

