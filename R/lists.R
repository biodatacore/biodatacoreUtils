#' Combines lists of vectors. Useful for generating lists of substitutions
#'
#' @param x list of vectors
#' @param method scalar character. one of \code{c('stack', 'merge', 'cross')}.
#'
#' @section Combination Method: `cross` generates all pairwise combinations. `merge` combines element wise. `stack` generates one long list.
#' @return list
#' @export
#'
lcombine <- function(x, method = c('stack', 'merge', 'cross')) {
    stopifnot(rlang::is_named(x))
    stopifnot(!anyDuplicated(names(x)))
    method <- match.arg(method)

    # split each name-values set into individual list elements
    if (method == 'cross') {
        purrr::cross(x)
    } else if (method == 'merge') {
        purrr::pmap(x, list)
    } else {
        x %>%
            utils::stack() %>%
            dplyr::mutate(ind = as.character(.data$ind)) %>%# strings as factor nonsense
            as.list() %>%
            purrr::pmap(~purrr::set_names(list(.x), .y))
    }

}
