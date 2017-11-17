#' Combines lists of vectors. Useful for generating lists of substitutions
#'
#' @param x list of vectors
#' @param method scalar character. one of \code{c('stack', 'merge', 'cross')}.
#'
#' @section Combination Method: `cross` generates all pairwise combinations. `merge` combines element wise. `stack` generates one long list.
#' @return list
#' @export
#'
lst_combine <- function(x, method = c('stack', 'merge', 'cross')) {
    # stopifnot(rlang::is_named(x))
    # stopifnot(!anyDuplicated(names(x)))
    method <- match.arg(method)

    # split each name-values set into individual list elements
    if (method == 'cross') {
        purrr::cross(x)
    } else if (method == 'merge') {
        purrr::pmap(x, list)
    } else {
        # imap passes characters if named, and indices otherwise. Only want to
        # inherit proper character names
        purrr::imap(x, function(.x, nm) {
            # as.list maps each element in a vector to a single element in a
            # list
            if (is.numeric(nm) || nchar(nm) == 0) {
                as.list(.x)
            } else {
                rlang::set_names(as.list(.x), rep(nm, length(.x)))
            }
        }) %>%
            purrr::reduce(c) %>%
            purrr::lmap(list)
    }

}
