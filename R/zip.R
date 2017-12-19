
# Zip ---------------------------------------------------------------------

.zip <- function(.l, .type) {
    purrr::pmap(.l, .type)
}

#' Zip together lists of vectors ala python
#'
#' \code{zip} alwyas returns a lists of lists. \code{zip_} returns a lists of
#' vectors of the corresponding type.
#'
#' @note zip unzips itself!
#'
#' @param .l list of vectors
#' @param .x,.y vectors
#' @return list
#' @export
#'
zip <- function(.l) {
    .zip(.l, list)
}

#' @rdname zip
#' @export
zip_lgl <- function(.l) {
    .zip(.l, rlang::lgl)
}

#' @rdname zip
#' @export
zip_chr <- function(.l) {
    .zip(.l, rlang::chr)
}

#' @rdname zip
#' @export
zip_int <- function(.l) {
    .zip(.l, rlang::int)
}

#' @rdname zip
#' @export
zip_dbl <- function(.l) {
    .zip(.l, rlang::dbl)
}

#' @rdname zip
#' @export
zip2 <- function(.x, .y) {
    zip(list(.x, .y))
}

#' @rdname zip
#' @export
zip2_lgl <- function(.x, .y) {
    zip_lgl(list(.x, .y))
}

#' @rdname zip
#' @export
zip2_chr <- function(.x, .y) {
    zip_chr(list(.x, .y))
}

#' @rdname zip
#' @export
zip2_int <- function(.x, .y) {
    zip_int(list(.x, .y))
}

#' @rdname zip
#' @export
zip2_dbl <- function(.x, .y) {
    zip_dbl(list(.x, .y))
}
