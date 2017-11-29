

# mzid --------------------------------------------------------------------

#' Creates new mzid object
#'
#' @keywords internal
#'
#' @param mz,rt scalar numeric: The mz and rt.
#' @param n_mz,n_rt scalar integerish: The number of digits after the decimal
#'   point from the mz and the rt to be used in
#' @param sep scalar character: character separator in mzid
#' @param tag scalar character: tag at start of mzid
#' @param source scalar character: data source of the mzids. Pass `NA` to keep
#'   the field, but fill with `NA`, pass `NULL` to create an object without the
#'   fields.
#'
#' @return mzid object
new_mzid <- function(mz, rt, n_mz, n_rt, sep, tag, source) {
    stopifnot(is_non_negative(mz, n = 1))
    stopifnot(is_non_negative(rt, n = 1))
    # stopifnot(length(mz) == length(rt))
    stopifnot(rlang::is_scalar_integerish(n_mz))
    stopifnot(rlang::is_scalar_integerish(n_rt))
    stopifnot(rlang::is_scalar_character(sep))
    stopifnot(rlang::is_scalar_character(tag))
    stopifnot(rlang::is_na(source) || rlang::is_scalar_character(source))

    # + 1 includes the decimal point the other numbers include numbers to its left


    # Just using `formatC` will round the last digit. This can make the mzid
    # mismatch with its individual mzs and rts. `trunc2` helps to avoid this.
    mz_trunc <-
        mz %>%
        trunc2(d = n_mz) %>%
        formatC(digits = n_mz, format = 'f')


    rt_trunc <-
        rt %>%
        trunc2(d = n_rt) %>%
        formatC(digits = n_rt, format = 'f')
    # rt %>%
    # as.character() %>%
    # stringr::str_sub(end = n_rt + 1 + nchar(floor(mz)))

    structure(
        paste(tag,
              mz_trunc,
              rt_trunc,
              sep = sep),
        mz = mz,
        rt = rt,
        source = source,
        class = c('mzid', 'character')
    )
}


#' Checks if object is mzid
#'
#' Returns original input
#'
#' @keywords internal
#'
#' @param x R object
#'
validate_mzid <- function(x) {
    stopifnot(inherits(x, 'mzid'))
    stopifnot(c('mz', 'rt', 'source') %in% names(attributes(x)))
    x
}

#' Creates a new mzid object
#'
#' What is an mzid object and why would I want to use it?
#'
#' @inheritParams new_mzid
#'
#' @return mzid object
#' @export
#'
mzid <- function(mz, rt, n_mz = 6, n_rt = 4, sep = '_', tag = 'mzid', source = NA) {
    # Right now there is no validation because validate_mzid only checks for
    # things that are guarenteed to be there after new_mzid is called.

    # validate_mzid(
    new_mzid(
        mz = mz,
        rt = rt,
        n_mz = n_mz,
        n_rt = n_rt,
        sep = sep,
        tag = tag,
        source = source
    )
    # )
}
# mzids --------------------------------------------------------------------


#' Title
#'
#' @param mz,rt vector numeric: The mz and rt. Scalar values are broadcast.
#' @param n_mz,n_rt scalar integerish: The number of digits after the decimal
#'   point from the mz and the rt to be used in
#' @param sep scalar character: character separator in mzid
#' @param tag scalar character: tag at start of mzid
#' @param source vector character: data source of the mzids. Pass `NA` to keep
#'   the field, but fill with `NA`, pass `NULL` to create an object without the
#'   fields. Scalar values are broadcast
#'
#' @return mzids object
#'
new_mzids <- function(mz, rt, n_mz, n_rt, sep, tag, source = NA) {
    stopifnot(!rlang::is_null(source))

    structure(
        purrr::pmap(list(mz, rt, source), function(mz, rt, source) {
            mzid(mz, rt, n_mz, n_rt, sep, tag, source)
        }),
        class = c('mzids', 'list')
    )


}

#' Title
#'
#' @keywords internal
#' @param x object
#'
#' @return object: same as `x`
#'
validate_mzids <- function(x) {

    stopifnot(inherits(x, 'mzids'))

    # names
    if (rlang::is_named(x)) {
        msg <-
            paste(
                'mzids object should not have names'
            )

        rlang::abort(glue::glue(msg))
    }

    # class check
    if (!purrr::every(x, ~inherits(., 'mzid'))) {

        bad_idxs <- which(!purrr::map_lgl(x, ~inherits(., "mzid")))
        msg <-
            paste(
                'There are non-mzid objects in mzids.',
                'Non-mzid indices: {paste(bad_idxs, collapse = ",")}'
            )
        rlang::abort(glue::glue(msg))
    }

    # duplicate mzids by source
    values <- purrr::map_chr(x, unclass)
    sources <- purrr::map_chr(x, ~attr(., 'source'))

    sources %<>% forcats::fct_explicit_na()

    values %>%
        split(sources) %>%
        purrr::iwalk(function(vals, srce) {
            if (anyDuplicated(vals)) {
                dups <- unique(vals[duplicated(vals)])
                msg <-
                    paste(
                        'There are duplicated mzids in source: {srce}.',
                        'Try increasing "n_mz" or "n_rt".',
                        '\nDuplicated values: {paste(dups, collapse = ", ")}'
                    )
                rlang::abort(glue::glue(msg))
            }
        })

    x
}


#' Creates a new mzids object
#'
#' What is an mzids object and why would I want to use it?
#'
#' @inheritParams new_mzids
#'
#' @return mzid object
#' @export
#'
mzids <-
    function(mz,
             rt,
             n_mz = 6,
             n_rt = 4,
             sep = '_',
             tag = 'mzid',
             source = NA) {
        validate_mzids(new_mzids(
            mz = mz,
            rt = rt,
            n_mz = n_mz,
            n_rt = n_rt,
            sep = sep,
            tag = tag,
            source = source
        ))


    }

# Methods -----------------------------------------------------------------

#' Prints an mzid object as a data frame
#'
#' @param x mzid object
#' @param ... objects passed to \code{\link[dplyr]{as_data_frame}}
#'
#' @aliases as.data.frame as.data.frame.mzid
#' @export
#'
as.data.frame.mzid <- function(x, ...) {
    atbs <- attributes(x)
    atbs %<>% .[names(.) != 'class']

    c(list(mzid = unclass(x)[1]), atbs) %>%
        dplyr::as_data_frame(... = ...)
}



#' Title
#'
#'
#' @param x R object
#'
#' @return scalar logical
#' @export
#'
is_mzid <- function(x) {
    validate_mzid(x)
}

#' @rdname is_mzid
#' @export
#'
is_mzids <- function(x) {
    validate_mzids(x)
}

