context('lists')


# lcombine ----------------------------------------------------------------


test_that('returns a list of depth 3', {
    lst <- list(a = 1:10, b = 11:20, c = 21:30)

    expect_equal(purrr::vec_depth(lcombine(lst, 'stack')), 3)
    expect_equal(purrr::vec_depth(lcombine(lst, 'merge')), 3)
    expect_equal(purrr::vec_depth(lcombine(lst, 'cross')), 3)
})

test_that('merge errors with unequal vector lengths', {
    lst <- list(a = 1:10, b = 11:20, c = 21:29)
    expect_error(lcombine(lst, 'merge'), 'Element 3 has length 9, not 1 or 10.')
})

test_that('errors with duplicate names', {
    lst <- list(a = 1:10, a = 11:20, c = 21:29)
    expect_error(lcombine(lst))
})

test_that('errors without names', {
    lst <- list(1:10, a = 11:20, c = 21:29)
    expect_error(lcombine(lst))
})
