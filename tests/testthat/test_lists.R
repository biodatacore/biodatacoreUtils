context('lists')


# lst_combine ----------------------------------------------------------------


test_that('returns a list of depth 3', {
    lst <- list(a = 1:10, b = 11:20, c = 21:30)

    expect_equal(purrr::vec_depth(lst_combine(lst, 'stack')), 3)
    expect_equal(purrr::vec_depth(lst_combine(lst, 'merge')), 3)
    expect_equal(purrr::vec_depth(lst_combine(lst, 'cross')), 3)
})

test_that('returns expected lengths', {
    lst <- list(a = 1:2, b = 3:4, c = 5:6)

    # 2 + 2 + 2
    expect_equal(length(lst_combine(lst, 'stack')), 6)

    # 2
    expect_equal(length(lst_combine(lst, 'merge')), 2)

    # 2 * 2 * 2
    expect_equal(length(lst_combine(lst, 'cross')), 8)
})

test_that('merge errors with unequal vector lengths', {
    lst <- list(a = 1:10, b = 11:20, c = 21:29)
    expect_error(lst_combine(lst, 'merge'), 'Element 3 has length 9, not 1 or 10.')
})

test_that('named and unnamed argument patterns are preserved', {
    lst <- list(a = 1:2, 3:4)

    stack_nms <- purrr::map_chr(lst_combine(lst, method = 'stack'), names)
    merge_nms <- purrr::map(lst_combine(lst, method = 'merge'), names) %>%
        unlist()
    cross_nms <- purrr::map(lst_combine(lst, method = 'cross'), names) %>%
        unlist()

    expect_equal(stack_nms, c('a', 'a', '', ''))
    expect_equal(merge_nms, c('a', '', 'a', ''))
    expect_equal(cross_nms, c('a', '', 'a', '', 'a', '', 'a', ''))
})

