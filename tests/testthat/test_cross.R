context('cross')


# cross_some --------------------------------------------------------------

test_that('gives correct length', {
    lst <- list(a = 1:3, b = 4:6, d = 7:9)
    expect_equal(length(cross_some(lst, 'a')), 3)
    expect_equal(length(cross_some(lst, c('a', 'b'))), 9)
})

test_that('preserves names' {
    lst <- list(a = 1:3, b = 4:6,  d = 7:9)
    crossed <- cross_some(lst, c('a', 'b'))

    all_have_all_names <-
        purrr::map_lgl(crossed, ~all(rlang::has_name(., c('a', 'b', 'd'))))

    expect_true(all(all_have_all_names))
})

test_that('indexes with names and indices' {
    lst <- list(a = 1:3, b = 4:6,  d = 7:9)
    crossed <- cross_some(lst, c('a', 'd'))

    expect_equivalent(cross_some(lst, c('a', 'd')), cross_some(lst, c(1, 3)))
})
