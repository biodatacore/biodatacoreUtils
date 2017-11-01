context('Predicates')

# is_wholenumber ----------------------------------------------------------

test_that('na_rm returns true if otherwise whole and NA otherwise', {
    expect_true(is_whole_number(c(1, NA), na_rm = TRUE))
    expect_NA(is_whole_number(c(1, NA), na_rm = FALSE))
})

test_that('returns NA for numeric empty and all NA vectors', {
    expect_NA(is_whole_number(rlang::dbl(), na_rm = FALSE))
    expect_NA(is_whole_number(rlang::dbl(NA), na_rm = FALSE))
    expect_NA(is_whole_number(rlang::dbl(NA), na_rm = TRUE))
})

test_that('returns FALSE for non-numeric and fraction', {
    expect_false(is_whole_number(factor(), na_rm = FALSE))
    expect_false(is_whole_number(rlang::lgl(), na_rm = FALSE))
    expect_false(is_whole_number(rlang::chr(), na_rm = FALSE))
    expect_false(is_whole_number(rlang::dbl(1.1), na_rm = FALSE))
})


# numeric_like ---------------------------------------------------------

test_that('true if all convertible', {
    expect_true(is_numeric_like(c(1, NA)))
    expect_true(is_numeric_like(c('1', NA)))
    expect_false(is_numeric_like(c('?', '1')))
})

test_that('true if all convertible factors', {
    expect_true(is_numeric_like(as.factor(c(1, NA))))
    expect_true(is_numeric_like(as.factor(c('1', NA))))

    # not false because factors stored as integers
    expect_false(is_numeric_like(as.factor(c('?', '1'))))
})

test_that('returns TRUE for empty and all NA vectors', {
    expect_true(is_numeric_like(factor()))
    expect_true(is_numeric_like(rlang::dbl()))
    expect_true(is_numeric_like(rlang::lgl()))
    expect_true(is_numeric_like(rlang::int()))
    expect_true(is_numeric_like(rlang::chr()))
    expect_true(is_numeric_like(NA))
})

# is_binary_valued ---------------------------------------------------

test_that('NAs count as a countable value', {
    expect_true(is_binary_valued(c(5, NA)))
    expect_false(is_binary_valued(c(5, 6, NA)))
    expect_true(is_binary_valued(c('a', 'b', NA), na_rm = TRUE))
})

test_that('factors work', {
    expect_true(is_binary_valued(as.factor(c(5, NA))))
    expect_false(is_binary_valued(as.factor(c(5, 6, NA))))
    expect_true(is_binary_valued(as.factor(c('a', 'b', NA)), na_rm = TRUE))
})

test_that('returns false on empty vectors', {
    expect_false(is_binary_valued(factor()))
    expect_false(is_binary_valued(rlang::dbl()))
    expect_false(is_binary_valued(rlang::lgl()))
    expect_false(is_binary_valued(rlang::chr()))
    expect_false(is_binary_valued(rlang::int()))
})


# positive -----------------------------------------------------

test_that('NAs return NA if otherwise positive', {
    expect_true(is_positive(c(5, NA), na_rm = TRUE))
    expect_equivalent(is_positive(c(5, NA)), NA)
})

test_that('NAs return FALSE if otherwise negative', {
    expect_false(is_positive(c(-5, NA), na_rm = TRUE))
    expect_false(is_positive(c(-5, NA), na_rm = FALSE))
})

test_that('returns NA for numeric empty and all NA vectors', {
    expect_NA(is_positive(rlang::dbl(), na_rm = FALSE))
    expect_NA(is_positive(rlang::dbl(NA), na_rm = FALSE))
    expect_NA(is_positive(rlang::dbl(NA), na_rm = TRUE))
})


test_that('returns false on empty non numeric vectors', {
    expect_false(is_positive(rlang::lgl()))
    expect_false(is_positive(rlang::chr()))
    expect_false(is_positive(factor()))
})


# is_non_negative_numeric -----------------------------------------------------

test_that('NAs return NA if otherwise 0-positive', {
    expect_true(is_non_negative(c(0, NA), na_rm = TRUE))
    expect_NA(is_non_negative(c(0, NA)))
})

test_that('NAs return FALSE if otherwise negative', {
    expect_false(is_non_negative(c(-5, NA), na_rm = TRUE))
    expect_false(is_non_negative(c(-5, NA), na_rm = FALSE))
})

test_that('returns NA for numeric empty and all NA vectors', {
    expect_NA(is_non_negative(rlang::dbl(), na_rm = FALSE))
    expect_NA(is_non_negative(rlang::dbl(NA), na_rm = FALSE))
    expect_NA(is_non_negative(rlang::dbl(NA), na_rm = TRUE))
})

test_that('returns false on empty non numeric vectors', {
    expect_false(is_non_negative(rlang::lgl()))
    expect_false(is_non_negative(rlang::chr()))
    expect_false(is_non_negative(factor()))
})

# zero --------------------------------------------------------------------

test_that('NAs return NA if otherwise 0-positive', {
    expect_true(is_zero(c(0, NA), na_rm = TRUE))
    expect_NA(is_zero(c(0, NA)))
})

test_that('NAs return FALSE if otherwise negative', {
    expect_false(is_zero(c(-5, NA), na_rm = TRUE))
    expect_false(is_zero(c(-5, NA), na_rm = FALSE))
})

test_that('returns NA for numeric empty and all NA vectors', {
    expect_NA(is_zero(rlang::dbl(), na_rm = FALSE))
    expect_NA(is_zero(rlang::dbl(NA), na_rm = FALSE))
    expect_NA(is_zero(rlang::dbl(NA), na_rm = TRUE))
})

test_that('returns false on empty non numeric vectors', {
    expect_false(is_zero(rlang::lgl()))
    expect_false(is_zero(rlang::chr()))
    expect_false(is_zero(factor()))
})
