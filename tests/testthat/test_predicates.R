context('Predicates')


# is_scalar_numeric -------------------------------------------------------

test_that('is_scalar_numeric works with Ints', {
    expect_true(is_scalar_numeric(1L))
    expect_true(is_scalar_numeric(-1L))
    expect_true(is_scalar_numeric(0L))
})

test_that('is_scalar_numeric works with doubles', {
    expect_true(is_scalar_numeric(5))
    expect_true(is_scalar_numeric(1/5))
    expect_true(is_scalar_numeric(-1.123))
    expect_true(is_scalar_numeric(0.10123))
})

test_that('is_scalar_numeric works with sci notation', {
    expect_true(is_scalar_numeric(10e1))
})

test_that('is_scalar_numeric fails with vector Ints', {
    expect_false(is_scalar_numeric(c(1L, 2L)))
})

test_that('is_scalar_numeric fails with vector doubles', {
    expect_false(is_scalar_numeric(c(5, 0, -1.23)))
})

test_that('is_scalar_numeric fails with character', {
    expect_false(is_scalar_numeric('1'))
    expect_false(is_scalar_numeric(c('1', '2')))
})



# is_wholenumber ----------------------------------------------------------


test_that('is_wholenumber works with Ints', {
    expect_true(is_wholenumber(1L))
    expect_true(is_wholenumber(-1L))
    expect_true(is_wholenumber(0L))
    expect_true(is_wholenumber(c(-1L, 0L, 1L)))
})

test_that('is_wholenumber works with doubles', {
    expect_true(is_wholenumber(1))
    expect_true(is_wholenumber(-1))
    expect_true(is_wholenumber(0))
    expect_true(is_wholenumber(c(-1, 0, 1)))
})

test_that('is_wholenumber works with sci notation', {
    expect_true(is_wholenumber(10e1))
})

test_that('is_wholenumber fails with character', {
    expect_false(is_wholenumber('1'))
    expect_false(is_wholenumber(c('1', '2')))
})


# is_numeric_like ---------------------------------------------------------

context('is_numeric_like')

test_that('is_numeric_like works with ints', {
    expect_true(is_numeric_like(5L))
    expect_true(is_numeric_like(c(5L, 4L)))
})

test_that('is_numeric_like works with double', {
    expect_true(is_numeric_like(5.5))
    expect_true(is_numeric_like(c(5.1, -12.53)))
})

test_that('is_numeric_like works with character double', {
    expect_true(is_numeric_like('5.5'))
    expect_true(is_numeric_like(c('5.1', '-12.53')))
})

# # Maybe if i come up witha  custum numeric converter this can work
# test_that('is_numeric_like works with character ints', {
#     expect_true(is_numeric_like('5L'))
#     expect_true(is_numeric_like(c('5L', '-12L')))
# })

test_that('is_numeric_like works with character sci-notation', {
    expect_true(is_numeric_like('5e10'))
    expect_true(is_numeric_like(c('5e11', '-12.53e13')))
})

test_that('is_numeric_like works with non-numeric character', {
    expect_false(is_numeric_like('?'))
    expect_false(is_numeric_like(c('>', 'hello')))
})

test_that('is_numeric_like works with NA', {
    expect_false(is_numeric_like(NA))
})



# is_binary_valued ---------------------------------------------------


test_that('is_binary_valued works with ints', {
    expect_false(
        is_binary_valued(5L)
    )
    expect_true(
        is_binary_valued(c(1L, -2L))
    )
    expect_false(
        is_binary_valued(NA_integer_)
    )
    expect_true(
        is_binary_valued(c(1L, -2L, NA_integer_))
    )
    expect_false(
        is_binary_valued(c(1L, -2L, NA_integer_), na.rm = FALSE)
    )
})

test_that('is_binary_valued works with floats', {
    expect_false(
        is_binary_valued(5.5)
    )
    expect_true(is_binary_valued(
        c(1.0, -2.0))
    )
    expect_false(
        is_binary_valued(NA_real_)
    )
    expect_true(
        is_binary_valued(c(1.1, -2.3, NA_real_))
    )
    expect_false(
        is_binary_valued(c(1.5, -2.123, NA_real_), na.rm = FALSE)
    )
})

test_that('is_binary_valued works with strings', {
    expect_false(
        is_binary_valued('5.5')
    )
    expect_true(
        is_binary_valued(c('hello', '?'))
    )
    expect_false(
        is_binary_valued(NA_character_)
    )
    expect_true(
        is_binary_valued(c('yo', 'dog', NA_character_))
    )
    expect_false(
        is_binary_valued(c('3', 'hi', NA_character_), na.rm = FALSE)
    )
})

test_that('is_binary_valued works with factors', {
    expect_false(
        is_binary_valued(factor('?'))
    )
    expect_true(
        is_binary_valued(factor(c('hello', '?')))
    )
    expect_false(
        is_binary_valued(factor(NA_character_))
    )
    expect_true(
        is_binary_valued(factor(c('hello', '?', NA_character_)))
    )
    expect_false(
        is_binary_valued(factor(c('hi', '?', NA_character_)), na.rm = FALSE)
    )
})

