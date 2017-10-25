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

