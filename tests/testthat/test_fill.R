context("fill")


# formulas ----------------------------------------------------------------

test_that("fill_at can refer to fill in 3 ways", {
    expect_equal(fill_at(c(1:3), 1, 10), c(10, 2, 3))
    expect_equal(fill_at(c(1:3), 1, ~10), c(10, 2, 3))
    expect_equal(fill_at(c(1:3), 1, function(.x) 10), c(10, 2, 3))
})

test_that("fill_if can refer to first argument in three ways", {
    expect_equal(fill_if(c(1:3), ~. == 1, 10), c(10, 2, 3))
    expect_equal(fill_if(c(1:3), ~.x == 1, 10), c(10, 2, 3))
    expect_equal(fill_if(c(1:3), ~..1 == 1, 10), c(10, 2, 3))
})


# as_filler ---------------------------------------------------------------


test_that("Additional arguments are ignored", {
    expect_equal(as_filler(function() NULL, foo = "bar", foobar), function() NULL)
})

# test_that("primitive functions are wrapped", {
#     expect_identical(as_mapper(`-`)(.y = 10, .x = 5), -5)
#     expect_identical(as_mapper(`c`)(1, 3, 5), c(1, 3, 5))
# })
#
# test_that("syntactic primitives are wrapped", {
#     expect_identical(as_mapper(`[[`)(mtcars, "cyl"), mtcars$cyl)
#     expect_identical(as_mapper(`$`)(mtcars, cyl), mtcars$cyl)
# })
#
# test_that("raw and complex types aren't supported for indexing", {
#     expect_error(as_mapper(1)(raw(2)))
#     expect_error(as_mapper(1)(complex(2)))
# })
