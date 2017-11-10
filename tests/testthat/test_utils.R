context('utils')


# reverse_split -----------------------------------------------------------


test_that('reverse_split works', {

    nums <- as.character(1:26)

    lst <- as.list(letters)
    names(lst) <- nums
    lst2 <- as.list(nums)
    names(lst2) <- letters

    # reverse split conversts numbers to characters
    expect_equal(reverse_split(lst), lst2)

    # reverse split does not return names in natural order
    expect_equal(reverse_split(reverse_split(lst)), lst[sort(names(lst))])

    lst3 <- list(a = 1:4, b = c(2,3), d = c(4,5))
    lst4 <- list('1' = 'a', '2' = c('a', 'b'), '3' = c('a', 'b'), '4' = c('a', 'd'), '5' = 'd')

    expect_equal(reverse_split(lst3), lst4)
})


# dat_to_mat --------------------------------------------------------------

mat <- matrix(rnorm(260), nrow = 26)
dat <- dplyr::as_data_frame(mat)

colnames(mat) <- LETTERS[1:10]
rownames(mat) <- letters
dat$name <- letters

dtm <- dat_to_mat(dat, 'name')


test_that('dat_to_mat returns a matrix', {
    expect_true(is.matrix(dtm))
})

test_that('dat_to_mat preserves column names', {
    expect_equal(colnames(dtm), names(dat[names(dat) != 'name']))
})

test_that('dat_to_mat gives proper rownames', {
    expect_equal(rownames(dtm), dat$name)
})

test_that('dat_to_mat preserves data', {
    expect_equivalent(dtm, mat)
})

test_that('dat_to_mat works with all tidyeval forms', {
    expect_equal(dat_to_mat(dat, name), dat_to_mat(dat, 'name'))
    expect_equal(dat_to_mat(dat, name), dat_to_mat(dat, 11))
})


# %nin% -------------------------------------------------------------------

test_that('%nin% works correctly', {
    expect_true(1 %nin% letters)
    expect_true('a' %nin% LETTERS)
    expect_true('A' %nin% letters)

    expect_false(1 %nin% 1:10)
    expect_false('a' %nin% letters)
    expect_false('A' %nin% LETTERS)

    expect_equivalent(c(1, 2) %nin% 1:3, c(FALSE, FALSE))
    expect_equivalent(c(4:5) %nin% 1:3, c(TRUE, TRUE))
})

