context('substitute')
# cross_sub ---------------------------------------------------------------

test_that('returns subs of appropriate length', {
    exprs <- list(fo1 = a ~ b + c, fo2 = b ~ a + c)
    envs <- lst_combine(list(a = 1:5, b = 6:10), method = 'stack')
    out <- cross_sub(exprs, envs)

    expect_equal(length(out), 2)
    expect_equal(length(out[[1]]), 10)
    expect_equal(length(out[[2]]), 10)
})

test_that('returns subs of appropriate length, even if there are no subs', {
    exprs <- list(fo1 = a ~ b + c, fo2 = b ~ a + c)
    envs <- lst_combine(list(d = 1:5), method = 'stack')
    out <- cross_sub(exprs, envs)

    expect_equal(length(out), 2)
    expect_equal(length(out[[1]]), 5)
    expect_equal(length(out[[2]]), 5)
})


test_that('expr as.name passes strings as names in fos', {
    exprs <- list(fo1 = a ~ b + c)
    envs <- lst_combine(list(b = c('yes')), method = 'stack')
    out_char <- cross_sub(exprs, envs)
    out_name <- cross_sub(exprs, envs, trans = as.name)


    expect_true(grepl('\\\"', deparse2(out_char[[1]][[1]])))
    expect_false(grepl('\\\"', deparse2(out_name[[1]][[1]])))

})


# f_cross_sub -------------------------------------------------------------

test_that('errors if formula isnt passed', {

    envs <- lst_combine(list(b = c('yes')), method = 'stack')

    qte <- list(quote(hello))
    cll <- list(as.call(list('round', quote(10.5))))
    nme <- list(as.name('a'))

    expect_error(f_cross_sub(qte, envs))
    expect_error(f_cross_sub(cll, envs))
    expect_error(f_cross_sub(nme, envs))
})

test_that('inherits fomrula environments', {

    fo <- a ~ b + c
    rlang::f_env(fo) <- rlang::new_environment(data = mtcars)

    exprs <- list(fo)
    envs <- lst_combine(list(b = c('yes')), method = 'stack')
    out <- f_cross_sub(exprs, envs, trans = as.name)

    expect_identical(rlang::f_env(fo), rlang::f_env(out[[1]][[1]]))
})
