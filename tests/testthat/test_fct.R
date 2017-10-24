context('Factor Ordering')


fct <- c(.1, 1.1, '1.1.1', 1:10, letters, 1:10, letters)

test_that('fct_ord_alphabet orders alphabetically', {
    expect_equal(
        levels(fct_ord_alphabet(fct)),
        c(0.1, 1, 1.1, '1.1.1', 10, 2:9, letters)
    )
})

test_that('fct_ord_natural orders naturally', {
    expect_equal(
        levels(fct_ord_natural(fct)),
        c(0.1, 1, 1.1, '1.1.1', 2:10, letters)
    )
})


# fct_ord_by --------------------------------------------------------------
fct <- rep(1:3, length.out = 100)
values <- 1:100

test_that('fct_ord_by orders correctly', {
    expect_equal(
        levels(fct_ord_by(fct, values)),
        as.character(c(2, 1, 3))
    )
    expect_equal(
        levels(fct_ord_by(fct, values, .agg = base::max)),
        as.character(c(2, 3, 1))
    )
})


# fat_ord_clust -----------------------------------------------------------


set.seed(12345)
fct <- rep(letters[1:10], length.out = 100)
mat <- matrix(rnorm(1000), nrow = 10)
colnames(mat) <- paste('sample', 1:100, sep = '_')

test_that('fct_ord_clust orders correctly', {
    rownames(mat) <- letters[1:10]

    fct_clust <- fct_ord_clust(fct, mat, return_clust = TRUE)
    expect_equal(
        levels(fct_clust$fct),
        fct_clust$clust$labels[fct_clust$clust$order]
    )
})

test_that('fct_ord_clust errors if there is a mismatch between factor values and suggested levels', {
    rownames(mat) <- letters[11:20]
    expect_error(
        fct_ord_clust(fct, mat, return_clust = TRUE),
        'setequal\\(lvls, unique\\(x\\)\\) is not TRUE'
    )

})

