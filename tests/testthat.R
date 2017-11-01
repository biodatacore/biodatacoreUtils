library(testthat)
library(biodatacoreUtils)


expect_NA <- function(x, ...) {
    testthat::expect_equivalent(x, expected = NA, ...)
}

test_check("biodatacoreUtils")
