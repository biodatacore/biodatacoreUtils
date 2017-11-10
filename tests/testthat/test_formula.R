context('formula')

# as.character.formula ----------------------------------------------------

test_that('as.character.formula works correctly', {

    expect_equivalent(as.character(a ~ b + c + .), 'a ~ b + c + .')

    # remove extra spaces
    expect_equivalent(as.character(a  ~  b   + c    +   .), 'a ~ b + c + .')

    # put in spaces?
    expect_equivalent(as.character(a~b+c+.), 'a ~ b + c + .')
})
