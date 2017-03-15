# tests for the FARScoursera package
# this file tests only fars_read

test_that("fars_read",{
    # a bad filename should cause an error
    expect_that(fars_read('nothere'),
                throws_error())
    # a valid filename should return a tibble
    expect_that(
        fars_read(
            system.file('extdata','accident_2013.csv.bz2',
                        package = 'FARScoursera')),
            is_a('tbl_df'))


})
