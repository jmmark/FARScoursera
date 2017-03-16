# tests for the FARScoursera package
# this file tests only make_filename

test_that("make_filename",{
    # check to make sure it returns a character for a valid filename, both
    # int and character input
    expect_is(make_filename('2013'),'character')
    expect_is(make_filename(2013),'character')
    # check to make sure the proper string is returned
    expect_match(make_filename('2013'),
                'accident_2013.csv.bz2')
    # check to make sure non-coercible-to-int throws coercian warning
    expect_warning(make_filename('abcd'))
})
