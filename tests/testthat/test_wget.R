context("wget stuff")

test_that("wget_install works and wget_exe() finds something", {
    if (.Platform$OS.type!="windows") {
        expect_error(install_wget())
        expect_true(is.string(wget_exe()))
    } else {
        install_wget()
        expect_true(is.string(wget_exe()))
    }
})

test_that("wget help works", {
    blah <- wget("--help",stdout=TRUE)
    expect_gt(length(blah),10)
    blah <- paste(blah,collapse="\n")
    ## should see the phrase "Mail bug reports and suggestions to <bug-wget@gnu.org>" in the output
    expect_true(grepl("bug reports and suggestions",blah,ignore.case=TRUE))
})    
