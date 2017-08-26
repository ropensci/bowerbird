context("wget stuff")

test_that("wget_install works and wget_exe() finds something", {
    if (.Platform$OS.type!="windows") {
        expect_error(install_wget())
        expect_true(is.string(wget_exe()))
    } else {
        wge <- install_wget()
        expect_true(file.exists(wge))
        expect_true(is.string(wget_exe()))
    }
})

test_that("wget help works", {
    blah <- wget("--help")
    blah <- rawToChar(blah$stdout)
    ## should see the phrase "Mail bug reports and suggestions to <bug-wget@gnu.org>" in the output
    expect_true(grepl("bug reports and suggestions",blah,ignore.case=TRUE))
})

test_that("internal flags_to_charvec function behaves",{
    is_zerolen_char <- function(z) is.character(z) && length(z)<1
    expect_true(is_zerolen_char(c()))
    expect_true(is_zerolen_char(""))
    expect_true(is_zerolen_char(NULL))
    expect_true(is_zerolen_char(NA_character_))
    expect_error(flags_to_charvec(NA))
})

test_that("internal resolve_wget_clobber_flags function behaves",{
    expect_equal(resolve_wget_clobber_flags(c("blah"),c("--no-clobber")),
                 c("blah","--no-clobber"))
    expect_equal(resolve_wget_clobber_flags(c("blah","--timestamping"),c("--no-clobber")),
                 c("blah","--timestamping"))
    expect_equal(resolve_wget_clobber_flags(c("blah","--no-clobber"),c("--timestamping")),
                 c("blah","--no-clobber"))

    expect_equal(resolve_wget_clobber_flags(c("blah"),c("-nc")),
                 c("blah","-nc"))
    expect_equal(resolve_wget_clobber_flags(c("blah","-N"),c("-nc")),
                 c("blah","-N"))
    expect_equal(resolve_wget_clobber_flags(c("blah","-nc"),c("-N")),
                 c("blah","-nc"))
})
