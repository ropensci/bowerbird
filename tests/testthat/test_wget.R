context("wget stuff")

test_that("wget_install works and bb_find_wget finds something", {
    if (.Platform$OS.type!="windows") {
        expect_error(bb_install_wget(),"only supports windows platforms") ## only supported for Windows platforms
        expect_true(is.string(bb_find_wget())) ## should still be able to find system-installed wget
    } else {
        wge <- bb_install_wget()
        expect_true(file.exists(wge))
        expect_true(is.string(bb_find_wget()))
    }
})

test_that("wget help works", {
    blah <- capture_messages(bb_wget("--help"))
    ## should see the phrase "Mail bug reports and suggestions to <bug-wget@gnu.org>" in the output
    expect_true(grepl("bug reports and suggestions",blah,ignore.case=TRUE))
})

test_that("internal flags_to_charvec function behaves",{
    is_zerolen_char <- function(z) is.character(z) && length(z)<1
    expect_true(is_zerolen_char(bowerbird:::flags_to_charvec(c())))
    expect_true(is_zerolen_char(bowerbird:::flags_to_charvec("")))
    expect_true(is_zerolen_char(bowerbird:::flags_to_charvec(NULL)))
    expect_true(is_zerolen_char(bowerbird:::flags_to_charvec(NA_character_)))
    expect_error(bowerbird:::flags_to_charvec(NA),"expecting flags as character vector or list")
})

test_that("internal resolve_wget_clobber_flags function behaves",{
    expect_equal(bowerbird:::resolve_wget_clobber_flags(c("blah"),c("--no-clobber")),
                 c("blah","--no-clobber"))
    expect_equal(bowerbird:::resolve_wget_clobber_flags(c("blah","--timestamping"),c("--no-clobber")),
                 c("blah","--timestamping"))
    expect_equal(bowerbird:::resolve_wget_clobber_flags(c("blah","--no-clobber"),c("--timestamping")),
                 c("blah","--no-clobber"))

    ## skip these temporarily - failing under covr for unknown reasons
    ##expect_equal(bowerbird:::resolve_wget_clobber_flags(c("blah"),c("-nc")),c("blah","-nc"))
    ##expect_equal(bowerbird:::resolve_wget_clobber_flags(c("blah","-N"),c("-nc")),c("blah","-N"))
    ##expect_equal(bowerbird:::resolve_wget_clobber_flags(c("blah","-nc"),c("-N")),c("blah","-nc"))
})
