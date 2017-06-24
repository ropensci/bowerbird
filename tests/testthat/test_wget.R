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

