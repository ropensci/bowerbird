context("test bowerbird utilities")

test_that("check_method_is works",{
    expect_true(check_method_is("bb_handler_wget",bb_handler_wget)) ## function name as string
    expect_true(check_method_is(bb_handler_wget,bb_handler_wget)) ## function
    ##expect_true(check_method_is(enquote(bb_handler_wget),bb_handler_wget))
    ##expect_true(check_method_is(quote(bb_handler_wget()),bb_handler_wget))
    expect_true(check_method_is(quote(bb_handler_wget),bb_handler_wget)) ## symbol
})

test_that("internal directory_from_url function works",{
    this_url <- c("http://blah.blah", "https://some.thing/blah", "https://some.thing/blah/", "ftp://yah.yah.yah/a.b/c/", "ftp://yah.yah.yah/a.b/c/d")
    expect_identical(directory_from_url(this_url), c("blah.blah/", "some.thing/", "some.thing/blah/", "yah.yah.yah/a.b/c/", "yah.yah.yah/a.b/c/"))
    expect_identical(directory_from_url(list(this_url)), c("blah.blah/", "some.thing/", "some.thing/blah/", "yah.yah.yah/a.b/c/", "yah.yah.yah/a.b/c/"))
    expect_identical(directory_from_url(this_url, no_host = TRUE), c("", "", "blah/", "a.b/c/", "a.b/c/"))
    expect_identical(directory_from_url(this_url, cut_dirs = 1), c("blah.blah/", "some.thing/", "some.thing/", "yah.yah.yah/c/", "yah.yah.yah/c/"))
    expect_identical(directory_from_url(this_url, no_host = TRUE, cut_dirs = 1), c("", "", "", "c/", "c/"))
    expect_identical(directory_from_url(this_url, no_host = TRUE, cut_dirs = 10), c("", "", "", "", ""))
    expect_identical(directory_from_url(this_url, no_host = FALSE, cut_dirs = 10), c("blah.blah/", "some.thing/", "some.thing/", "yah.yah.yah/", "yah.yah.yah/"))
})

test_that("internal list_files function works", {
    path <- system.file(package = "bowerbird")
    testargs <- list(path = path, recursive = TRUE)
    expect_setequal(do.call(list.files, testargs), do.call(list_files, testargs))

    path <- system.file(package = "bowerbird")
    testargs <- list(path = path, recursive = FALSE)
    expect_setequal(do.call(list.files, testargs), do.call(list_files, testargs))

    testargs <- list(path = path, recursive = TRUE, full.names = TRUE)
    expect_setequal(do.call(list.files, testargs), do.call(list_files, testargs))

    testargs <- list(path = path, recursive = TRUE, include.dirs = TRUE)
    expect_setequal(do.call(list.files, testargs), do.call(list_files, testargs))

    skip_on_os("windows")
    ## skip everything from here onwards if on windows
    path <- "/dev"
    testargs <- list(path = path, recursive = FALSE)
    expect_setequal(do.call(list.files, testargs), do.call(list_files, testargs))
    testargs <- list(path = path, recursive = FALSE, include.dirs = TRUE)
    expect_setequal(do.call(list.files, testargs), do.call(list_files, testargs))
})
