context("test bowerbird utilities")

test_that("check_method_is works",{
    expect_true(check_method_is("bb_handler_wget",bb_handler_wget)) ## function name as string
    expect_true(check_method_is(bb_handler_wget,bb_handler_wget)) ## function
    ##expect_true(check_method_is(enquote(bb_handler_wget),bb_handler_wget))
    ##expect_true(check_method_is(quote(bb_handler_wget()),bb_handler_wget))
    expect_true(check_method_is(quote(bb_handler_wget),bb_handler_wget)) ## symbol
})

test_that("internal directory_from_url function works",{
    this_url <- c("http://blah.blah","https://some.thing/blah","https://some.thing/blah/","ftp://yah.yah.yah/a.b/c/")
    expect_identical(directory_from_url(this_url),c("blah.blah/","some.thing/","some.thing/blah/","yah.yah.yah/a.b/c/"))
    expect_identical(directory_from_url(list(this_url)),c("blah.blah/","some.thing/","some.thing/blah/","yah.yah.yah/a.b/c/"))
})

