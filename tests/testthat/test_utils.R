context("test bowerbird utilities")

test_that("check_method_is works",{
    expect_true(check_method_is("aadc_eds_get",aadc_eds_get)) ## function name as string
    expect_true(check_method_is(aadc_eds_get,aadc_eds_get)) ## function
    expect_true(check_method_is(enquote(aadc_eds_get),aadc_eds_get))
    expect_true(check_method_is(quote(aadc_eds_get()),aadc_eds_get))
    expect_true(check_method_is(quote(aadc_eds_get),aadc_eds_get)) ## symbol
})

test_that("method dispatch code works",{
    mycat <- function(data_source) cat(data_source)
    ## as function
    myarg <- "oui oui"
    mth <- mycat
    expect_true(is.function(mth))
    expect_output(do.call(mth,list(data_source=myarg)),myarg)
    expect_output(do.call(mth,list(myarg)),myarg)
    expect_error(do.call(mth,list(blah=myarg)))
    ## as symbol
    mth <- quote(mycat)
    expect_true(is.name(mth))
    expect_output(do.call(eval(mth),list(data_source=myarg)),myarg)
    expect_output(do.call(eval(mth),list(myarg)),myarg)

    ## call constructed by e.g. enquote(fun)
    mth <- enquote(mycat)
    expect_true(is.call(mth))
    expect_identical(all.names(mth)[1],"quote")
    expect_output(do.call(eval(mth),list(data_source=myarg)),myarg)
    expect_output(do.call(eval(mth),list(myarg)),myarg)

    ## call constructed as e.g. quote(fun())
    mth <- quote(mycat())
    expect_true(is.call(mth))
    expect_false(all.names(mth)[1]=="quote")
    expect_identical(all.names(mth)[1],"mycat")
    expect_output(do.call(all.names(mth)[1],list(data_source=myarg)),myarg)
    expect_output(do.call(all.names(mth)[1],list(myarg)),myarg)
})

    
