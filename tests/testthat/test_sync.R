context("sync")

test_that("bb_sync works with dry run",{
    skip_on_cran()
    temp_root <- tempdir()
    cf <- bb_config(local_file_root=temp_root)
    cf <- cf %>% bb_add(bb_example_sources()[1,])
    bb_sync(cf,catch_errors=FALSE,confirm_downloads_larger_than=NULL,dry_run=TRUE)
})

test_that("bb_sync fails when given an empty config",{
    cf <- bb_config(local_file_root="/does/not/matter")
    expect_warning(bb_sync(cf,catch_errors=FALSE),"config has no data sources")
})

test_that("bb_sync behaves correctly when root does not exist",{
    td <- tempfile(pattern="dir") ## use this as a dir name
    ## it should not yet exist
    if (!dir.exists(td) && !file.exists(td)) {
        cf <- bb_config(local_file_root=td)
        cf <- cf %>% bb_add(bb_example_sources()[1,])
        expect_error(bb_sync(cf,catch_errors=FALSE,confirm_downloads_larger_than=NULL,dry_run=TRUE),"does not exist")
        ## ok if we tell bb_sync to create it
        blah <- bb_sync(cf,catch_errors=FALSE,confirm_downloads_larger_than=NULL,dry_run=TRUE,create_root=TRUE)
    }
})

test_that("bb_sync is quiet when asked",{
    skip_on_cran()
    temp_root <- tempdir()
    cf <- bb_config(local_file_root=temp_root)
    myds <- bb_source(
        id="bilbobaggins",
        name="test",
        description="blah",
        doc_url= "http://some.where.org/",
        citation="blah",
        license="blah",
        method=list("bb_handler_wget",recursive=TRUE,no_check_certificate=TRUE,level=1),
        source_url="https://github.com/ropensci/bowerbird/blob/master/README.Rmd") ## just some file to download
    cf <- cf %>% bb_add(myds)
    expect_silent(bb_sync(cf,verbose=FALSE,confirm_downloads_larger_than=NULL))

    ## and same with rget
    cf <- bb_config(local_file_root=temp_root)
    myds <- bb_source(
        id="bilbobaggins",
        name="test",
        description="blah",
        doc_url= "http://some.where.org/",
        citation="blah",
        license="blah",
        method=list("bb_handler_rget", level = 1),
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data.zip")
    cf <- cf %>% bb_add(myds)
    expect_silent(bb_sync(cf,verbose=FALSE,confirm_downloads_larger_than=NULL))
})

test_that("bb_sync errors on a source that is missing required authentication info",{
    skip_on_cran()
    mysrc <- bb_example_sources("Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 3")
    cf <- bb_config(local_file_root=tempdir()) %>% bb_add(mysrc)
    expect_error(bb_sync(cf,confirm_downloads_larger_than=NULL),"requires authentication") ## error at the bb_validate stage, because user and password have not been set

    ## would also get error at the handler stage, for the same reason
    expect_error(do.call(bb_handler_earthdata,c(list(cf),bb_data_sources(cf)$method[[1]][-1])),"requires user and password")
})

test_that("bb_sync works with a sink() call in place",{
    skip_on_cran()
    sinkfile <- tempfile()
    sink(file=sinkfile)
    myds <- bb_source(
        id="bilbobaggins",
        name="test",
        description="blah",
        doc_url= "http://some.where.org/",
        citation="blah",
        license="blah",
        method=list("bb_handler_wget",recursive=TRUE,level=1),
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data.zip") ## just some file to download
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),myds)
    bb_sync(cf,verbose=TRUE,confirm_downloads_larger_than=NULL)
    sink()
    op <- readLines(sinkfile)
    ## sink file should contain direct cat output "Synchronizing dataset: test"
    expect_true(any(grepl("Synchronizing dataset: test",op)))
})
