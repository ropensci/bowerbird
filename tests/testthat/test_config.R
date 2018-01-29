context("data config")

test_that("config save/load works", {
    src <- bb_example_sources()
    cf <- bb_add(bb_config("/dummy/file/root"),src)
    expect_s3_class(cf,"bb_config")
    tmpfile <- tempfile()
    saveRDS(cf,tmpfile)
    expect_identical(serialize(cf,NULL),serialize(readRDS(file=tmpfile),NULL))
})

test_that("config summary appears to work", {
    skip_if_not(rmarkdown::pandoc_available("1.12.3"),"skipping config summary test because pandoc is not available or is not a recent enough version")
    temp_root <- tempdir()
    cf <- bb_add(bb_config(temp_root),bb_example_sources())
    summary_filename <- bb_summary(cf)
    expect_true(file.exists(summary_filename))
})

test_that("local directory looks right",{
    src <- bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        doc_url="blah",
        citation="blah",
        source_url="http://some.place.com/some/path/",
        license="blah",
        method=list("bb_handler_wget"),
        data_group="blah")
    cf <- bb_config("/some/local/path") %>% bb_add(src)
    temp <- bb_data_source_dir(cf)
    temp <- gsub("\\+","/",temp) ## make sure are unix-style path seps
    expect_identical(sub("/$","",temp),"/some/local/path/some.place.com/some/path")
})

test_that("config validation works",{
    src <- bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        doc_url="blah",
        citation="blah",
        source_url="http://some.place.com/some/path/",
        license="blah",
        method=list("bb_handler_wget"),
        data_group="blah")
    src2 <- bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        doc_url="blah",
        citation="blah",
        source_url="http://some.place.com/some/path/",
        license="blah",
        authentication_note="this source requires login",
        method=list("bb_handler_wget"),
        data_group="blah",
        warn_empty_auth=FALSE)
    cf <- bb_config("/some/local/path") %>% bb_add(src) %>% bb_add(src2)
    expect_error(bowerbird:::bb_validate(cf),"requires authentication")

    src2$user <- "username"
    src2$password <- "password"
    cf <- bb_config("/some/local/path") %>% bb_add(src) %>% bb_add(src2)
    expect_true(bowerbird:::bb_validate(cf))
})

test_that("bb_settings works",{
    cf <- bb_config(local_file_root="/your/data/directory")
    sets <- bb_settings(cf)
    ## the names of the settings here should match the allowed names
    ## except that dry_run is allowed as a setting, but it is set in bb_sync not in bb_config, so it won't be set here
    ## set it now
    sets$dry_run <- FALSE
    expect_true(setequal(names(sets),bowerbird:::allowed_settings()))
    expect_null(sets$http_proxy)
    ## change a setting
    sets$http_proxy <- "something"
    bb_settings(cf) <- sets
    expect_identical(bb_settings(cf)$http_proxy,"something")
    ## try to add a non-recognized setting
    expect_warning(bb_settings(cf) <- list(bilbobaggins=1),"is not a recognized bowerbird config setting")
    expect_null(bb_settings(cf)$bilbobaggins)
    ## a side effect of that call is that it will have removed all other settings
    ## because the new settins list contained only the bilbobaggins element
    expect_true(length(bb_settings(cf))<1)
})
