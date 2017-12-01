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
    src <- bb_source2(
        id="bilbobaggins",
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="http://some.place.com/some/path/",
        license="blah",
        method=list("bb_handler_wget"),
        data_group="blah")
    cf <- bb_config2("/some/local/path") %>% bb_add(src)
    temp <- bb_data_source_dir(cf)
    temp <- gsub("\\+","/",temp) ## make sure are unix-style path seps
    expect_identical(sub("/$","",temp),"/some/local/path/some.place.com/some/path")
})

test_that("config validation works",{
    src <- bb_source2(
        id="bilbobaggins",
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="http://some.place.com/some/path/",
        license="blah",
        method=list("bb_handler_wget"),
        data_group="blah")
    src2 <- bb_source2(
        id="bilbobaggins",
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="http://some.place.com/some/path/",
        license="blah",
        authentication_note="this source requires login",
        method=list("bb_handler_wget"),
        data_group="blah",
        warn_empty_auth=FALSE)
    cf <- bb_config2("/some/local/path") %>% bb_add(src) %>% bb_add(src2)
    expect_error(bb_validate(cf))

    src2$user <- "username"
    src2$password <- "password"
    cf <- bb_config("/some/local/path") %>% bb_add(src) %>% bb_add(src2)
    expect_true(bb_validate(cf))
})
