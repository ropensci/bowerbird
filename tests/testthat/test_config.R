context("data config")

test_that("config save/load works", {
    src <- bb_example_sources()
    cf <- add(bb_config("/dummy/file/root"),src)
    expect_s3_class(cf,"bb_config")
    tmpfile <- tempfile()
    saveRDS(cf,tmpfile)
    expect_identical(serialize(cf,NULL),serialize(readRDS(file=tmpfile),NULL))
})

test_that("config summary appears to work", {
    skip_if_not(rmarkdown::pandoc_available("1.12.3"),"skipping config summary test because pandoc is not available or is not a recent enough version")
    temp_root <- tempdir()
    cf <- add(bb_config(temp_root),bb_example_sources())
    summary_filename <- bb_summary(cf)
    expect_true(file.exists(summary_filename))
})

test_that("local directory looks right",{
    src <- bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="http://some.place.com/some/path/",
        license="blah",
        method=bb_wget,
        method_flags=character(),
        data_group="blah")
    cf <- bb_config("/some/local/path") %>% add(src)
    temp <- data_source_dir(cf)
    temp <- gsub("\\+","/",temp) ## make sure are unix-style path seps
    expect_identical(sub("/$","",temp),"/some/local/path/some.place.com/some/path")
})

