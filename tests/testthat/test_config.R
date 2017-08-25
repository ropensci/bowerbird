context("data config")

test_that("config save/load works", {
    src <- bb_example_sources()
    cf <- add(bb_config("/dummy/file/root"),src)
    expect_s3_class(cf,"data.frame")
    tmpfile <- tempfile()
    saveRDS(cf,tmpfile)
    expect_identical(serialize(cf,NULL),serialize(readRDS(file=tmpfile),NULL))
})

test_that("config operations preserve attributes", {
    ods <- bb_source(
        id="bilbobaggins",
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See http://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        comment="",
        method=oceandata_get,
        method_flags=c("search=T20000322000060.L3m_MO_SST_sst_9km.nc"),
        postprocess=NULL,
        access_function="",
        data_group="Sea surface temperature")
    temp_root <- tempdir()
    ocf <- add(bb_config(local_file_root="irrelevant_here"),ods)
    ## check we have the expected bb attributes and that bb_attributes finds them
    bbae <- c("wget_global_flags","wget_default_flags","local_file_root","clobber","skip_downloads")
    expect_true(setequal(names(bb_attributes(ocf)),bbae))
    expect_identical(bb_attributes(ocf %>% slice(1)),bb_attributes(ocf))
    expect_identical(bb_attributes(ocf[1,]),bb_attributes(ocf))

    temp <- bb_attributes_to_cols(ocf[1,])
    expect_equal(ncol(temp),length(names(ocf))+length(bbae))
    expect_named(temp,c(names(ocf),bbae),ignore.order=TRUE)
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
    temp <- data_source_dir(cf[1,])
    temp <- gsub("\\+","/",temp) ## make sure are unix-style path seps
    expect_identical(sub("/$","",temp),"/some/local/path/some.place.com/some/path")
})

