context("data config")

test_that("config save/load works", {
    cf <- add(bb_config("/dummy/file/root"),bb_sources())
    expect_type(cf,"list")
    tmpfile <- tempfile()
    saveRDS(cf,tmpfile)
    expect_identical(cf,readRDS(file=tmpfile))
})

test_that("config operations preserve attributes", {
    ods <- bb_source(
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See http://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        comment="",
        method=oceandata_get,
        method_flags="search=T20000322000060.L3m_MO_SST_sst_9km.nc",
        postprocess=NULL,
        access_function="",
        data_group="Sea surface temperature")
    temp_root <- tempdir()
    ocf <- add(bb_config(local_file_root="irrelevant_here"),ods)
    ## check we have the expected bb attributes and that bb_attributes finds them
    bbae <- c("wget_global_flags","local_file_root","clobber","skip_downloads")
    expect_true(setequal(names(bb_attributes(ocf)),bbae))
    expect_identical(bb_attributes(ocf %>% bb_slice(1)),bb_attributes(ocf))

    temp <- bb_attributes_to_cols(ocf %>% bb_slice(1))
    expect_equal(ncol(temp),length(names(ocf))+length(bbae))
    expect_named(temp,c(names(ocf),bbae))##,ignore.order=TRUE)
})
