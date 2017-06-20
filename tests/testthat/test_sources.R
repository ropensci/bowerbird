context("data sources")

test_that("predefined sources work", {
    src <- bb_sources(c("NSIDC passive microwave supporting files"))
    expect_s3_class(src,"data.frame")
    expect_equal(nrow(src),1)

    src_all <- bb_sources()
    expect_gt(nrow(src_all),0)
    src_si <- bb_sources(data_group="Sea ice")
    expect_gt(nrow(src_si),0)
    expect_lt(nrow(src_si),nrow(src_all))
    expect_true(all(src_si$data_group=="Sea ice"))
})

test_that("bb_source works with multiple postprocess actions", {
    bb_source(
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See http://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        comment="",
        method=oceandata_get,
        method_flags="search=T20000322000060.L3m_MO_SST_sst_9km.nc",
        postprocess=list(quote(pp_unzip(delete=TRUE)),pp_gunzip),
        access_function="",
        data_group="Sea surface temperature")
    
    bb_source(
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See http://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        comment="",
        method=oceandata_get,
        method_flags="search=T20000322000060.L3m_MO_SST_sst_9km.nc",
        postprocess=list(pp_unzip,pp_gunzip),
        access_function="",
        data_group="Sea surface temperature")
})
