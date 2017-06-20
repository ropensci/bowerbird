context("data sources")

test_that("predefined sources work", {
    expect_warning(src <- bb_sources(c("NSIDC passive microwave supporting files")))
    expect_s3_class(src,"data.frame")
    expect_equal(nrow(src),1)

    expect_warning(src_all <- bb_sources())
    expect_gt(nrow(src_all),0)
    expect_warning(src_si <- bb_sources(data_group="Sea ice"))
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

test_that("sources with authentication have an authentication_note entry", {
    expect_warning(src <- bb_sources())
    idx <- (!is.na(src$user) | !is.na(src$password)) & na_or_empty(src$authentication_note)
    expect_false(any(idx),sprintf("%d data sources with non-NA authentication but no authentication_note entry",sum(idx)))
})

test_that("authentication checks work",{
    expect_warning(bb_source(
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        method=bb_wget,
        method_flags="",
        postprocess=NULL,
        data_group="blah"))
    
    expect_warning(bb_source(
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        user="",
        method=bb_wget,
        method_flags="",
        postprocess=NULL,
        data_group="blah"))

    expect_warning(bb_source(
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        password="",
        method=bb_wget,
        method_flags="",
        postprocess=NULL,
        data_group="blah"))

    ## no warning
    bb_source(
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        user="user",
        password="password",
        method=bb_wget,
        method_flags="",
        postprocess=NULL,
        data_group="blah")
})

