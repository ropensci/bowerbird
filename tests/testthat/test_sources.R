context("data sources")

test_that("predefined sources work", {
    src <- bb_example_sources()
    expect_gt(nrow(src),0)
    src <- src[1,]
    expect_s3_class(src,"data.frame")
    expect_equal(nrow(src),1)
})

test_that("empty/missing/NA source_urls get dealt with correctly",{
    ds <- bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        reference="xxx",
        citation="blah",
        license="",
        source_url="",
        method=quote(bb_handler_oceandata))
    expect_identical(ds$source_url,list(c(NA_character_)))

    ## missing/empty source_url (if allowed by the handler) should be converted to NA
    ds <- bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        reference="xxx",
        citation="blah",
        license="",
        method=quote(bb_handler_oceandata))
    expect_identical(ds$source_url,list(c(NA_character_)))
    ds <- bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        reference="xxx",
        citation="blah",
        source_url="",
        license="",
        method=quote(bb_handler_oceandata))
    expect_identical(ds$source_url,list(c(NA_character_)))

    ## wget handler requires non-missing/non-NA/non-empty source_url
    expect_error(bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        reference="xxx",
        citation="blah",
        license="",
        method=quote(bb_handler_wget)))
    expect_error(bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        reference="xxx",
        citation="blah",
        source_url="",
        license="",
        method=quote(bb_handler_wget)))
    expect_error(bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        reference="xxx",
        citation="blah",
        source_url=NA,
        license="",
        method=quote(bb_handler_wget)))

    ## multiple source_urls, empty/NA ones should be removed
    ds <- bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        reference="xxx",
        citation="blah",
        source_url=c("aaa","",NA_character_),
        license="",
        method=quote(bb_handler_wget))
    expect_identical(ds$source_url,list(c("aaa")))

})

test_that("bb_source works with multiple postprocess actions", {
    bb_source(
        id="bilbobaggins",
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See http://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        comment="",
        method=bb_handler_oceandata,
        method_flags=c("search=T20000322000060.L3m_MO_SST_sst_9km.nc"),
        postprocess=list(quote(bb_unzip(delete=TRUE)),bb_gunzip),
        access_function="",
        data_group="Sea surface temperature")

    bb_source(
        id="bilbobaggins",
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See http://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        comment="",
        method=bb_handler_oceandata,
        method_flags=c("search=T20000322000060.L3m_MO_SST_sst_9km.nc"),
        postprocess=list(bb_unzip,bb_gunzip),
        access_function="",
        data_group="Sea surface temperature")
})

test_that("sources with authentication have an authentication_note entry", {
    src <- bb_example_sources()
    idx <- (!is.na(src$user) | !is.na(src$password)) & na_or_empty(src$authentication_note)
    expect_false(any(idx),sprintf("%d data sources with non-NA authentication but no authentication_note entry",sum(idx)))
})

test_that("authentication checks work",{
    expect_warning(bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        method=bb_handler_wget,
        method_flags=character(),
        postprocess=NULL,
        data_group="blah"))

    expect_warning(bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        user="",
        method=bb_handler_wget,
        method_flags=character(),
        postprocess=NULL,
        data_group="blah"))

    expect_warning(bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        password="",
        method=bb_handler_wget,
        method_flags=character(),
        postprocess=NULL,
        data_group="blah"))

    ## no warning
    bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        reference="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        user="user",
        password="password",
        method=bb_handler_wget,
        method_flags=character(),
        postprocess=NULL,
        data_group="blah")
})



