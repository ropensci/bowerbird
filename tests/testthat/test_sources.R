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
        doc_url="xxx",
        citation="blah",
        license="",
        source_url="",
        method=list("bb_handler_oceandata"))
    expect_identical(ds$source_url,list(c(NA_character_)))

    ## missing/empty source_url (if allowed by the handler) should be converted to NA
    ds <- bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        doc_url="xxx",
        citation="blah",
        license="",
        method=list("bb_handler_oceandata"))
    expect_identical(ds$source_url,list(c(NA_character_)))
    ds <- bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        doc_url="xxx",
        citation="blah",
        source_url="",
        license="",
        method=list("bb_handler_oceandata"))
    expect_identical(ds$source_url,list(c(NA_character_)))

    ## wget handler requires non-missing/non-NA/non-empty source_url
    expect_error(bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        doc_url="xxx",
        citation="blah",
        license="",
        method=list("bb_handler_wget")),"requires at least one non-empty source URL")
    expect_error(bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        doc_url="xxx",
        citation="blah",
        source_url="",
        license="",
        method=list(bb_handler_wget)),"requires at least one non-empty source URL")
    expect_error(bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        doc_url="xxx",
        citation="blah",
        source_url=NA,
        license="",
        method=list(bb_handler_wget)),"requires at least one non-empty source URL")

    ## multiple source_urls, empty/NA ones should be removed
    ds <- bb_source(
        id="xxx",
        name="xxx",
        description="xxx",
        doc_url="xxx",
        citation="blah",
        source_url=c("aaa","",NA_character_),
        license="",
        method=list(bb_handler_wget))
    expect_identical(ds$source_url,list(c("aaa")))

})

test_that("bb_source works with multiple postprocess actions", {
    bb_source(
        id="bilbobaggins",
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        doc_url= "https://oceancolor.gsfc.nasa.gov/",
        citation="See https://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        comment="",
        method=list("bb_handler_oceandata",search="T20000322000060.L3m_MO_SST_sst_9km.nc"),
        postprocess=list(list("bb_unzip",delete=TRUE),"bb_gunzip"),
        access_function="",
        data_group="Sea surface temperature")

    bb_source(
        id="bilbobaggins",
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        doc_url= "https://oceancolor.gsfc.nasa.gov/",
        citation="See https://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        comment="",
        method=list("bb_handler_oceandata",search="T20000322000060.L3m_MO_SST_sst_9km.nc"),
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
        doc_url="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        method=list("bb_handler_wget"),
        postprocess=NULL,
        data_group="blah"),"requires authentication")

    expect_warning(bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        doc_url="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        user="",
        method=list("bb_handler_wget"),
        postprocess=NULL,
        data_group="blah"),"requires authentication")

    expect_warning(bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        doc_url="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        password="",
        method=list("bb_handler_wget"),
        postprocess=NULL,
        data_group="blah"),"requires authentication")

    ## no warning
    bb_source(
        id="bilbobaggins",
        name="Test",
        description="blah",
        doc_url="blah",
        citation="blah",
        source_url="blah",
        license="blah",
        authentication_note="auth note",
        user="user",
        password="password",
        method=list("bb_handler_wget"),
        postprocess=NULL,
        data_group="blah")
})

test_that("bb_modify_source works",{
    ## use this one
    src <- subset(bb_example_sources(),id=="SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047")
    ## check src is as expected
    expect_identical(src$method[[1]],list("bb_handler_wget",recursive=TRUE,level=3))
    expect_identical(src$postprocess[[1]],list(list("bb_gunzip")))

    expect_error(src %>% bb_modify_source(bilbo="baggins",frodo="also baggins"),"unexpected input parameters")

    ## simple text parameters
    expect_warning(temp <- src %>% bb_modify_source(user="me"),"requires authentication") ## needs both user and password
    expect_warning(temp <- src %>% bb_modify_source(password="blah"),"requires authentication") ## needs both user and password
    temp <- src %>% bb_modify_source(user="me",password="blah")
    expect_identical(temp$user,"me")
    expect_identical(temp$password,"blah")
    ## test mandatory parm made missing
    expect_error(src %>% bb_modify_source(user="me",password="blah",doc_url=NULL),"provide a doc_url")
    ## test source urls
    ## can't be a list
    expect_error(src %>% bb_modify_source(user="me",password="blah",source_url=list("new_url1","new_url2")),"not a character vector")
    temp <- src %>% bb_modify_source(user="me",password="blah",source_url=c("new_url1","new_url2"))
    expect_identical(temp$source_url,list(c("new_url1","new_url2")))

    ## method is tricker, is a list
    ## pass illegal handler (not a function)
    expect_error(src %>% bb_modify_source(user="me",password="blah",method=list("notafunction",a=1)),"resolves to a function")
    temp <- src %>% bb_modify_source(user="me",password="blah",method=list("bb_handler_oceandata",a=1))
    ## this should replace the method function name, and add a=1, but leave the existing recursive and level elements
    expect_identical(temp$method[[1]],list("bb_handler_oceandata",recursive=TRUE,level=3,a=1))
    temp <- src %>% bb_modify_source(user="me",password="blah",method=list("bb_handler_wget",level=NULL)) ## should remove the level element
    expect_identical(temp$method[[1]],list("bb_handler_wget",recursive=TRUE))

    ## warn_empty_auth behaviour
    expect_warning(src %>% bb_modify_source(),"requires authentication")
    temp <- src %>% bb_modify_source(warn_empty_auth=FALSE) ## OK

    ## postprocess is also a list
    expect_error(src %>% bb_modify_source(user="me",password="blah",postprocess=c()),"is not a list")
    expect_identical(src,src %>% bb_modify_source(postprocess=list(),warn_empty_auth=FALSE)) ## identical
    temp <- src %>% bb_modify_source(user="me",password="blah",postprocess=list("bb_cleanup")) ## new pp element
    expect_equal(length(temp$postprocess[[1]]),2)
    temp <- src %>% bb_modify_source(user="me",password="blah",postprocess=list(list("bb_cleanup",pattern="*.zip"))) ## new pp element with arg
    expect_equal(length(temp$postprocess[[1]]),2)
    expect_identical(temp$postprocess[[1]],list(list("bb_gunzip"),list("bb_cleanup",pattern="*.zip")))
    temp <- src %>% bb_modify_source(user="me",password="blah",postprocess=list(list("bb_gunzip",delete=TRUE))) ## modify existing pp element
    expect_equal(length(temp$postprocess[[1]]),1)
    expect_identical(temp$postprocess[[1]],list(list("bb_gunzip",delete=TRUE)))
})
