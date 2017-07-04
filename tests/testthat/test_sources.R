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

test_that("sources with authentication have an authentication_note entry", {
    src <- bb_sources()
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

##test_that("an earthdata source works",{
##    ## earthdata test
##    temp_root <- tempdir()
##    cf <- bb_config(local_file_root=temp_root) %>%
##        add(bb_sources(name="NSIDC SMMR-SSM/I Nasateam sea ice concentration") %>%
##            mutate(user="benraymond",
##                   password="H(~7FkC:(U9GU9ffjC.Y$@hZo",
##                   source_url="https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/",##final-gsfc/south/daily/1978/",
##                   method=list(quote(earthdata_get)),
##                   method_flags="--recursive --level=inf --no-parent"))
##    bb_sync(cf)
##})

test_that("source nsidc0051 still works under ftp (due to be moved to https)",{
    skip_on_cran()
    temp_root <- tempdir()
    cf <- bb_config(local_file_root=temp_root) %>%
        add(bb_sources(name="NSIDC SMMR-SSM/I Nasateam sea ice concentration") %>%
            mutate(source_url="ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/1978/nt_19781231_n07_v1.1_s.bin"))
    bb_sync(cf)

    fnm <- file.path(temp_root,"sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/daily/1978/nt_19781231_n07_v1.1_s.bin")
    expect_true(file.exists(fnm))
    fi <- file.info(fnm)
    expect_gt(fi$size,50e3)
})

test_that("source nsidc0081 still works under ftp (due to be moved to https)",{
    skip_on_cran()
    temp_root <- tempdir()
    target_file <- format(Sys.Date()-10,"nt_%Y%m%d_f18_nrt_s.bin")
    cf <- bb_config(local_file_root=temp_root) %>%
        add(bb_sources(name="NSIDC SMMR-SSM/I Nasateam near-real-time sea ice concentration") %>%
            mutate(source_url=paste0("ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/south/",target_file)))

    bb_sync(cf)
    fnm <- file.path(temp_root,"sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/south",target_file)
    expect_true(file.exists(fnm))
    fi <- file.info(fnm)
    expect_gt(fi$size,50e3)
})

test_that("source nsidc0082 still works under ftp (due to be moved to https)",{
    skip_on_cran()
    temp_root <- tempdir()
    cf <- bb_config(local_file_root=temp_root) %>%
        add(bb_sources(name="Radarsat Antarctic digital elevation model V2") %>%
            mutate(source_url="ftp://sidads.colorado.edu/pub/DATASETS/nsidc0082_radarsat_dem_v02/200M/BINARY/ramp200dem_osu_v2.hdr")) %>%
            slice(1L)
    bb_sync(cf)
    fnm <- file.path(temp_root,"sidads.colorado.edu/pub/DATASETS/nsidc0082_radarsat_dem_v02/200M/BINARY/ramp200dem_osu_v2.hdr")
    expect_true(file.exists(fnm))
    fi <- file.info(fnm)
    expect_gt(fi$size,1e3)
})


test_that("trailing slash check for AADC EDS method URLs works",{
    expect_warning(temp <- bb_source(
        name="AADC test",
        description="blah",
        reference= "blah",
        citation="blah",
        source_url=c("first","second/","third","this_one_ok/download"),
        license="blah",
        method=aadc_eds_get,
        method_flags=""),
        regexp="trailing /")

    expect_equal(sum(grepl("/$",temp$source_url)),3)
})

test_that("selection by name or ID works",{
    temp1 <- bb_sources("CNES-CLS09 MDT")
    temp2 <- bb_sources("CNES-CLS09 Mean Dynamic Topography")
    expect_identical(temp1,temp2)
})

    
