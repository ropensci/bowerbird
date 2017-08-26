context("sync")

test_that("bb_sync works with dry run on bb_wget",{
    skip_on_cran()
    temp_root <- tempdir()
    cf <- bb_config(local_file_root=temp_root,skip_downloads=TRUE)
    cf <- cf %>% add(bb_example_sources()[1,])
    bb_sync(cf,catch_errors=FALSE)
})

test_that("bb_sync is quiet when asked",{
    skip_on_cran()
    temp_root <- tempdir()
    cf <- bb_config(local_file_root=temp_root)
    myds <- bb_source(
        id="bilbobaggins",
        name="test",
        description="blah",
        reference= "http://some.where.org/",
        citation="blah",
        license="blah",
        method=bb_wget,
        source_url="https://github.com/AustralianAntarcticDivision/bowerbird/blob/master/README.Rmd", ## just some file to download
        method_flags=c("--recursive","--level=1"))
    cf <- cf %>% add(myds)
    expect_silent(bb_sync(cf,verbose=FALSE))
})

test_that("bb_sync works on oceandata",{
    skip_on_cran()
    ods <- bb_source(
        id="bilbobaggins",
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See http://oceancolor.gsfc.nasa.gov/cms/citations",
        license="Please cite",
        comment="",
        method=oceandata_get,
        method_flags=c("search=T20000322000060.L3m_MO_SST_sst_9km.nc"),
        postprocess=NULL,
        access_function="",
        data_group="Sea surface temperature")
    temp_root <- tempdir()
    cf <- add(bb_config(local_file_root=temp_root,clobber=2),ods)
    bb_sync(cf)

    fnm <- file.path(temp_root,"oceandata.sci.gsfc.nasa.gov/MODIST/Mapped/Monthly/9km/SST/T20000322000060.L3m_MO_SST_sst_9km.nc")
    expect_true(file.exists(fnm))
    fi <- file.info(fnm)
    expect_gt(fi$size,6e6)
})

test_that("bb_sync works with a sink() call in place",{
    skip_on_cran()
    sinkfile <- tempfile()
    sink(file=sinkfile)
    myds <- bb_source(
        id="bilbobaggins",
        name="test",
        description="blah",
        reference= "http://some.where.org/",
        citation="blah",
        license="blah",
        method=bb_wget,
        source_url="https://github.com/AustralianAntarcticDivision/bowerbird/blob/master/README.Rmd", ## just some file to download
        method_flags=c("--recursive","--level=1"))
    temp_root <- tempdir()
    cf <- add(bb_config(local_file_root=temp_root,clobber=2),myds)
    bb_sync(cf)
    sink()
    op <- readLines(sinkfile)
    ## sink file should contain direct cat output "Synchronizing dataset: test"
    expect_true(any(grepl("Synchronizing dataset: test",op)))
    ## sink file should also contain wget output, e.g. "FINISHED"
    expect_true(any(grepl("FINISHED",op)))
})
