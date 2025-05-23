context("AWS S3 handler")
test_that("bb_handler_aws_s3 works", {
    src <- bb_source(
        name = "Crowdsourced Bathymetry example data",
        id = "odp-noaa-nesdis-ncei-csb",
        description = "A small example bathymetry csv data set",
        doc_url = "https://noaa-dcdb-bathymetry-pds.s3.amazonaws.com/docs/readme.html",
        citation = "Crowdsourced Bathymetry was accessed on DATE from https://registry.opendata.aws/odp-noaa-nesdis-ncei-csb",
        license = "Please cite",
        method = list("bb_handler_aws_s3", bucket = "", base_url = "noaa-dcdb-bathymetry-pds.s3.amazonaws.com", prefix = "", region = "", accept_download = "20170425114516079166_.*"),
        postprocess = NULL,
        collection_size = 0.1)
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    expect_true(grepl("noaa\\-dcdb\\-bathymetry\\-pds\\.s3\\.amazonaws\\.com[/\\\\]?$", bb_data_source_dir(cf)))
    status <- bb_sync(cf, confirm_downloads_larger_than = NULL)
    expect_equal(nrow(status$files[[1]]), 1)
    expect_true(file.exists(status$files[[1]]$file))
    fi <- file.info(status$files[[1]]$file)
    expect_gt(fi$size, 1e3)
    expect_lt(fi$size, 2e3)

    ## SILO climate data
    src <- bb_source(
        name = "SILO climate data",
        id = "silo-open-data",
        description = "Australian climate data from 1889 to yesterday. This source includes a single example monthly rainfall data file.",
        doc_url = "https://www.longpaddock.qld.gov.au/silo/gridded-data/",
        citation = "SILO datasets are constructed by the Queensland Government using observational data provided by the Australian Bureau of Meteorology and are available under the Creative Commons Attribution 4.0 license",
        license = "CC-BY 4.0",
        method = list("bb_handler_aws_s3", region = "",  base_url = "silo-open-data.s3.amazonaws.com", prefix = "Official/annual/monthly_rain/", accept_download = "2005\\.monthly_rain\\.nc$"),
        postprocess = NULL,
        collection_size = 0.02,
        data_group = "Climate")
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    expect_true(grepl("silo-open-data.s3.amazonaws.com/Official/annual/monthly_rain/?$", bb_data_source_dir(cf)))
    status <- bb_sync(cf, confirm_downloads_larger_than = NULL, verbose = TRUE)

    expect_equal(nrow(status$files[[1]]), 1)
    expect_true(file.exists(status$files[[1]]$file))
    fi <- file.info(status$files[[1]]$file)
    expect_gt(fi$size, 0)
    ## first few bytes should indicate that it's HDF
    chk <- readBin(status$files[[1]]$file, "raw", n = 6L)
    expect_identical(chk, as.raw(c(137, utf8ToInt("HDF\r\n"))))

    ## go again, it should not download anything
    status <- bb_sync(cf, confirm_downloads_larger_than = NULL, verbose = TRUE)
    expect_identical(status$files[[1]]$note, "existing copy")
})
