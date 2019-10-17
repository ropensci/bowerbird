context("AWS S3 handler")
test_that("bb_handler_aws_s3 works", {
    src <- bb_source(
        name = "eBird Status and Trends example data",
        id = "yebsap-ERD2016-EBIRD_SCIENCE-20180729-7c8cec83",
        description = "The example data consists of the results for Yellow-bellied Sapsucker subset to Michigan",
        doc_url = "https://github.com/CornellLabofOrnithology/ebirdst/blob/master/R/ebirdst-loading.R",
        citation = "Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, M. Iliff, and S. Kelling. eBird Status and Trends. Version: November 2018. https://ebird.org/science/status-and-trends. Cornell Lab of Ornithology, Ithaca, New York",
        license = "CC-BY",
        method = list("bb_handler_aws_s3", bucket = "", base_url = "amazonaws.com", prefix = "yebsap-ERD2016-EBIRD_SCIENCE-20180729-7c8cec83", region = "clo-is-da-example-data.s3", accept_download = "config\\.RData$"),
        comment = "Unusual spec of region and base_url is a workaround for an aws.s3 issue, see https://github.com/cloudyr/aws.s3/issues/318",
        postprocess = NULL,
        collection_size = 0.1,
        data_group = "Biology")
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    expect_true(grepl("clo-is-da-example-data.s3.amazonaws.com/yebsap-ERD2016-EBIRD_SCIENCE-20180729-7c8cec83$", bb_data_source_dir(cf)))
    status <- bb_sync(cf, confirm_downloads_larger_than = NULL)

    expect_equal(nrow(status$files[[1]]), 1)
    expect_true(file.exists(status$files[[1]]$file))
    fi <- file.info(status$files[[1]]$file)
    expect_gt(fi$size, 4e3)
    expect_lt(fi$size, 5e3)

    ## test the same source using accept_download_extra
    ## this should download all tif, csv, txt files plus the .RData file
    src <- bb_source(
        name = "eBird Status and Trends example data",
        id = "yebsap-ERD2016-EBIRD_SCIENCE-20180729-7c8cec83",
        description = "The example data consists of the results for Yellow-bellied Sapsucker subset to Michigan",
        doc_url = "https://github.com/CornellLabofOrnithology/ebirdst/blob/master/R/ebirdst-loading.R",
        citation = "Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, M. Iliff, and S. Kelling. eBird Status and Trends. Version: November 2018. https://ebird.org/science/status-and-trends. Cornell Lab of Ornithology, Ithaca, New York",
        license = "CC-BY",
        method = list("bb_handler_aws_s3", bucket = "", base_url = "amazonaws.com", prefix = "yebsap-ERD2016-EBIRD_SCIENCE-20180729-7c8cec83", region = "clo-is-da-example-data.s3", accept_download_extra = "config\\.RData$"),
        comment = "Unusual spec of region and base_url is a workaround for an aws.s3 issue, see https://github.com/cloudyr/aws.s3/issues/318",
        postprocess = NULL,
        collection_size = 0.1,
        data_group = "Biology")
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    status <- bb_sync(cf, confirm_downloads_larger_than = NULL, dry_run = TRUE)
    expect_gt(nrow(status$files[[1]]), 10)
    expect_true(all(grepl("(tif|csv|txt|RData)$", status$files[[1]]$file)))

    ## test the same source using reject_download
    src <- bb_source(
        name = "eBird Status and Trends example data",
        id = "yebsap-ERD2016-EBIRD_SCIENCE-20180729-7c8cec83",
        description = "The example data consists of the results for Yellow-bellied Sapsucker subset to Michigan",
        doc_url = "https://github.com/CornellLabofOrnithology/ebirdst/blob/master/R/ebirdst-loading.R",
        citation = "Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, M. Iliff, and S. Kelling. eBird Status and Trends. Version: November 2018. https://ebird.org/science/status-and-trends. Cornell Lab of Ornithology, Ithaca, New York",
        license = "CC-BY",
        method = list("bb_handler_aws_s3", bucket = "", base_url = "amazonaws.com", prefix = "yebsap-ERD2016-EBIRD_SCIENCE-20180729-7c8cec83", region = "clo-is-da-example-data.s3", reject_download = "(tif|csv|txt)$"),
        comment = "Unusual spec of region and base_url is a workaround for an aws.s3 issue, see https://github.com/cloudyr/aws.s3/issues/318",
        postprocess = NULL,
        collection_size = 0.1,
        data_group = "Biology")
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    status <- bb_sync(cf, confirm_downloads_larger_than = NULL)
    expect_equal(nrow(status$files[[1]]), 0)

    ## SILO climate data
    src <- bb_source(
        name = "SILO climate data",
        id = "silo-open-data",
        description = "Australian climate data from 1889 to yesterday. This source includes a single example monthly rainfall data file.",
        doc_url = "https://www.longpaddock.qld.gov.au/silo/gridded-data/",
        citation = "SILO datasets are constructed by the Queensland Government using observational data provided by the Australian Bureau of Meteorology and are available under the Creative Commons Attribution 4.0 license",
        license = "CC-BY 4.0",
        method = list("bb_handler_aws_s3", region = "silo-open-data.s3",  base_url = "amazonaws.com", prefix = "annual/monthly_rain/", accept_download = "2005\\.monthly_rain\\.nc$"),
        comment = "Unusual spec of region and base_url is a workaround for an aws.s3 issue, see https://github.com/cloudyr/aws.s3/issues/318",
        postprocess = NULL,
        collection_size = 0.02,
        data_group = "Climate")
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    expect_true(grepl("silo-open-data.s3.amazonaws.com/annual/monthly_rain$", bb_data_source_dir(cf)))
    status <- bb_sync(cf, confirm_downloads_larger_than = NULL, verbose = TRUE)

    expect_equal(nrow(status$files[[1]]), 1)
    expect_true(file.exists(status$files[[1]]$file))
    fi <- file.info(status$files[[1]]$file)
    expect_gt(fi$size, 14e6)
})
