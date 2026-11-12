test_that("various dry run tests",{
    ## an oceandata source with not too many files
    od_src <- bb_source(name = "Oceandata VIIRS Level-3 mapped seasonal 9km chl-a",
                     id = "VIIRS_L3m_SNxx_SNPP_CHL_chlor_a_9km",
                     description = "Seasonal remote-sensing chlorophyll-a from the VIIRS satellite at 9km spatial resolution",
                     doc_url = "https://oceancolor.gsfc.nasa.gov/",
                     citation = generic_oceandata_citation,
                     license = "Please cite",
                     method = list("bb_handler_oceandata", period = c("SNWI", "SNSP", "SNSU", "SNAU"), sensor = "VIIRS", dtype = "L3m", dtid = 1007, resolution_id = "9km", prod_id = "chlor_a", sdate = "2020-01-01"),
                     postprocess = list("bb_oceandata_cleanup"),
                     collection_size = 0.5,
                     comment = "NRT files will be included unless a non-NRT version exists for the same day",
                     user = "", password = "", warn_empty_auth = FALSE,
                     authentication_note = "Requires Earthdata login, see https://urs.earthdata.nasa.gov/. Note that you will also need to authorize the application 'OB.DAAC Data Access' (see 'My Applications' at https://urs.earthdata.nasa.gov/profile)",
                     data_group = "Ocean colour")
    res <- bb_get(od_src, local_file_root = tempdir(), verbose = interactive(), dry_run = TRUE)

    expect_true(nrow(res$files[[1]]) > 0)
    expect_true(setequal(names(res$files[[1]]), c("url", "file", "note", "last_modified", "size")))
    ## for oceandata, the last_modified and size columns should be populated
    expect_false(any(is.na(res$files[[1]]$last_modified)))
    expect_is(res$files[[1]]$last_modified, "POSIXt")
    expect_false(any(is.na(res$files[[1]]$size)))
    expect_is(res$files[[1]]$size, "integer")

    ## an S3 source
    s3_src <- bb_source(name = "SILO climate data",
                        id = "silo-open-data",
                        description = "Australian climate data from 1889 to yesterday. This source includes a single example monthly rainfall data file.",
                        doc_url = "https://www.longpaddock.qld.gov.au/silo/gridded-data/",
                        citation = "SILO datasets are constructed by the Queensland Government using observational data provided by the Australian Bureau of Meteorology and are available under the Creative Commons Attribution 4.0 license",
                        license = "CC-BY 4.0",
                        method = list("bb_handler_aws_s3", region = "",  base_url = "silo-open-data.s3.amazonaws.com", prefix = "Official/annual/monthly_rain/", accept_download = "202[456]\\.monthly_rain\\.nc$"),
                        postprocess = NULL,
                        collection_size = 0.02,
                        data_group = "Climate")
    s3_res <- bb_get(s3_src, local_file_root = tempdir(), verbose = interactive(), dry_run = TRUE)
    expect_equal(nrow(s3_res$files[[1]]), 3)
    expect_true(setequal(names(s3_res$files[[1]]), c("url", "file", "note", "last_modified", "size")))
    expect_false(any(is.na(s3_res$files[[1]]$last_modified)))
    expect_is(s3_res$files[[1]]$last_modified, "POSIXt")
    expect_false(any(is.na(s3_res$files[[1]]$size)))
    expect_is(s3_res$files[[1]]$size, "integer")

    ## a regular http/rget source
    src <- bb_source(id = "bilbobaggins", name = "test", description = "blah", doc_url =  "http://some.where.org/",
                     citation = "blah", license = "blah", method = list("bb_handler_rget", level = 1),
                     source_url = c("https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data.zip",
                                    "https://github.com/ropensci/bowerbird/blob/master/README.Rmd"))
    res <- bb_get(src, local_file_root = tempdir(), verbose = interactive(), dry_run = TRUE)
    expect_true(nrow(res$files[[1]]) > 0)
    expect_true(setequal(names(res$files[[1]]), c("url", "file", "note", "last_modified", "size")))
    expect_true(all(is.na(res$files[[1]]$last_modified)))
    expect_is(res$files[[1]]$last_modified, "POSIXt")
    expect_true(all(is.na(res$files[[1]]$size)))
    expect_is(res$files[[1]]$size, "integer")

    ## sync from a snapshot that has size/last-modified info
    src_s3b <- bb_add_snapshot(s3_src, s3_res)
    ## re-run a dry run, which is a bit silly but we should end up with an unchanged result
    res <- bb_get(src_s3b, local_file_root = tempdir(), verbose = interactive(), dry_run = TRUE)
    expect_equal(res$files, s3_res$files)

    ## an actual sync
    temp_root <- tempfile()
    dir.create(temp_root)
    on.exit(unlink(temp_root, recursive = TRUE))
    cf <- bb_config(local_file_root = temp_root) %>% bb_add(src_s3b)
    res <- bb_sync(cf, verbose = interactive(), confirm_downloads_larger_than = NULL)
    expect_equal(nrow(res$files[[1]]), 3)
    expect_true(setequal(names(res$files[[1]]), c("url", "file", "note", "last_modified", "size")))
    expect_false(any(is.na(res$files[[1]]$last_modified)))
    expect_is(res$files[[1]]$last_modified, "POSIXt")
    expect_false(any(is.na(res$files[[1]]$size)))
    expect_is(res$files[[1]]$size, "integer")

    ## and re-sync, nothing should download because we have it already
    res2 <- bb_sync(cf, verbose = interactive(), confirm_downloads_larger_than = NULL)

})
