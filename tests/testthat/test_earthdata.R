context("earthdata handler")

test_that("bb_handler_earthdata works", {
    skip_if_not(nzchar(Sys.getenv("EARTHDATA_USER")), "skipping earthdata test download, requires Earth data login (set the EARTHDATA_USER and EARTHDATA_PASS environment variables)")
    skip_if_not(nzchar(Sys.getenv("EARTHDATA_PASS")), "skipping earthdata test download, requires Earth data login (set the EARTHDATA_USER and EARTHDATA_PASS environment variables)")

    src <- bb_example_sources("Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 3") %>%
        bb_modify_source(user = Sys.getenv("EARTHDATA_USER"), password = Sys.getenv("EARTHDATA_PASS"),
                         method = list(level = 3, accept_download = "monthly-climatology/.+\\.s\\.png"))
    temp_root <- tempfile()
    dir.create(temp_root)
    res <- bb_get(src, local_file_root = temp_root)

    expect_equal(nrow(res$files[[1]]), 12)
    expect_true(all(file.exists(res$files[[1]]$file)))
    unlink(temp_root, recursive = TRUE)
})

test_that("bb_handler_earthdata requires credentials unless doing a dry run", {
    src <- bb_example_sources("Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 3")
    temp_root <- tempfile()
    dir.create(temp_root)
    on.exit(unlink(temp_root, recursive = TRUE))
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    expect_error(bb_sync(cf, catch_errors = FALSE, confirm_downloads_larger_than = NULL), "requires authentication")
    ## validation passes for a dry run (whether a dry run is actually possible or not depends on the public status of the earthdata resource, so we can only find out at runtime)
    st <- bb_settings(cf)
    st$dry_run <- TRUE
    bb_settings(cf) <- st
    expect_true(bowerbird:::bb_validate(cf))
    ## and the handler itself refuses a non-dry run without credentials
    st$dry_run <- FALSE
    bb_settings(cf) <- st
    expect_error(bb_handler_earthdata(cf), "requires user and password")
    ## actually trying that dry run will fail because this resource does actually require credentials, even for dry run
    res <- bb_get(src, local_file_root = tempdir(), dry_run = TRUE)
    expect_false(res$status)
    ## other sources requiring authentication still fail validation in a dry run
    src2 <- bb_source(name = "CMEMS global gridded SSH reprocessed (1993-ongoing) test",
                      id = "SEALEVEL_GLO_PHY_L4_MY_008_047-test",
                      description = "Test",
                      doc_url = "https://data.marine.copernicus.eu/product/SEALEVEL_GLO_PHY_L4_MY_008_047/description",
                      citation = "Test",
                      license = "Test",
                      method = list("bb_handler_copernicus", product = "SEALEVEL_GLO_PHY_L4_MY_008_047"),
                      authentication_note = "Requires authentication",
                      user = "", password = "", warn_empty_auth = FALSE)
    cf2 <- bb_add(bb_config(local_file_root = temp_root), src2)
    st2 <- bb_settings(cf2)
    st2$dry_run <- TRUE
    bb_settings(cf2) <- st2
    expect_error(bowerbird:::bb_validate(cf2), "requires authentication")
    expect_error(res <- bb_get(src2, local_file_root = tempdir(), dry_run = TRUE), "requires authentication")
})

test_that("bb_handler_earthdata dry run works without credentials", {
    skip_on_cran()
    ## CMR virtual directory listings are public, so a dry run needs no login
    src <- bb_source(
        name = "GHRSST Level 4 MUR Global Foundation SST v4.1 (one day)",
        id = "GHRSST-MUR-SST_v4.1-test",
        description = "MUR SST, single day, used for testing",
        doc_url = "https://podaac.jpl.nasa.gov/dataset/MUR-JPL-L4-GLOB-v4.1",
        citation = "See doc_url",
        source_url = "https://cmr.earthdata.nasa.gov/virtual-directory/collections/C1996881146-POCLOUD/temporal/2023/01/01",
        license = "Please cite",
        method = list("bb_handler_earthdata", level = 2, accept_follow = "/virtual-directory/collections/C1996881146-POCLOUD/temporal/", accept_download = "\\.nc$", allow_unrestricted_auth = TRUE),
        user = "",
        password = "",
        collection_size = 0.001,
        warn_empty_auth = FALSE)
    temp_root <- tempfile()
    dir.create(temp_root)
    on.exit(unlink(temp_root, recursive = TRUE))
    res <- bb_get(src, local_file_root = temp_root, dry_run = TRUE)
    expect_true(res$status)
    expect_gte(nrow(res$files[[1]]), 1)
    expect_true(all(grepl("\\.nc$", res$files[[1]]$url)))
    expect_equal(length(list.files(temp_root, recursive = TRUE)), 0)
})
