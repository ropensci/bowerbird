context("earthdata handler")

test_that("bb_handler_earthdata works", {
    skip_if_not(nzchar(Sys.getenv("EARTHDATA_USER")), "skipping earthdata test download, requires Earth data login (set the EARTHDATA_USER and EARTHDATA_PASS environment variables)")
    skip_if_not(nzchar(Sys.getenv("EARTHDATA_PASS")), "skipping earthdata test download, requires Earth data login (set the EARTHDATA_USER and EARTHDATA_PASS environment variables)")

    src <- bb_example_sources("Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 3") %>%
        bb_modify_source(user = Sys.getenv("EARTHDATA_USER"), password = Sys.getenv("EARTHDATA_PASS"),
                         method = list(level = 3, accept_download = "monthly-climatology/.+\\.s\\.png"))
    temp_root <- tempdir()
    res <- bb_get(src, local_file_root = temp_root)

    expect_true(nrow(res$files[[1]]) == 12)
    expect_true(all(file.exists(res$files[[1]]$file)))
})
