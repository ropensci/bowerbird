context("thredds handler")

test_that("bb_handler_thredds works", {
    my_source <- bb_source(
        name = "OSI SAF Global Low Resolution Sea Ice Drift",
        id = "10.15770/EUM_SAF_OSI_NRT_2007",
        description = "Example dataset.",
        doc_url = "https://osi-saf.eumetsat.int/products/osi-405-c",
        ## just the 2009 subset for demo purposes
        source_url = "https://thredds.met.no/thredds/catalog/osisaf/met.no/ice/drift_lr/merged/2009/catalog.html",
        citation = "See https://doi.org/10.15770/EUM_SAF_OSI_NRT_2007",
        license = "Please cite",
        method = list("bb_handler_thredds", level = 2))

    temp_root <- tempdir()
    res <- bb_get(my_source, local_file_root = temp_root, dry_run = TRUE)
    expect_true(nrow(res$files[[1]]) == 24)

    ## actually download, but just one file
    my_source <- bb_modify_source(my_source, method = list(accept_download = "200912061200.+\\.nc"))
    res <- bb_get(my_source, local_file_root = temp_root, dry_run = FALSE)
    expect_true(nrow(res$files[[1]]) == 1)
    expect_true(file.exists(res$files[[1]]$file))
    expect_true(file.size(res$files[[1]]$file) > 500e3)
})
