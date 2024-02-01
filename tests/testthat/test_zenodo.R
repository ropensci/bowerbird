context("Zenodo sources")
test_that("A small Zenodo source works", {
    src <- bb_zenodo_source(3533328)
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    expect_true(all(grepl("zenodo.org/api/records/3533328/files", bb_data_source_dir(cf))))
    status <- bb_sync(cf, confirm_downloads_larger_than = NULL)
    expect_equal(nrow(status$files[[1]]), 14)
    expect_true(all(file.exists(status$files[[1]]$file)))
    fi <- file.size(status$files[[1]]$file)
    expect_true(all(fi > 0))
})


test_that("Zenodo 'use_latest' works", {
    src1 <- bb_zenodo_source(6131579, use_latest = FALSE)
    expect_true(grepl("6131579", src1$id))
    src2 <- bb_zenodo_source(6131579, use_latest = TRUE)
    expect_false(grepl("6131579", src2$id))
    expect_false(isTRUE(all.equal(src1, src2)))
})
