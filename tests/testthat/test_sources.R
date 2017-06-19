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
