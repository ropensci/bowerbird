context("data sources")

test_that("predefined sources work", {
    src <- bb_sources(c("NSIDC passive microwave supporting files"))
    expect_s3_class(src,"data.frame")
    expect_equal(nrow(src),1)
})
