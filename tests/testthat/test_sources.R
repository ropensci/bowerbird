context("data sources")

test_that("predefined sources work", {
    src <- bb_sources(c("seaice_nsidc_supporting","NSIDC passive microwave supporting files"))
    expect_s3_class(src,"data.frame")
    expect_equal(nrow(src),1)
})
