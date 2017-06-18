context("data config")

test_that("config save/load works", {
    cf <- add(bb_config("/dummy/file/root"),bb_sources())
    expect_type(cf,"list")
    tmpfile <- tempfile()
    saveRDS(cf,tmpfile)
    expect_identical(cf,readRDS(file=tmpfile))
})
