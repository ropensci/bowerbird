context("data config")

test_that("config save/load works", {
    cf <- add(bb_config(),bb_sources())
    expect_type(cf,"list")
    tmpfile <- tempfile()
    bb_save_config(cf,tmpfile)
    expect_identical(cf,bb_config(file=tmpfile))
})
