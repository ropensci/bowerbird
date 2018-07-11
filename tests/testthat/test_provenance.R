context("provenance functions")

test_that("bb_fingerprint does something sensible",{
    skip_on_cran()
    ##skip_on_appveyor()
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/AustralianAntarcticDivision/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data_was_gzipped.csv.gz",
        license="MIT",
        method=list("bb_handler_rget", level = 1),
        postprocess=list("bb_gunzip"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fgpr <- bb_fingerprint(cf)
    expect_named(fgpr,c("filename","data_source_id","size","last_modified","hash"))
    expect_equal(fgpr$hash[grepl("gz$",fgpr$filename)],"9d27e2bba97224f8934f1c591d9f50888f65348a")
})
