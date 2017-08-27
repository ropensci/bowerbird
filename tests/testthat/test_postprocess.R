context("postprocessing")

test_that("decompressing zip files works",{
    skip_on_cran()
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        reference="https://github.com/AustralianAntarcticDivision/bowerbird",
        citation="No citation needed.",
        source_url="https://github.com/AustralianAntarcticDivision/bowerbird/raw/master/inst/extdata/example_data.zip",
        license="MIT",
        method=quote(bb_handler_wget),
        method_flags=c("--recursive","--level=1","-e","robots=off"),
        postprocess=quote(bb_unzip))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data.zip")))
    expect_true(file.exists(file.path(fp,"example_data_was_zipped.csv")))
    x <- read.csv(file.path(fp,"example_data_was_zipped.csv"))
    expect_named(x,c("a","b","c"))
})

test_that("decompressing gzipped files works",{
    skip_on_cran()
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        reference="https://github.com/AustralianAntarcticDivision/bowerbird",
        citation="No citation needed.",
        source_url="https://github.com/AustralianAntarcticDivision/bowerbird/raw/master/inst/extdata/example_data_was_gzipped.csv.gz",
        license="MIT",
        method=quote(bb_handler_wget),
        method_flags=c("--recursive","--level=1","-e","robots=off"),
        postprocess=quote(bb_gunzip))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data_was_gzipped.csv.gz")))
    expect_true(file.exists(file.path(fp,"example_data_was_gzipped.csv")))
    x <- read.csv(file.path(fp,"example_data_was_gzipped.csv"))
    expect_named(x,c("a","b","c"))
})

test_that("decompressing bzipped files works",{
    skip_on_cran()
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        reference="https://github.com/AustralianAntarcticDivision/bowerbird",
        citation="No citation needed.",
        source_url="https://github.com/AustralianAntarcticDivision/bowerbird/raw/master/inst/extdata/example_data_was_bzipped.csv.bz2",
        license="MIT",
        method=quote(bb_handler_wget),
        method_flags=c("--recursive","--level=1","-e","robots=off"),
        postprocess=quote(bb_bunzip2))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data_was_bzipped.csv.bz2")))
    expect_true(file.exists(file.path(fp,"example_data_was_bzipped.csv")))
    x <- read.csv(file.path(fp,"example_data_was_bzipped.csv"))
    expect_named(x,c("a","b","c"))
})



