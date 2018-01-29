context("postprocessing")

test_that("decompressing zip files works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/AustralianAntarcticDivision/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/AustralianAntarcticDivision/bowerbird/master/inst/extdata/example_data.zip",##"https://github.com/AustralianAntarcticDivision/bowerbird/raw/master/inst/extdata/example_data.zip",
        license="MIT",
        method=list("bb_handler_wget",recursive=TRUE,level=1,robots_off=TRUE),
        postprocess=list("bb_unzip"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data.zip")))
    expect_true(file.exists(file.path(fp,"example_data_was_zipped.csv")))
    x <- read.csv(file.path(fp,"example_data_was_zipped.csv"))
    expect_named(x,c("a","b","c"))
})

test_that("decompressing gzipped files works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/AustralianAntarcticDivision/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/AustralianAntarcticDivision/bowerbird/master/inst/extdata/example_data_was_gzipped.csv.gz",##"https://github.com/AustralianAntarcticDivision/bowerbird/raw/master/inst/extdata/example_data_was_gzipped.csv.gz",
        license="MIT",
        method=list("bb_handler_wget",recursive=TRUE,level=1,robots_off=TRUE),
        postprocess=list("bb_gunzip"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data_was_gzipped.csv.gz")))
    expect_true(file.exists(file.path(fp,"example_data_was_gzipped.csv")))
    x <- read.csv(file.path(fp,"example_data_was_gzipped.csv"))
    expect_named(x,c("a","b","c"))
})

test_that("decompressing bzipped files works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/AustralianAntarcticDivision/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/AustralianAntarcticDivision/bowerbird/master/inst/extdata/example_data_was_bzipped.csv.bz2",##"https://github.com/AustralianAntarcticDivision/bowerbird/raw/master/inst/extdata/example_data_was_bzipped.csv.bz2",
        license="MIT",
        method=list("bb_handler_wget",recursive=TRUE,level=1,robots_off=TRUE),
        postprocess=list("bb_bunzip2"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data_was_bzipped.csv.bz2")))
    expect_true(file.exists(file.path(fp,"example_data_was_bzipped.csv")))
    x <- read.csv(file.path(fp,"example_data_was_bzipped.csv"))
    expect_named(x,c("a","b","c"))
})

test_that("decompressing Z-compressed files works",{
    skip_on_cran()
    skip_on_os("windows") ## requires archive, which isn't yet on cran and requires compilation from source
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/AustralianAntarcticDivision/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/AustralianAntarcticDivision/bowerbird/master/inst/extdata/20170822.nc.Z",##"https://github.com/AustralianAntarcticDivision/bowerbird/raw/master/inst/extdata/20170822.nc.Z",
        license="MIT",
        method=list("bb_handler_wget",recursive=TRUE,level=1,robots_off=TRUE),
        postprocess=list("bb_uncompress"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"20170822.nc.Z")))
    expect_true(file.exists(file.path(fp,"20170822.nc")))
    expect_true(file.info(file.path(fp,"20170822.nc"))$size>1e4)
})

test_that("cleanup postprocessing works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/AustralianAntarcticDivision/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/AustralianAntarcticDivision/bowerbird/master/inst/extdata/example_data.zip",##"https://github.com/AustralianAntarcticDivision/bowerbird/raw/master/inst/extdata/example_data.zip",
        license="MIT",
        method=list("bb_handler_wget",recursive=TRUE,level=1,robots_off=TRUE),
        postprocess=list("bb_unzip",list("bb_cleanup",pattern="\\.csv$"))
    )
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_false(file.exists(file.path(fp,"example_data_was_zipped.csv")))
})
