context("postprocessing")

test_that("decompressing zip files works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data.zip",
        license="MIT",
        method=list("bb_handler_rget", level = 0),
        postprocess=list("bb_unzip"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root, clobber = 2), my_source)
    res <- bb_sync(cf, confirm_downloads_larger_than = NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp, "example_data.zip")))
    expect_true(file.exists(file.path(fp, "example_data_was_zipped.csv")))
    x <- read.csv(file.path(fp, "example_data_was_zipped.csv"))
    expect_named(x, c("a", "b", "c"))
    ## should also have direct paths to those files in the res$files object
    expect_true(bowerbird:::std_path(file.path(fp, "example_data.zip"), case = TRUE) %in% bowerbird:::std_path(res$files[[1]]$file, case = TRUE))
    expect_true(bowerbird:::std_path(file.path(fp, "example_data_was_zipped.csv"), case = TRUE) %in% bowerbird:::std_path(res$files[[1]]$file, case = TRUE))
})

## same thing, using wget
test_that("decompressing zip files works (wget)",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data.zip",
        license="MIT",
        method=list("bb_handler_wget", recursive = TRUE, level = 1, robots_off = TRUE),
        postprocess=list("bb_unzip"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root, clobber = 2), my_source)
    res <- bb_sync(cf, confirm_downloads_larger_than = NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp, "example_data.zip")))
    expect_true(file.exists(file.path(fp, "example_data_was_zipped.csv")))
    x <- read.csv(file.path(fp, "example_data_was_zipped.csv"))
    expect_named(x, c("a", "b", "c"))
})

test_that("decompressing gzipped files works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data_was_gzipped.csv.gz",
        license="MIT",
        method=list("bb_handler_rget", level = 1),
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

test_that("decompressing tar files works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data_was_tarred.tar",
        license="MIT",
        method=list("bb_handler_rget", level = 1),
        postprocess=list("bb_untar"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data_was_tarred.tar")))
    expect_true(file.exists(file.path(fp,"example_data_was_tarred.csv")))
    x <- read.csv(file.path(fp,"example_data_was_tarred.csv"))
    expect_named(x,c("a","b","c"))
})

test_that("decompressing tgz files works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data_was_tgzed.tgz",
        license="MIT",
        method=list("bb_handler_rget", level = 1),
        postprocess=list("bb_untar"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data_was_tgzed.tgz")))
    expect_true(file.exists(file.path(fp,"example_data_was_tgzed.csv")))
    x <- read.csv(file.path(fp,"example_data_was_tgzed.csv"))
    expect_named(x,c("a","b","c"))
})

test_that("decompressing tar.gz files works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data_was_targzipped.tar.gz",
        license="MIT",
        method=list("bb_handler_rget", level = 1),
        postprocess=list("bb_untar"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data_was_targzipped.tar.gz")))
    expect_true(file.exists(file.path(fp,"example_data_was_targzipped.csv")))
    x <- read.csv(file.path(fp,"example_data_was_targzipped.csv"))
    expect_named(x,c("a","b","c"))
})

test_that("decompressing bzipped files works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data_was_bzipped.csv.bz2",
        license="MIT",
        method=list("bb_handler_rget", level = 1),
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
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/20170822.nc.Z",
        license="MIT",
        method=list("bb_handler_rget", level = 1),
        postprocess=list("bb_uncompress"))

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"20170822.nc.Z")))
    expect_true(file.exists(file.path(fp,"20170822.nc")))
    expect_equal(file.info(file.path(fp,"20170822.nc"))$size,840508)
})

test_that("cleanup postprocessing works",{
    skip_on_cran()
    skip_on_appveyor() ## fails for unknown reasons
    ## unzip then cleanup
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data.zip",
        license="MIT",
        method=list("bb_handler_rget", level = 1),
        postprocess=list("bb_unzip",list("bb_cleanup",pattern="\\.csv$"))
    )
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    res <- bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_false(file.exists(file.path(fp,"example_data_was_zipped.csv")))

    ## unzip then cleanup then unzip again!
    my_source <- bb_source(
        name="Bowerbird test data",
        id="bbtest-v0.1",
        description="These are just some trivial test files provided with the bowerbird package.",
        doc_url="https://github.com/ropensci/bowerbird",
        citation="No citation needed.",
        source_url="https://raw.githubusercontent.com/ropensci/bowerbird/master/inst/extdata/example_data.zip",
        license="MIT",
        method=list("bb_handler_rget", level = 1),
        postprocess=list("bb_unzip", list("bb_cleanup",pattern="\\.csv$"), "bb_unzip")
    )
    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root=temp_root,clobber=2),my_source)
    res <- bb_sync(cf,confirm_downloads_larger_than=NULL)

    fp <- bb_data_source_dir(cf)
    expect_true(file.exists(file.path(fp,"example_data_was_zipped.csv")))

})
