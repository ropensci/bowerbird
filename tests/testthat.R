library(testthat)
library(bowerbird)

if (.Platform$OS.type == "windows") {
    if (is.null(bb_find_wget(error = FALSE))) {
        bb_install_wget()
        tmp <- bb_find_wget() ## will throw error if wget not found
    }
}
test_check("bowerbird")
