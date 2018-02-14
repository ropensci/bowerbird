library(testthat)
library(bowerbird)

if (.Platform$OS.type=="windows") {
    if (is.null(bb_find_wget(error=FALSE))) {
        warning("At init of testing: could not find wget executable, installing.\n")
        bb_install_wget()
        tmp <- bb_find_wget() ## will throw error if wget not found
    }
}
test_check("bowerbird")
