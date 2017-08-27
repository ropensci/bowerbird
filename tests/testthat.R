library(testthat)
library(dplyr)
library(bowerbird)

if (.Platform$OS.type=="windows") {
    if (is.null(bb_find_wget())) {
        warning("At init of testing: could not find wget executable, installing.\n")
        bb_install_wget()
        if (is.null(bb_find_wget())) stop("could not install wget execitable")
    }
}
test_check("bowerbird")
