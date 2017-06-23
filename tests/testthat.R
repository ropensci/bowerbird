library(testthat)
library(dplyr)
library(bowerbird)

if (.Platform$OS.type=="windows") {
    tryCatch(wget_exe(),
             error=function(e) {
                 warning("At init of testing: could not find wget executable, installing.\n")
                 install_wget()
             })
}
test_check("bowerbird")
