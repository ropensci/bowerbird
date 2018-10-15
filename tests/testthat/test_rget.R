context("rget/httr")

test_that("authentication works",{
    skip_on_cran()
    skip_on_travis() ## this randomly fails on travis, don't know why
    res <- httr::GET("http://httpbin.org/basic-auth/user/passwd")
    expect_true(httr::http_error(res))
    ccf <- bowerbird:::build_curl_config(user = "user", password = "passwd", enforce_basic_auth = TRUE)
    res <- httr::with_config(ccf, httr::GET("http://httpbin.org/basic-auth/user/passwd"))
    expect_equal(res$status_code, 200)
})

test_that("rget's remote_time option works",{
    skip_on_cran()
    skip_on_travis()

    test_url <- "https://www.google.com.au/robots.txt"
    ccf <- bowerbird:::build_curl_config(remote_time = FALSE)
    f1 <- tempfile()
    res <- httr::with_config(ccf, httr::GET(test_url, write_disk(path = f1, overwrite = TRUE)))
    ccf <- bowerbird:::build_curl_config(remote_time = TRUE)
    f2 <- tempfile()
    res <- httr::with_config(ccf, httr::GET(test_url, write_disk(path = f2, overwrite = TRUE)))
    finf1 <- file.info(c(f1, f2))
    ## don't expect that local file timestamp will have been modified here, because curl doesn't seem to honour the filetime option?
    expect_true(abs(as.numeric(difftime(finf1$mtime[1], finf1$mtime[2], units = "secs"))) < 10) ## less than 10s
    ## so we set the file time manually
    bowerbird:::set_file_timestamp(f2, headers(res))
    finf2 <- file.info(c(f1, f2))
    expect_true(as.numeric(difftime(finf2$mtime[2], finf2$mtime[1], units = "secs")) < -10) ## less than -10, i.e. remote time stamp is more than 10s earlier than now

    ## and same deal, handled within the rget function
    test_url <- "https://www.google.com.au/robots.txt"
    withdir <- function(cwd = getwd()) {
        on.exit(setwd(cwd))
        setwd(tempdir())
        res <- bb_rget(test_url, force_local_filename = "f1.txt", remote_time = FALSE)
        finf1 <- file.info(res$files[[1]]$file[1])
        unlink(res$files[[1]]$file[1])
        res <- bb_rget(test_url, force_local_filename = "f2.txt", remote_time = TRUE)
        finf2 <- file.info(res$files[[1]]$file[1])
        unlink(res$files[[1]]$file[1])
        c(finf1$mtime, finf2$mtime)
    }
    tms <- withdir()
    expect_true(as.numeric(difftime(tms[2], tms[1], units = "secs")) < -10) ## less than -10, i.e. remote time stamp is more than 10s earlier than now
})

test_that("rget use_url_directory parameter works", {
    skip_on_cran()
    test_url <- "https://www.google.com.au/robots.txt"
    outdir <- tempdir()
    if (file.exists(file.path(outdir, "robots.txt"))) file.remove(file.path(outdir, "robots.txt"))
    withdir <- function(cwd = getwd()) {
        on.exit(setwd(cwd))
        setwd(outdir)
        bb_rget(test_url, use_url_directory = FALSE)
    }
    res <- withdir()
    expect_true(file.exists(file.path(outdir, "robots.txt")))
})
