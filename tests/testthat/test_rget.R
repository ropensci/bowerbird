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
    res <- httr::with_config(ccf, httr::GET(test_url, httr::write_disk(path = f1, overwrite = TRUE)))
    ccf <- bowerbird:::build_curl_config(remote_time = TRUE)
    f2 <- tempfile()
    res <- httr::with_config(ccf, httr::GET(test_url, httr::write_disk(path = f2, overwrite = TRUE)))
    finf1 <- file.info(c(f1, f2))
    ## don't expect that local file timestamp will have been modified here, because curl doesn't seem to honour the filetime option?
    expect_true(abs(as.numeric(difftime(finf1$mtime[1], finf1$mtime[2], units = "secs"))) < 10) ## less than 10s
    ## so we set the file time manually
    bowerbird:::set_file_timestamp(f2, httr::headers(res))
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

test_that("rget works with multiple input URLs", {
    expect_error(bb_rget(c("http://blah", "ftp://hah")), "bb_rget can't handle a mixture of ftp/http")
    res1 <- bb_rget("https://github.com/ropensci/bowerbird/", accept_download = "\\.md$", no_parent = FALSE, level = 1)
    res2 <- bb_rget(c("https://github.com/ropensci/bowerbird/", "https://github.com/ropensci/bowerbird/"), accept_download = "\\.md$", no_parent = FALSE, level = 1)
    expect_identical(res1, res2)

    ## multiple urls with multiple force_local_filenames
    outdir <- tempfile()
    dir.create(outdir)
    res <- bb_rget(rep("https://raw.githubusercontent.com/ropensci/bowerbird/master/README.md", 2), force_local_filename = file.path(outdir, c("1.md", "2.md")), use_url_directory = FALSE)
    expect_identical(res$files[[1]]$file, file.path(outdir, c("1.md", "2.md")))
    expect_true(all(file.exists(res$files[[1]]$file)))

    cwd <- getwd()
    outdir <- tempfile()
    dir.create(outdir)
    setwd(outdir)
    res <- bb_rget(rep("https://raw.githubusercontent.com/ropensci/bowerbird/master/README.md", 2), force_local_filename = c("1.md", "2.md"))
    ## should save into directory given by url structure, but named by 1.md and 2.md
    setwd(cwd)
    ## entries in res$files will not be named with the absolute path, just relative to outdir
    expect_identical(res$files[[1]]$file, file.path("raw.githubusercontent.com/ropensci/bowerbird/master", c("1.md", "2.md")))
    expect_true(all(file.exists(file.path(outdir, res$files[[1]]$file))))
})

test_that("env vars are substituted correctly on windows", {
    skip_on_os(c("mac", "linux", "solaris"))
    expect_identical(use_secret("bilbobaggins"), "bilbobaggins") ## no match
    Sys.setenv(FrodoBaggins = "ABC123")
    expect_identical(use_secret("FrodoBaggins"), "ABC123") ## match
    ## note that windows environment variables are not case sensitive, but linux/mac are
    expect_identical(use_secret("frodobaggins"), "ABC123") ## match, case insensitive
    expect_identical(use_secret(c("bilbobaggins", "FrodoBaggins", "frodobaggins", "", NA_character_)), c("bilbobaggins", "ABC123", "ABC123", "", NA_character_)) ## multiple
    Sys.unsetenv("FrodoBaggins")
})

test_that("env vars are substituted correctly on non-windows", {
    skip_on_os("windows")
    expect_identical(use_secret("bilbobaggins"), "bilbobaggins") ## no match
    Sys.setenv(FrodoBaggins = "ABC123")
    expect_identical(use_secret("FrodoBaggins"), "ABC123") ## match
    expect_identical(use_secret("frodobaggins"), "frodobaggins") ## no match, case sensitive
    expect_identical(use_secret(c("bilbobaggins", "FrodoBaggins", "frodobaggins", "", NA_character_)), c("bilbobaggins", "ABC123", "frodobaggins", "", NA_character_)) ## multiple
    Sys.unsetenv("FrodoBaggins")
})
