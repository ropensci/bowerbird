context("rget/httr")

test_that("authentication works",{
    skip_on_cran()
    res <- httr::GET("http://httpbin.org/basic-auth/user/passwd")
    expect_true(httr::http_error(res))
    ccf <- bowerbird:::build_curl_config(user = "user", password = "passwd", enforce_basic_auth = TRUE)
    res <- httr::with_config(ccf, httr::GET("http://httpbin.org/basic-auth/user/passwd"))
    expect_equal(res$status_code, 200)
})
