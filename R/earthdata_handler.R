#' Handler for data sets from Earthdata providers
#'
#' This is a handler function to be used with data sets from NASA's Earthdata system. This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}.
#'
#' This function uses \code{\link{bb_rget}}, and so data sources using this function will need to provide appropriate \code{\link{bb_rget}} parameters. Note that curl v5.2.1 introduced a breaking change to the default value of the `unrestricted_auth` option: see <https://github.com/jeroen/curl/issues/260>. Your Earthdata source definition might require `allow_unrestricted_auth = TRUE` as part of the method parameters.
#'
#' @references https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+With+Earthdata+Login
#' @param ... : parameters passed to \code{\link{bb_rget}}
#'
#' @return TRUE on success
#'
#' @examples
#' \dontrun{
#'
#' ## note that the full version of this data source is provided as part of bb_example_data_sources()
#'
#' my_source <- bb_source(
#'   name = "Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 3",
#'   id = "10.5067/IJ0T7HFHB9Y6",
#'   description = "NSIDC provides this data set ... [truncated; see bb_example_data_sources()]",
#'   doc_url = "https://nsidc.org/data/NSIDC-0192/versions/3",
#'   citation = "Stroeve J, Meier WN (2018) ... [truncated; see bb_example_data_sources()]",
#'   source_url = "https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0192_seaice_trends_climo_v3/",
#'   license = "Please cite, see http://nsidc.org/about/use_copyright.html",
#'   authentication_note = "Requires Earthdata login, see https://urs.earthdata.nasa.gov/.
#'     Note that you will also need to authorize the application 'nsidc-daacdata'
#'     (see 'My Applications' at https://urs.earthdata.nasa.gov/profile)",
#'   method = list("bb_handler_earthdata", level = 4, relative = TRUE,
#'                 accept_download = "\\.(s|n|png|txt)$", allow_unrestricted_auth = TRUE),
#'   user = "your_earthdata_username",
#'   password = "your_earthdata_password",
#'   collection_size = 0.02)
#' }
#'
#' @export
bb_handler_earthdata <- function(...) {
    bb_handler_earthdata_inner(...)
}


## @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
## @param verbose logical: if TRUE, provide additional progress output
## @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
## @param use_wget logical: TRUE use wget (deprecated), FALSE use rget
bb_handler_earthdata_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, use_wget = FALSE, allow_unrestricted_auth, ...) {
    assert_that(is(config, "bb_config"))
    assert_that(nrow(bb_data_sources(config)) == 1)
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(local_dir_only), !is.na(local_dir_only))
    assert_that(is.flag(use_wget), !is.na(use_wget))

    ## see e.g. https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
    ## note preauthorization needed: https://wiki.earthdata.nasa.gov/display/EL/How+To+Pre-authorize+an+application
    ## https://nsidc.org/support/faq/what-options-are-available-bulk-downloading-data-https-earthdata-login-enabled

    if (local_dir_only) {
        if (use_wget) {
            return(bb_handler_wget(config, verbose = verbose, local_dir_only = TRUE, ...))
        } else {
            return(bb_handler_rget(config, verbose = verbose, local_dir_only = TRUE, ...))
        }
    }

    dummy <- bb_data_sources(config)
    if (is.null(dummy$user) || is.null(dummy$password) || na_or_empty(dummy$user) || na_or_empty(dummy$password)) stop("Earthdata source \"", dummy$name, "\" requires user and password")
    cookies_file <- gsub("\\\\", "/", tempfile()) ## probably don't need the gsub, was there for windows debugging
    ## create this file
    if (!file.exists(cookies_file)) cat("", file = cookies_file)
    on.exit(file.remove(cookies_file))
    ## must use --auth-no-challenge (force basic auth) else the server redirects to the html login page, rather than accepting the provided credentials
    if (use_wget) {
        ## potentially substitute username and password from env vars
        dummy$user <- use_secret(dummy$user)
        dummy$password <- use_secret(dummy$password)
        dummy$method <- list(list("bb_handler_wget", ..., extra_flags = c("--http-user", dummy$user, "--http-password", dummy$password, "--auth-no-challenge", "--load-cookies", cookies_file, "--save-cookies", cookies_file, "--keep-session-cookies"), reject = "index.html*", robots_off = TRUE))
        dummy$user <- NA_character_
        dummy$password <- NA_character_
        bb_data_sources(config) <- dummy
        ## must make the wget call twice: first time it will authenticate, and write the cookies, but then redirect to the original page and wget won't go further because it knows it's already been there and doesn't want to get into an infinite loop
        do.call(bb_handler_wget, c(list(config, verbose = verbose), dummy$method[[1]][-1]))
        ## but the second time it will authenticate using the stored cookie and proceed with the recursion
        do.call(bb_handler_wget, c(list(config, verbose = verbose), dummy$method[[1]][-1]))
    } else {
        if (missing(allow_unrestricted_auth) || is.null(allow_unrestricted_auth)) {
            allow_unrestricted_auth <- FALSE
            warning("if your download does not find the files you are expecting, try adding `allow_unrestricted_auth = TRUE` to the method parameters. This is necessary for Earthdata servers that serve data from a different hostname to the landing hostname")
        }
        ## note that user and password will be substituted from env vars if appropriate inside the bb_handler_rget call
        my_curl_config <- build_curl_config(debug = FALSE, show_progress = FALSE, user = dummy$user, password = dummy$password, enforce_basic_auth = TRUE)
        ## and some more configs specifically for earthdata
        my_curl_config$options$followlocation <- 1
        my_curl_config$options$cookiefile <- cookies_file ## reads cookies from here
        my_curl_config$options$cookiejar <- cookies_file ## saves cookies here
        if (isTRUE(allow_unrestricted_auth)) my_curl_config$options$unrestricted_auth <- 1L ## prior to curl 5.2.1 this was the default, and without it the authentication won't be properly passed to earthdata servers that serve data from a different hostname to the landing hostname
        ## but we make this something that the source has to specifically set, because it's a security risk: https://curl.se/libcurl/c/CURLOPT_UNRESTRICTED_AUTH.html
        dummy$method[[1]]$allow_unrestricted_auth <- NULL ## remove this from the method parms being passed to rget
        ## method should already contain target_s3_args if we are uploading to an s3 endpoint, so don't need to do anything further
        do.call(bb_handler_rget, c(list(config, verbose = verbose, curl_opts = my_curl_config$options), dummy$method[[1]][-1]))
    }
}
