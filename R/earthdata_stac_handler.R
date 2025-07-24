#' Handler for data sets from Earthdata providers, using the STAC catalogue to do the initial search
#'
#' This is a handler function to be used with data sets from NASA's Earthdata system. This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}.
#' This function finds the items to download via the STAC catalog, and then uses \code{\link{bb_rget}} to download them. Data sources using this function can provide appropriate \code{\link{bb_rget}} parameters if required.
#' The method arguments accepted by \code{bb_handler_aws_s3} are currently:
#' \itemize{
#'   \item "stac_id" string: the STAC identifier
#'   \item "collection_id" string: the collection identifier
#' }
#'
#' @references https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+With+Earthdata+Login
#'
#' @param ... : parameters, see Description
#'
#' @return A tibble with columns \code{ok}, \code{files}, \code{message}
#'
#' @examples
#' \dontrun{
#'
#' my_source <- bb_source(
#'   name = "Nimbus Ice Edge Points from Nimbus Visible Imagery",
#'   id = "10.5067/NIMBUS/NmIcEdg2",
#'   description = "This data set (NmIcEdg2) ... [truncated; see sources_seaice()]",
#'   doc_url = "http://nsidc.org/data/nmicedg2/",
#'   citation = "Gallaher D and Campbell G ... [truncated; see sources_seaice()]",
#'   license = "Please cite, see http://nsidc.org/about/use_copyright.html",
#'   authentication_note = "Requires Earthdata login, see https://urs.earthdata.nasa.gov/.
#'     Note that you will also need to authorize the application 'nsidc-daacdata'
#'     (see 'My Applications' at https://urs.earthdata.nasa.gov/profile)",
#'   method = list("bb_handler_earthdata_stac", stac_id = "NSIDC_CPRD",
#'     collection_id = "NmIcEdg2_1"),
#'   user = "your_earthdata_username",
#'   password = "your_earthdata_password",
#'   collection_size = 0.02)
#' }
#'
#' @export
bb_handler_earthdata_stac <- function(...) {
    bb_handler_earthdata_stac_inner(...)
}


## @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
## @param verbose logical: if TRUE, provide additional progress output
## @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_earthdata_stac_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, stac_id, collection_id, ...) {
    assert_that(is(config, "bb_config"))
    assert_that(nrow(bb_data_sources(config)) == 1)
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(local_dir_only), !is.na(local_dir_only))

    ## see e.g. https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
    ## note preauthorization needed: https://wiki.earthdata.nasa.gov/display/EL/How+To+Pre-authorize+an+application
    ## https://nsidc.org/support/faq/what-options-are-available-bulk-downloading-data-https-earthdata-login-enabled

    if (local_dir_only) {
        ## we need to download the stac catalog to know the source URLs
        ## note that this only uses the first few items and takes their common path as the storage directory, so it might be too specific (first few items all live in a subdirectory of the true output) or even outright wrong (later items live elsewhere)
        st <- rstac::stac(paste0("https://cmr.earthdata.nasa.gov/stac/", stac_id)) %>%
            rstac::stac_search(collections = collection_id) %>%
            rstac::get_request()
        return(fs::path_common(earthdata_get_stac_items_download_urls(st$features)))
    }

    tempds <- bb_data_sources(config)
    if (is.null(tempds$user) || is.null(tempds$password) || na_or_empty(tempds$user) || na_or_empty(tempds$password)) stop("Earthdata source \"", tempds$name, "\" requires user and password")
    cookies_file <- gsub("\\\\", "/", tempfile()) ## probably don't need the gsub, was there for windows debugging
    ## create this file
    if (!file.exists(cookies_file)) cat("", file = cookies_file)
    on.exit(file.remove(cookies_file))
    ## must use --auth-no-challenge (force basic auth) else the server redirects to the html login page, rather than accepting the provided credentials
    ## note that user and password will be substituted from env vars if appropriate inside the bb_handler_rget call
    my_curl_config <- build_curl_config(debug = FALSE, show_progress = FALSE, user = tempds$user, password = tempds$password, enforce_basic_auth = TRUE)
    ## and some more configs specifically for earthdata
    my_curl_config$options$followlocation <- 1
    my_curl_config$options$cookiefile <- cookies_file ## reads cookies from here
    my_curl_config$options$cookiejar <- cookies_file ## saves cookies here

    ## retrieve the item URLS from the STAC catalog and add them to the tempds data source
    if (verbose) cat("Retrieving STAC catalog (will be slow with large collections) ...")
    tempds$source_url <- list(earthdata_get_stac_item_urls(stac_id, collection_id, limit = 1e3))
    if (verbose) cat("done.\n")

    tempds$method[[1]]$stac_id <- tempds$method[[1]]$collection_id <- NULL ## remove these from the method parms being passed to rget
    ## method should already contain target_s3_args if we are uploading to an s3 endpoint, so don't need to do anything further
    bb_data_sources(config) <- tempds
    do.call(bb_handler_rget, Filter(length, c(list(config, verbose = verbose, curl_opts = my_curl_config$options), tempds$method[[1]][-1])))
}

earthdata_get_stac_item_urls <- function(stac_id, collection_id, ...) {
    ## can pass other parms to stac_search via ...
    st <- rstac::stac(paste0("https://cmr.earthdata.nasa.gov/stac/", stac_id)) %>%
        rstac::stac_search(collections = collection_id, ...) %>%
        rstac::post_request() %>% rstac::items_fetch() ## this is slow with large collections, items_fetch gets ALL matched items, paging through results
    earthdata_get_stac_items_download_urls(st$features)
}

earthdata_get_stac_items_download_urls <- function(items) {
    sapply(if (inherits(items, "doc_items")) items$features else items, function(z) unname(unlist(lapply(z$assets, function(ast) if (isTRUE(tolower(ast$title) == "direct download")) ast$href))))
}
