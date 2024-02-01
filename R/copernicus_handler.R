#' Handler for Copernicus Marine datasets
#'
#' This is a handler function to be used with data sets from Copernicus Marine. This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}.
#'
#' Note that users will need a Copernicus login.
#'
#' @references https://help.marine.copernicus.eu/en/collections/4060068-copernicus-marine-toolbox
#' @param product string: the desired Copernicus marine product. See \code{\link[CopernicusMarine]{cms_products_list}}
# @param layer string: (optional) the layer within a product. See \code{\link[CopernicusMarine]{cms_product_details}}
#' @param ... : parameters passed to \code{\link{bb_rget}}
#'
#' @return TRUE on success
#'
#' @export
bb_handler_copernicus <- function(product, ...) { ##layer,
    assert_that(is.string(product), nzchar(product))
    ##if (!missing(layer)) {
    ##    if (!is.null(layer)) assert_that(is.string(layer), nzchar(layer))
    ##} else {
    ##    layer <- NULL
    ##}
    do.call(bb_handler_copernicus_inner, list(..., product = product))##, layer = layer))
}


# @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
# @param verbose logical: if TRUE, provide additional progress output
# @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_copernicus_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, product, ...) { ## layer = NULL
    assert_that(is(config, "bb_config"))
    assert_that(nrow(bb_data_sources(config)) == 1)
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(local_dir_only), !is.na(local_dir_only))
    assert_that(is.string(product), nzchar(product))
    ##if (!is.null(layer)) assert_that(is.string(layer), nzchar(layer))
    dots <- list(...)

    thisds <- bb_data_sources(config) ## user and password info will be in here
    this_att <- bb_settings(config)


    if (local_dir_only) {
        ## actual source URLs look like e.g. https://s3.waw3-1.cloudferro.com/mdl-native-07/native/SEALEVEL_GLO_PHY_L4_NRT_008_046/cmems_obs-sl_glo_phy-ssh_nrt_allsat-l4-duacs-0.25deg_P1D_202311/2022/01/nrt_global_allsat_phy_l4_20220101_20220107.nc
        ## but it seems plausible that the server/bucket might change over time
        ## so we will store locally under data.marine.copernicus.eu/<product>/
        return(file.path(this_att$local_file_root, "data.marine.copernicus.eu", product))
    }

##    if (is.null(thisds$user) || is.null(thisds$password) || na_or_empty(thisds$user) || na_or_empty(thisds$password)) stop(sprintf("Copernicus sources require a login: provide your user and password in the source configuration"))
    if (verbose) cat("Downloading file list ... \n")
    myfiles <- CopernicusMarine::cms_list_stac_files(product) ## layer is ignored in this?
    myfiles$url <- paste0("https://", file.path(myfiles$home, myfiles$native, myfiles$current_path))
    ## take the `current_path` of each file and find the product ID
    pidx <- stringr::str_locate(myfiles$current_path, stringr::fixed(paste0(product, "/")))
    ## use the section of `current_path` from the product ID onwards as the local directory under "data.marine.copernicus.eu"
    myfiles$local_filename <- file.path("data.marine.copernicus.eu", vapply(seq_len(nrow(myfiles)), function(i) if (!is.na(pidx[i, 1])) substr(myfiles$current_path[i], pidx[i, 1], 9999) else myfiles$current_path[i], FUN.VALUE = "", USE.NAMES = FALSE)) ## where local copy will go
    myfiles$was_downloaded <- FALSE
    if (verbose) cat("\n", nrow(myfiles), " file", if (nrow(myfiles) > 1) "s", " to download\n", sep = "")
    ## for each file, download if needed and store in appropriate directory
    ok <- TRUE
    my_curl_config <- build_curl_config(debug = FALSE, show_progress = verbose) ##, user = thisds$user, password = thisds$password, enforce_basic_auth = TRUE)
    fidx <- file.exists(myfiles$local_filename)
    myfiles$existing_checksum <- NA_character_
    myfiles$existing_checksum[fidx] <- vapply(myfiles$local_filename[fidx], file_hash, hash = "md5", FUN.VALUE = "", USE.NAMES = FALSE)
    for (idx in seq_len(nrow(myfiles))) {
        this_url <- myfiles$url[idx]
        this_fullfile <- myfiles$local_filename[idx]
        if (!this_att$dry_run) {
            this_exists <- !is.na(myfiles$existing_checksum[idx])
            download_this <- !this_exists
            if (this_att$clobber < 1) {
                ## don't clobber existing
            } else if (this_att$clobber == 1) {
                ## replace existing if server hash does not match that of local copy
                if (this_exists) {
                    download_this <- !isTRUE(myfiles$ETag[idx] == myfiles$checksum[idx])
                }
            } else {
                download_this <- TRUE
            }
            if (download_this) {
                if (verbose) cat("Downloading:", this_url, "... \n")
                if (!dir.exists(dirname(this_fullfile))) dir.create(dirname(this_fullfile), recursive = TRUE)
                myfun <- warning ## if (stop_on_download_error) stop else warning
                req <- httr::with_config(my_curl_config, httr::GET(this_url, httr::write_disk(path = this_fullfile, overwrite = TRUE)))
                if (httr::http_error(req)) {
                    myfun("Error downloading ", this_url, ": ", httr::http_status(req)$message)
                } else {
                    myfiles$was_downloaded[idx] <- TRUE
                }
            } else {
                if (this_exists) {
                    if (verbose) cat("not downloading ", myfiles$filename[idx], ", local copy exists with identical checksum\n", sep = "")
                }
            }
        }
    }
    if (this_att$dry_run && verbose) {
        cat(" dry_run is TRUE, bb_handler_oceandata is not downloading the following files:\n", paste(myfiles$url, collapse="\n "), "\n")
    }
    fls <- myfiles[, c("url", "local_filename", "was_downloaded")]
    names(fls)[2] <- "file"
    tibble(ok = ok, files = list(fls), message = "")
}
