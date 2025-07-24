#' Handler for Copernicus Marine datasets
#'
#' This is a handler function to be used with data sets from Copernicus Marine. This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}.
#'
#' Note that users will need a Copernicus login.
#'
#' @references https://help.marine.copernicus.eu/en/collections/4060068-copernicus-marine-toolbox
#' @param product string: the desired Copernicus marine product. See \code{\link[CopernicusMarine]{cms_products_list}}
#' @param ctype legacy parameter, ignored
# @param layer string: (optional) the layer within a product. See \code{\link[CopernicusMarine]{cms_product_details}}
#' @param ... : parameters passed to \code{\link{bb_rget}}
#'
#' @return TRUE on success
#'
#' @export
bb_handler_copernicus <- function(product, ctype, ...) { ##layer,
    assert_that(is.string(product), nzchar(product))
    ##if (!missing(layer)) {
    ##    if (!is.null(layer)) assert_that(is.string(layer), nzchar(layer))
    ##} else {
    ##    layer <- NULL
    ##}
    bb_handler_copernicus_inner(..., product = product) ##, layer = layer))
}


## @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
## @param verbose logical: if TRUE, provide additional progress output
## @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_copernicus_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, product, ...) { ## layer = NULL
    assert_that(is(config, "bb_config"))
    assert_that(nrow(bb_data_sources(config)) == 1)
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(local_dir_only), !is.na(local_dir_only))
    assert_that(is.string(product), nzchar(product))
    ##if (!is.null(layer)) assert_that(is.string(layer), nzchar(layer))
    dots <- list(...)

    use_etags <- FALSE ## previously we used the ETag information to decide whether a file had changed since it was last downloaded. The ETags were md5 hashes of each file. But at some point during 2024 it seems that the ETags are not consistently populated as md5 hashes (either this changed, or it was never actually the case for all data sets). So now use last_modified times. The ETag code has been left here for reference temporarily but will be removed at some later date TODO

    ## thisds <- bb_data_sources(config) ## user and password info will be in here, but we don't need it for the copernicus handler
    this_att <- bb_settings(config)


    if (local_dir_only) {
        ## actual source URLs look like e.g. https://s3.waw3-1.cloudferro.com/mdl-native-07/native/SEALEVEL_GLO_PHY_L4_NRT_008_046/cmems_obs-sl_glo_phy-ssh_nrt_allsat-l4-duacs-0.25deg_P1D_202311/2022/01/nrt_global_allsat_phy_l4_20220101_20220107.nc
        ## but it seems plausible that the server/bucket might change over time
        ## so we will store locally under data.marine.copernicus.eu/<product>/
        return(file.path(this_att$local_file_root, "data.marine.copernicus.eu", product))
    }

    if (verbose) cat("Downloading file list ... \n")
    myfiles <- CopernicusMarine::cms_list_native_files(product)
    if (is.null(myfiles) || nrow(myfiles) < 1) stop("No files found for Copernicus Marine product: ", product)
    names(myfiles) <- tolower(names(myfiles))
    if (!all(c("base_url", "bucket", "key", if (use_etags) "etag" else "lastmodified") %in% names(myfiles))) stop("file list does not have the expected columns, has there been a change to the format returned by `CopernicusMarine::cms_list_native_files()`?")
    if (use_etags) myfiles$etag <- sub("^\"", "", sub("\"$", "", myfiles$etag))
    ## "home" now "base_url", "native" is "bucket", "current_path" is now "key"
    ## if (!"url" %in% names(myfiles)) myfiles$url <- paste0("https://", file.path(myfiles$home, myfiles$native, myfiles$current_path))
    if (!"url" %in% names(myfiles)) myfiles$url <- paste0("https://", file.path(myfiles$base_url, myfiles$bucket, myfiles$key))
    ## take the `key` of each file and find the product ID
    pidx <- stringr::str_locate(myfiles$key, stringr::fixed(paste0(product, "/")))
    ## use the section of `key` from the product ID onwards as the local directory under "data.marine.copernicus.eu"
    myfiles$local_filename <- file.path("data.marine.copernicus.eu", vapply(seq_len(nrow(myfiles)), function(i) if (!is.na(pidx[i, 1])) substr(myfiles$key[i], pidx[i, 1], 9999) else myfiles$key[i], FUN.VALUE = "", USE.NAMES = FALSE)) ## where local copy will go
    myfiles$was_downloaded <- FALSE
    if (verbose) cat("\n", nrow(myfiles), " file", if (nrow(myfiles) > 1) "s", " to download\n", sep = "")
    ## for each file, download if needed and store in appropriate directory
    ok <- TRUE
    my_curl_config <- build_curl_config(debug = FALSE, show_progress = verbose)
    if (use_etags) {
        fidx <- file.exists(myfiles$local_filename) & !is.na(myfiles$etag)
        myfiles$existing_checksum <- NA_character_
        myfiles$existing_checksum[fidx] <- vapply(myfiles$local_filename[fidx], file_hash, hash = "md5", FUN.VALUE = "", USE.NAMES = FALSE)
    } else {
        fidx <- file.exists(myfiles$local_filename)
        myfiles$local_last_modified <- as.POSIXct(NA)
        myfiles$local_last_modified[fidx] <- fs::file_info(myfiles$local_filename[fidx])$modification_time
    }
    myfiles$would_actually_download <- FALSE ## only used with dry_run
    for (idx in seq_len(nrow(myfiles))) {
        this_url <- myfiles$url[idx]
        this_fullfile <- myfiles$local_filename[idx]
        this_exists <- if (use_etags) !is.na(myfiles$existing_checksum[idx]) else !is.na(myfiles$local_last_modified[idx])
        download_this <- !this_exists
        if (this_att$clobber < 1) {
            ## don't clobber existing
        } else if (this_att$clobber == 1) {
            if (use_etags) {
                if (!is.na(myfiles$etag[idx])) {
                    ## we have a remote hash, so replace existing if remote hash does not match that of local copy
                    if (this_exists) download_this <- !isTRUE(myfiles$etag[idx] == myfiles$existing_checksum[idx])
                } else {
                    ## no remote hash, so attempt the download and rely on timestamps
                    download_this <- TRUE
                }
            } else {
                ## download unless local copy has a newer timestamp than the remote copy
                ## this is equivalent to no-clobber, but much faster because we won't issue a conditional download request for every file, we are checking modification times first and only requesting downloads of the modified files
                download_this <- !isTRUE(myfiles$local_last_modified[idx] >= myfiles$lastmodified[idx])
            }
        } else {
            download_this <- TRUE
        }
        if (!this_att$dry_run) {
            if (download_this) {
                if (verbose) cat("Downloading:", this_url, "... \n")
                if (!dir.exists(dirname(this_fullfile))) dir.create(dirname(this_fullfile), recursive = TRUE)
                myfun <- warning ## if (stop_on_download_error) stop else warning
                if (!use_etags || !is.na(myfiles$etag[idx])) {
                    req <- httr::with_config(my_curl_config, httr::GET(this_url, httr::write_disk(path = this_fullfile, overwrite = TRUE)))
                    if (httr::http_error(req)) {
                        myfun("Error downloading ", this_url, ": ", httr::http_status(req)$message)
                    } else {
                        myfiles$was_downloaded[idx] <- TRUE
                    }
                } else {
                    ## request with modified-since header so that timestamping check gets applied
                    res <- bb_rget(this_url, force_local_filename = this_fullfile, use_url_directory = FALSE, clobber = this_att$clobber, curl_opts = my_curl_config$options, verbose = verbose)
                    if (!res$ok) {
                        myfun("Error downloading ", this_url, ": ", res$message)
                    } else {
                        myfiles$was_downloaded[idx] <- TRUE
                    }
                }
            } else {
                if (this_exists) {
                    if (verbose) cat("not downloading ", myfiles$local_filename[idx], ", local copy exists", if (use_etags) " with identical checksum", "\n", sep = "")
                }
            }
        } else {
            myfiles$would_actually_download[idx] <- download_this
        }
    }
    if (verbose && this_att$dry_run) {
        cat(" dry_run is TRUE, bb_handler_copernicus is not downloading the following files:\n", paste(myfiles$url[which(myfiles$would_actually_download)], collapse="\n "), "\n")
    }
    fls <- myfiles[, c("url", "local_filename", "was_downloaded")]
    names(fls)[2] <- "file"
    tibble(ok = ok, files = list(fls), message = "")
}
