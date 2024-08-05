#' Handler for Copernicus Marine datasets
#'
#' This is a handler function to be used with data sets from Copernicus Marine. This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}.
#'
#' Note that users will need a Copernicus login.
#'
#' @references https://help.marine.copernicus.eu/en/collections/4060068-copernicus-marine-toolbox
#' @param product string: the desired Copernicus marine product. See \code{\link[CopernicusMarine]{cms_products_list}}
#' @param ctype string: most likely "stac" for a dataset containing multiple files, or "file" for a single file
# @param layer string: (optional) the layer within a product. See \code{\link[CopernicusMarine]{cms_product_details}}
#' @param ... : parameters passed to \code{\link{bb_rget}}
#'
#' @return TRUE on success
#'
#' @export
bb_handler_copernicus <- function(product, ctype = "stac", ...) { ##layer,
    assert_that(is.string(product), nzchar(product))
    assert_that(is.string(ctype), nzchar(ctype))
    ##if (!missing(layer)) {
    ##    if (!is.null(layer)) assert_that(is.string(layer), nzchar(layer))
    ##} else {
    ##    layer <- NULL
    ##}
    bb_handler_copernicus_inner(..., product = product, ctype = ctype) ##, layer = layer))
}


## @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
## @param verbose logical: if TRUE, provide additional progress output
## @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_copernicus_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, product, ctype, ...) { ## layer = NULL
    assert_that(is(config, "bb_config"))
    assert_that(nrow(bb_data_sources(config)) == 1)
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(local_dir_only), !is.na(local_dir_only))
    assert_that(is.string(product), nzchar(product))
    assert_that(is.string(ctype))
    ctype <- match.arg(ctype, c("stac", "file"))
    ##if (!is.null(layer)) assert_that(is.string(layer), nzchar(layer))
    dots <- list(...)

    ## thisds <- bb_data_sources(config) ## user and password info will be in here, but we don't need it for the copernicus handler
    this_att <- bb_settings(config)


    if (local_dir_only) {
        ## actual source URLs look like e.g. https://s3.waw3-1.cloudferro.com/mdl-native-07/native/SEALEVEL_GLO_PHY_L4_NRT_008_046/cmems_obs-sl_glo_phy-ssh_nrt_allsat-l4-duacs-0.25deg_P1D_202311/2022/01/nrt_global_allsat_phy_l4_20220101_20220107.nc
        ## but it seems plausible that the server/bucket might change over time
        ## so we will store locally under data.marine.copernicus.eu/<product>/
        return(file.path(this_att$local_file_root, "data.marine.copernicus.eu", product))
    }

    if (verbose) cat("Downloading file list ... \n")
    myfiles <- if (ctype == "file") cms_list_stac_single_file(product) else CopernicusMarine::cms_list_stac_files(product) ## layer is ignored in this?
    if (nrow(myfiles) < 1) stop("No files found for Copernicus Marine product: ", product)
    if (!all(c("home", "native", "current_path", "ETag") %in% names(myfiles))) stop("file list does not have the expected columns, has there been a change to the format returned by `CopernicusMarine::cms_list_stac_files()`?")
    myfiles$ETag <- sub("^\"", "", sub("\"$", "", myfiles$ETag))
    if (!"url" %in% names(myfiles)) myfiles$url <- paste0("https://", file.path(myfiles$home, myfiles$native, myfiles$current_path))
    ## take the `current_path` of each file and find the product ID
    pidx <- stringr::str_locate(myfiles$current_path, stringr::fixed(paste0(product, "/")))
    ## use the section of `current_path` from the product ID onwards as the local directory under "data.marine.copernicus.eu"
    myfiles$local_filename <- file.path("data.marine.copernicus.eu", vapply(seq_len(nrow(myfiles)), function(i) if (!is.na(pidx[i, 1])) substr(myfiles$current_path[i], pidx[i, 1], 9999) else myfiles$current_path[i], FUN.VALUE = "", USE.NAMES = FALSE)) ## where local copy will go
    myfiles$was_downloaded <- FALSE
    if (verbose) cat("\n", nrow(myfiles), " file", if (nrow(myfiles) > 1) "s", " to download\n", sep = "")
    ## for each file, download if needed and store in appropriate directory
    ok <- TRUE
    my_curl_config <- build_curl_config(debug = FALSE, show_progress = verbose)
    fidx <- file.exists(myfiles$local_filename) & !is.na(myfiles$ETag)
    myfiles$existing_checksum <- NA_character_
    myfiles$existing_checksum[fidx] <- vapply(myfiles$local_filename[fidx], file_hash, hash = "md5", FUN.VALUE = "", USE.NAMES = FALSE)
    myfiles$would_actually_download <- FALSE ## only used with dry_run
    for (idx in seq_len(nrow(myfiles))) {
        this_url <- myfiles$url[idx]
        this_fullfile <- myfiles$local_filename[idx]
        this_exists <- !is.na(myfiles$existing_checksum[idx])
        download_this <- !this_exists
        if (this_att$clobber < 1) {
            ## don't clobber existing
        } else if (this_att$clobber == 1) {
            if (!is.na(myfiles$ETag[idx])) {
                ## we have a remote hash, so replace existing if remote hash does not match that of local copy
                if (this_exists) {
                    download_this <- !isTRUE(myfiles$ETag[idx] == myfiles$existing_checksum[idx])
                }
            } else {
                ## no remote hash, so attempt the download and rely on timestamps
                download_this <- TRUE
            }
        } else {
            download_this <- TRUE
        }
        if (!this_att$dry_run) {
            if (download_this) {
                if (verbose) cat("Downloading:", this_url, "... \n")
                if (!dir.exists(dirname(this_fullfile))) dir.create(dirname(this_fullfile), recursive = TRUE)
                myfun <- warning ## if (stop_on_download_error) stop else warning
                if (!is.na(myfiles$ETag[idx])) {
                    req <- httr::with_config(my_curl_config, httr::GET(this_url, httr::write_disk(path = this_fullfile, overwrite = TRUE)))
                    if (httr::http_error(req)) {
                        myfun("Error downloading ", this_url, ": ", httr::http_status(req)$message)
                    } else {
                        myfiles$was_downloaded[idx] <- TRUE
                    }
                } else {
                    res <- bb_rget(this_url, force_local_filename = this_fullfile, use_url_directory = FALSE, clobber = this_att$clobber, curl_opts = my_curl_config$options, verbose = verbose)
                    if (!res$ok) {
                        myfun("Error downloading ", this_url, ": ", res$message)
                    } else {
                        myfiles$was_downloaded[idx] <- TRUE
                    }
                }
            } else {
                if (this_exists) {
                    if (verbose) cat("not downloading ", myfiles$filename[idx], ", local copy exists with identical checksum\n", sep = "")
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


## modified version of cms_list_stac_files

## workaround for single-layer datasets: cms_list_stac_files doesn't work because the cms_stac_properties href points to the actual file, not to its bucket
## our options are to truncate to the bucket level and issue the list-bucket request (which gives us the file's ETag)
## OR use the cms_stac_properties href as-is, without the ETag BUT this doesn't work for the MDT data set because the cms_stac_properties href is incorrect! (listing the bucket gives a different/correct one)
cms_list_stac_single_file <- function(product, layer) {
    method <- "list-bucket" ## or "url-as-is", see notes above
    props <- CopernicusMarine::cms_stac_properties(product, layer)
    if (length(props) == 0) return(NULL)
    assets <- props$href
    split <- strsplit(assets, "/")[[1]]
    if (method == "list-bucket") {
        ## to do the list-bucket
        if (length(split) > 7) split <- split[1:7] ## for a single-layer response, the 8th element will be an actual file
        assets <- paste(split, collapse = "/")
    }
    props <- tibble(current_path = gsub("^/", "", stringr::str_extract(assets, "/native/.*?$")), home = split[3], native = split[grepl("-native-", split)])
    if (method == "url-as-is") {
        props$url <- assets
        props$ETag <- NA_character_
        return(props)
    }

    .list_stac <- function(base_props) {
        prep_url <- sub("/+$", "/", paste0("https://", base_props$native, ".", base_props$home, "/?delimiter=%2F&list-type=2&prefix=", utils::URLencode(base_props$current_path), "/"))
        result <- httr::GET(prep_url)
        result <- xml2::as_list(httr::content(result, as = "parsed", type = "application/xml", encoding = "UTF-8"))
        result <- result$ListBucketResult
        c_prefix <- tibble(Key = unname(unlist(result[names(result) == "CommonPrefixes"]))) ## should be empty for a single-layer dataset
        content <- result$Contents
        content <- tibble::as_tibble(do.call(cbind, lapply(content, unlist)))
        bucket <- rbind(c_prefix, content)
        if (nrow(bucket) != 1 || !"Size" %in% names(bucket)) stop("copernicus 'file' data source has not returned a single file")
        if (!"Key" %in% names(bucket)) stop("copernicus 'file' data source has unexpected format")
        bucket$home <- base_props$home
        bucket$native <- base_props$native
        names(bucket)[names(bucket) == "Key"] <- "current_path"
        bucket
    }
    .list_stac(props)
}
