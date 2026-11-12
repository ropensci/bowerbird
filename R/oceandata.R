#' Handler for Oceandata data sets
#'
#' This is a handler function to be used with data sets from NASA's Oceandata system. This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}.
#'
#' Note that users will need an Earthdata login, see \url{https://urs.earthdata.nasa.gov/}. Users will also need to authorize the application 'OB.DAAC Data Access' (see 'My Applications' at \url{https://urs.earthdata.nasa.gov/profile}). Dry runs (listing files but not downloading them) can be performed without authentication.
#'
#' Previously it was possible to search based on filename pattern alone, e.g. "S*L3m_MO_CHL_chlor_a_9km.nc" would match monthly level-3 mapped chlorophyll data from the SeaWiFS satellite at 9km resolution, in netcdf format. However, as of June 2026 the OB.DAAC search API requires other parameters to be set, in order to reduce the search load on the server, see \url{https://www.earthdata.nasa.gov/data/alerts-outages/ob.daac-file-search-utility-now-requires-more-specificity}. It is not obvious which or how many parameters are required for a given search. If you are constructing an oceandata bowerbird source you are advised to use the file search web page \url{https://oceandata.sci.gsfc.nasa.gov/api/file_search} and its help page \url{https://oceandata.sci.gsfc.nasa.gov/file_search/file_search_help/} to help find the correct parameters.
#'
#' It is possible to provide multiple values for one or more search parameters: the POST request to the file search API will be repeated for all possible combinations of parameters (constructed via \code{expand.grid}). This might be helpful, for example, by allowing \code{dtid} to be specified as a two-element vector with one value for refined data and the second for NRT data. Specifying two \code{dtid} values will yield a much faster search than omitting \code{dtid} and relying on the \code{search} parameter to achieve the same refined/NRT file inclusions.
#'
#' Note that the \code{bb_handler_oceandata} does not take need `source_url` to be specified in the \code{bb_source} call.
#'
#' @references \url{https://oceandata.sci.gsfc.nasa.gov/}, \url{https://oceandata.sci.gsfc.nasa.gov/file_search/file_search_help/}, \url{https://www.earthdata.nasa.gov/data/alerts-outages/ob.daac-file-search-utility-now-requires-more-specificity}
#'
#' @param search character: the search string (file name pattern). Note that relying on the search string tends to yield slower searches: specify the target files as tightly as possible using the other search parameters. But you might still need the search string to impose additional filtering on the results
#' @param sensor character: the sensor, e.g. "SeaWiFS", "MODISA", "MODIST", "VIIRS", "CZCS", "MERIS", etc: see \url{https://oceandata.sci.gsfc.nasa.gov/file_search/file_search_help/}
#' @param dtype character: the data type (e.g. "L3m") to pass to the oceancolor file searcher. Valid options at the time of writing are L0, L1, L2, L3b (for binned data), L3m (for mapped data), MET (for ancillary data), misc (for sundry products)
#' @param dtid integer: data type ID, see \url{https://oceandata.sci.gsfc.nasa.gov/file_search/file_search_help/}. Note that providing the \code{dtid} typically gives a much faster search
#' @param period character: the time period, one of "DAY", (daily) "8D" (8-day), "MO", (monthly) "MC" (monthly climatology), "YR" (yearly), "SN" (seasonal), "SC" (seasonal climatology), etc: see \url{https://oceandata.sci.gsfc.nasa.gov/file_search/file_search_help/}
#' @param resolution_id character: "9km" or "4km"
#' @param prod_id character: product, e.g. "par", "chlor_a", etc: see \url{https://oceandata.sci.gsfc.nasa.gov/file_search/file_search_help/}
#' @param suite_id character: product suite, e.g. "RRS", etc: see \url{https://oceandata.sci.gsfc.nasa.gov/file_search/file_search_help/}
#' @param sdate Date or character: start date (in "YYYY-MM-DD" format, if provided as a string)
#' @param edate Date or character: end date (in "YYYY-MM-DD" format, if provided as a string). If not specified it will default to the current date
#' @param ... : extra parameters passed automatically by \code{bb_sync}
#'
#' @return TRUE on success
#'
#' @examples
#'
#' my_source <- bb_source(
#'   name = "Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a",
#'   id = "SeaWiFS_L3m_MO_CHL_chlor_a_9km",
#'   description = "Monthly remote-sensing chlorophyll-a from the SeaWiFS satellite at
#'     9km spatial resolution",
#'   doc_url = "https://oceancolor.gsfc.nasa.gov/",
#'   citation = "See https://oceancolor.gsfc.nasa.gov/citations",
#'   license = "Please cite",
#'   method = list("bb_handler_oceandata", sensor = "SeaWiFS", dtype = "L3m",
#'                 dtid = 1123, period = "MO", resolution_id = "9km", prod_id = "chlor_a",
#'                 sdate = "1997-09-01", edate = "2011-01-01"),
#'   postprocess = NULL,
#'   collection_size = 7.2,
#'   user = "USERNAME", password = "PASSWORD",
#'   data_group = "Ocean colour")
#'
#' @export
bb_handler_oceandata <- function(search, sensor, dtype, dtid, period, resolution_id, prod_id, suite_id, sdate, edate, ...) {
    if (missing(search)) search <- NULL
    if (!is.null(search)) assert_that(is.character(search), all(nzchar(search)))
    if (missing(dtype)) dtype <- NULL
    if (!is.null(dtype)) assert_that(is.character(dtype), all(nzchar(dtype)))
    if (missing(sensor)) sensor <- NULL
    if (!is.null(sensor)) assert_that(is.character(sensor), all(nzchar(sensor)))
    if (missing(period)) period <- NULL
    if (!is.null(period)) assert_that(is.character(period), all(nzchar(period)))
    if (missing(resolution_id)) resolution_id <- NULL
    if (!is.null(resolution_id)) assert_that(is.character(resolution_id), all(nzchar(resolution_id)))
    if (missing(prod_id)) prod_id <- NULL
    if (!is.null(prod_id)) assert_that(is.character(prod_id), all(nzchar(prod_id)))
    if (missing(suite_id)) suite_id <- NULL
    if (!is.null(suite_id)) assert_that(is.character(suite_id), all(nzchar(suite_id)))
    if (missing(dtid)) dtid <- NULL
    if (!is.null(dtid)) assert_that(is.numeric(dtid), !any(is.na(dtid)))
    if (missing(sdate)) sdate <- NULL
    if (!is.null(sdate)) assert_that(is.character(sdate), all(nzchar(sdate)))
    if (missing(edate)) edate <- format(Sys.Date() + 40, "%Y-%m-%d") ## edate seems to be required, so insert today's date plus a bit if not provided by the user
    if (!is.null(edate)) assert_that(is.character(edate), all(nzchar(edate)))
    do.call(bb_handler_oceandata_inner, list(..., search = search, dtype = dtype, sensor = sensor, period = period, resolution_id = resolution_id, prod_id = prod_id, suite_id = suite_id, dtid = dtid, sdate = sdate, edate = edate))
}


# @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
# @param verbose logical: if TRUE, provide additional progress output
# @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_oceandata_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, search, dtype = NULL, sensor = NULL, period = NULL, resolution_id = NULL, prod_id = NULL, suite_id = NULL, dtid = NULL, sdate = NULL, edate = NULL, stop_on_download_error = FALSE, target_s3_args, ...) {
    ## oceandata synchronization handler

    ## oceandata provides a file search interface, e.g.:
    ## wget -q --post-data="cksum=1&search=A2002*DAY_CHL_chlor*9km*" -O - https://oceandata.sci.gsfc.nasa.gov/api/file_search
    ## wget -q --post-data="cksum=1&search=S*L3m_MO_CHL_chlor_a_9km.nc" -O - https://oceandata.sci.gsfc.nasa.gov/api/file_search
    ## or
    ## wget -q --post-data="dtype=L3b&cksum=1&search=A2014*DAY_CHL.*" -O - https://oceandata.sci.gsfc.nasa.gov/api/file_search
    ## returns list of files and SHA1 checksum for each file
    ## each file can be retrieved from https://oceandata.sci.gsfc.nasa.gov/ob/getfile/filename

    ## expect that config$data_sources$method list will contain the search and dtype components of the post string
    ##  i.e. "search=...&dtype=..." in "dtype=L3m&addurl=1&results_as_file=1&search=A2002*DAY_CHL_chlor*9km*"
    ##  or just include the data type in the search pattern e.g. "search=A2002*L3m_DAY_CHL_chlor*9km*

    assert_that(is(config, "bb_config"))
    assert_that(nrow(bb_data_sources(config)) == 1)
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(local_dir_only), !is.na(local_dir_only))
    if (!is.null(search)) assert_that(is.string(search), nzchar(search))
    if (!is.null(dtype)) assert_that(is.string(dtype), nzchar(dtype))
    if (!is.null(sensor)) assert_that(is.string(sensor), nzchar(sensor))
    assert_that(is.flag(stop_on_download_error), !is.na(stop_on_download_error))
    dots <- list(...)

    thisds <- bb_data_sources(config) ## user and password info will be in here
    this_att <- bb_settings(config)

    ## handle target_s3_args
    if (missing(target_s3_args) || is.null(target_s3_args)) target_s3_args <- list()
    if ("target_s3_args" %in% names(this_att)) target_s3_args <- c(target_s3_args, this_att$target_s3_args)
    ## are we syncing to an s3 target?
    s3_target <- is_s3_target(target_s3_args)
    if (s3_target) target_s3_args <- check_s3_args(target_s3_args)


    if (local_dir_only) {
        if (s3_target) {
            ## if we are syncing to s3, then there is no concept of the target directory beyond the bucket. Objects in the bucket can be named with directory-like prefixes, but there are no directories in an s3 bucket
            return(get_aws_s3_url(bucket = target_s3_args$bucket, region = target_s3_args$region, base_url = target_s3_args$base_url, path = ""))
        }
        ## note that we can't use url_mapper here, because the source definition is unlikely to conform to the expected full pattern
        ## also, a given source might include products across multiple fine-resolution directories: the result here will be the lowest level output directory that is common to all files associated with the source
        ## highest-level dir
        out <- "" ## optional prefix with hostname, below
        ## refine by platform. Find platform, either full or one-letter abbrev
        trunc <- FALSE ## if set to TRUE, truncate the output at this point
        if (!is.null(sensor)) {
            out <- file.path(out, sensor)
        } else {
            this_platform <- oceandata_find_platform(search)
            if (!is.na(this_platform) && nzchar(this_platform)) {
                ## want old platform name for backwards compatibility
                if (!this_platform %in% oceandata_platform_map()$platform) {
                    this_platform <- oceandata_platform_map(oceandata_platform_to_abbrev(this_platform))
                }
                out <- file.path(out, this_platform)
            } else {
                trunc <- TRUE
            }
        }
        if (!trunc) {
            if (isTRUE(dtype == "L3m") || grepl("L3m", search)) {
                out <- file.path(out, "Mapped")
            } else if (isTRUE(dtype == "L3b") || grepl("L3", search)) {
                out <- file.path(out, "L3BIN")
            } else {
                trunc <- TRUE
            }
        }
        if (!trunc) {
            ## time period, if it's specified in the search string
            if (!is.null(period)) {
                if (all(period %in% oceandata_alltp$abbrev)) {
                    chk <- unique(oceandata_alltp$time_period[oceandata_alltp$abbrev %in% period])
                    if (length(chk) == 1) {
                        out <- file.path(out, chk)
                    } else {
                        trunc <- TRUE
                    }
                } else {
                    trunc <- TRUE
                }
            } else {
                tp <- stringr::str_detect(search, paste0("\\.", oceandata_alltp$abbrev, "\\."))
                if (sum(tp, na.rm = TRUE) == 1) {
                    out <- file.path(out, oceandata_alltp$time_period[which(tp)])
                } else {
                    trunc <- TRUE
                }
            }
        }
        if (!trunc && (isTRUE(dtype == "L3m") || grepl("L3m", search))) {
            ## spatial res
            if (!is.null(resolution_id)) {
                out <- file.path(out, resolution_id)
            } else {
                if (grepl("4km", search)) out <- file.path(out, "4km") else if (grepl("9km", search)) out <- file.path(out, "9km") else trunc <- TRUE
            }
        }
        if (!trunc) {
            ## next level down is parameter name
            ptbl <- oceandata_parameters()
            if (!is.null(prod_id)) {
                dest_p <- NULL
                pn <- stringr::str_detect(prod_id, paste0("^", ptbl$prod_id, "$"))
                if (sum(pn, na.rm = TRUE) > 0) {
                    dest_p <- unique(ptbl$parameter[which(pn)])
                    ## we can match on multiple parameters so long as they all map to the same parameter directory name
                }
                out <- file.path(out, if (length(dest_p) == 1) dest_p else prod_id)
            } else {
                dest_p <- NULL
                pn <- stringr::str_detect(search, ptbl$pattern)
                if (sum(pn, na.rm = TRUE) > 0) {
                    dest_p <- unique(ptbl$parameter[which(pn)])
                }
                ## we can match on multiple parameters so long as they all map to the same parameter directory name
                if (length(dest_p) == 1) {
                    out <- file.path(out, dest_p)
                } else {
                    trunc <- TRUE
                }
            }
        }
        out <- sub("^/", "", out)
        if (!isTRUE(dots$no_host)) out <- file.path("oceandata.sci.gsfc.nasa.gov", out)
        return(file.path(this_att$local_file_root, sub("/$", "", out)))
    }
    if (!isTRUE(this_att$dry_run) && (is.null(thisds$user) || is.null(thisds$password) || na_or_empty(thisds$user) || na_or_empty(thisds$password))) stop("Oceandata sources require an Earthdata login: provide your user and password in the source configuration")
    if (verbose) cat("Downloading file list ... \n")
    myfiles <- bb_oceandata_filesearch(search = search, dtype = dtype, sensor = sensor, period = period, resolution_id = resolution_id, prod_id = prod_id, suite_id = suite_id, dtid = dtid, sdate = sdate, edate = edate)
    if (nrow(myfiles) < 1) stop("No files matched the supplied oceancolour data file search query (", search, ")")
    myfiles <- myfiles[, c("checksum", "filename", "last_modified", "size")]
    myfiles <- myfiles[order(myfiles$filename), ]
    if (verbose) cat(sprintf("\n%d file%s to download\n", nrow(myfiles), if (nrow(myfiles)>1) "s" else ""))
    ## for each file, download if needed and store in appropriate directory
    ok <- TRUE
    downloads <- tibble(url = NA_character_, file = myfiles$filename, was_downloaded = FALSE, to_download = FALSE, size = myfiles$size, last_modified = myfiles$last_modified)
    cookies_file <- tempfile()
    my_curl_config <- build_curl_config(debug = FALSE, show_progress = verbose, user = thisds$user, password = thisds$password, enforce_basic_auth = TRUE)
    ## and some more configs specifically for earthdata
    my_curl_config$options$followlocation <- 1
    my_curl_config$options$cookiefile <- cookies_file ## reads cookies from here
    my_curl_config$options$cookiejar <- cookies_file ## saves cookies here
    my_curl_config$options$unrestricted_auth <- 1L ## prior to curl 5.2.1 this was the default, and without it the authentication won't be properly passed to earthdata servers that serve data from a different hostname to the landing hostname
    myfiles$local_filename <- vapply(myfiles$filename, oceandata_url_mapper, no_host = isTRUE(dots$no_host), FUN.VALUE = "", USE.NAMES = FALSE) ## where local copy will go

    bx <- if (s3_target) aws_list_objects(target_s3_args, create = !isTRUE(this_att$dry_run), do_error = FALSE) else tibble(Key = character(), LastModified = as.POSIXct(c()))
    ## bx will be a zero-row tibble if !s3_target, or s3_target and the bucket is empty. If dry_run, bx will be NULL if it doesn't exist
    if (is.null(bx)) bx <- tibble(Key = character(), LastModified = as.POSIXct(c()))
    f_exists <- if (s3_target) { myfiles$filename %in% basename(bx$Key) } else { file.exists(myfiles$local_filename) }
    if (s3_target) {
        f_last_modified <- rep(as.POSIXct(NA), nrow(myfiles))
        for (idx in seq_len(nrow(myfiles))) {
            if (f_exists[idx]) f_last_modified[idx] <- bx$LastModified[which(basename(bx$Key) == myfiles$filename[idx])][1]
        }
        ## or could do that as a join
    } else {
        f_last_modified <- file.info(myfiles$local_filename)$mtime ## will be NA if file does not exist
    }
    myfiles$existing_checksum <- NA_character_
    ## iterate through file list and figure out which ones we'll actually download
    for (idx in seq_len(nrow(myfiles))) {
        this_url <- paste0("https://oceandata.sci.gsfc.nasa.gov/ob/getfile/", myfiles$filename[idx]) ## full URL
        downloads$url[idx] <- this_url
        this_fullfile <- myfiles$local_filename[idx]
        downloads$file[idx] <- this_fullfile
        download_this <- !f_exists[idx]
        if (this_att$clobber < 1) {
            ## don't clobber existing
        } else if (this_att$clobber == 1) {
            ## replace existing if server copy newer than local copy
            if (!s3_target) {
                ## ## use checksum rather than dates for this
                ## if (f_exists[idx]) {
                ##     existing_checksum <- file_hash(myfiles$local_filename[idx], hash = "sha1")
                ##     download_this <- existing_checksum != myfiles$checksum[idx]
                ## }
                ## checksums can be horribly slow to calculate locally (if the server does not have hardware hashing?)
                ## use dates
                if (is.na(f_last_modified[idx]) || (!is.na(myfiles$last_modified[idx]) && myfiles$last_modified[idx] > f_last_modified[idx])) download_this <- TRUE
                ## note though that apparently it is possible to get into the situation where a local file is marginally older than the remote according to the time stamps, but the if-modified-since download attempt does not download it, saying that the remote is not newer. Huh? So we set `remote_time = 2` in the rget call as a horrible workaround
            } else {
                ## check date of object in bucket list, download if oceandata's copy is newer
                bidx <- which(basename(bx$Key) == myfiles$filename[idx])
                if (length(bidx) == 1 && !is.na(myfiles$last_modified[idx]) && myfiles$last_modified[idx] > bx$LastModified[bidx]) {
                    download_this <- TRUE
                }
            }
        } else {
            download_this <- TRUE
        }
        if (grepl("\\.NRT\\.", this_fullfile) && isTRUE(download_this)) {
            ## as of 2024-ish, files can be named *.NRT.nc, and these are eventually replaced by non-NRT versions
            ## don't download NRT files if the replacement exists, either locally or on the remote server
            non_nrt_file <- sub("\\.NRT\\.nc$", ".nc", this_fullfile)
            if (!s3_target && file.exists(non_nrt_file)) {
                ## local non-NRT exists
                if (verbose) cat("not downloading ", myfiles$filename[idx], ", non-NRT version exists in local collection\n", sep = "")
                download_this <- FALSE
            } else if (s3_target && basename(non_nrt_file) %in% basename(bx$Key)) {
                ## non-NRT exists in bucket
                if (verbose) cat("not downloading ", myfiles$filename[idx], ", non-NRT version exists in destination bucket\n", sep = "")
                download_this <- FALSE
            } else if (basename(non_nrt_file) %in% myfiles$filename) {
                ## remote non-NRT is to be downloaded
                if (verbose) cat("not downloading ", myfiles$filename[idx], ", non-NRT version exists on the remote server\n", sep = "")
                download_this <- FALSE
            }
        }
        if (download_this) {
            if (!this_att$dry_run) {
                downloads$to_download[idx] <- TRUE
            } else {
                ## dry run
                cat("not downloading ", myfiles$filename[idx], ", dry_run is TRUE\n", sep = "")
            }
        } else if (verbose && f_exists[idx]) {
            if (!s3_target) {
                cat("not downloading ", myfiles$filename[idx], ", local copy exists and remote is not newer\n", sep = "")
            } else {
                cat("not downloading ", myfiles$filename[idx], ", ", if (this_att$clobber == 1) "newer ", "copy exists in destination bucket\n", sep = "")
            }
        }
    }
    to_download <- downloads$to_download
    downloads <- downloads[, setdiff(names(downloads), "to_download")]
    if (!this_att$dry_run && any(to_download, na.rm = TRUE)) {
        ## download all the files we identified above
        idx <- which(to_download)
        res <- bb_rget(downloads$url[idx], force_local_filename = downloads$file[idx], use_url_directory = FALSE, clobber = this_att$clobber, curl_opts = my_curl_config$options, verbose = verbose, target_s3_args = target_s3_args, remote_time = 2) ## TODO check are there any other args in dots to use here? dots[intersect(names(dots), names(formals("bb_rget")))]
        ## merge res back into the downloads tibble
        if (res$ok) {
            downloads[idx, ] <- res$files[[1]]
        } else {
            ## do something sensible TODO
        }
    }
    tibble(ok = ok, files = list(downloads), message = "")
}


# Satellite platform names and abbreviations used in Oceancolor URLs and file names
# Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
# For example, \url{https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{https://oceandata.sci.gsfc.nasa.gov/ob/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the Oceancolor visual browser or file search facility)
# maps to \url{https://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). Locally, this file will be stored in oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
# The \code{oceandata_platform_map} function maps the URL platform component ("V" in this example) to the corresponding directory name ("VIIRS")
# @param abbrev character: the platform abbreviation from the URL (e.g. "Q" for Aquarius, "M" for MODIS-Aqua)
# @param error_no_match logical: should an error be thrown if the abbrev is not matched?
# @references \url{https://oceandata.sci.gsfc.nasa.gov/}
# @return Either the platform name string corresponding to the abbreviation, if \code{abbrev} supplied, or a data.frame of all abbreviations and platform name strings if \code{abbrev} is missing
# @seealso \code{\link{oceandata_timeperiod_map}}, \code{\link{oceandata_parameter_map}}
# @export
oceandata_platform_map <- function(abbrev,error_no_match=FALSE) {
    allp <- tribble(~abbrev, ~platform,
                    "Q", "Aquarius",
                    "C", "CZCS",
                    "H", "HICO",
                    "M", "MERIS",
                    "A", "MODISA",
                    "T", "MODIST",
                    "O", "OCTS",
                    "S", "SeaWiFS",
                    "V", "VIIRS")
    if (missing(abbrev)) {
        allp
    } else {
        assert_that(is.string(abbrev))
        abbrev[!abbrev %in% allp$abbrev] <- oceandata_platform_to_abbrev(abbrev[!abbrev %in% allp$abbrev]) ## turn full platform names into abbreviations
        out <- allp$platform[allp$abbrev==abbrev]
        if (error_no_match & length(out)<1) {
            stop("oceandata platform \"", abbrev, "\" not recognized")
        }
        out
    }
}

oceandata_alltp <- tribble(~abbrev, ~time_period,
                           "WC", "8D_Climatology",
                           "8D", "8Day",
                           "YR", "Annual",
                           "CU", "Cumulative",
                           "DAY", "Daily",
                           "MO", "Monthly",
                           "MC", "Monthly_Climatology",
                           "R32", "Rolling_32_Day",
                           "SNSP", "Seasonal",
                           "SNSU", "Seasonal",
                           "SNAU", "Seasonal",
                           "SNWI", "Seasonal",
                           "SCSP", "Seasonal_Climatology",
                           "SCSU", "Seasonal_Climatology",
                           "SCAU", "Seasonal_Climatology",
                           "SCWI", "Seasonal_Climatology")

# Time periods and abbreviations used in Oceancolor URLs and file names
# Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
# For example, \url{https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{https://oceandata.sci.gsfc.nasa.gov/ob/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the Oceancolor visual browser or file search facility)
# maps to \url{https://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). Locally, this file will be stored in oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
# The \code{oceandata_timeperiod_map} function maps the URL time period component ("DAY" in this example) to the corresponding directory name ("Daily")
# @references \url{https://oceandata.sci.gsfc.nasa.gov/}
# @param abbrev string: the time period abbreviation from the URL (e.g. "DAY" for daily, "SCSP" for seasonal spring climatology)
# @param error_no_match logical: should an error be thrown if the abbrev is not matched?
# @return Either the time period string corresponding to the abbreviation, if \code{abbrev} supplied, or a data.frame of all abbreviations and time period strings if \code{abbrev} is missing
# @seealso \code{\link{oceandata_platform_map}}, \code{\link{oceandata_parameter_map}}
# @export
oceandata_timeperiod_map <- function(abbrev,error_no_match=FALSE) {
    if (missing(abbrev)) {
        oceandata_alltp
    } else {
        assert_that(is.string(abbrev))
        out <- oceandata_alltp$time_period[oceandata_alltp$abbrev==abbrev]
        if (error_no_match & length(out)<1) {
            stop("oceandata URL timeperiod token ",abbrev," not recognized")
        }
        out
    }
}


# rdname oceandata_parameter_map
#
# @param platform V for VIIRS, S for SeaWiFS, etc.
#
# @export
oceandata_parameters <- function(platform) {
    ## note some VIIRS parameters that appear in the browse file structure but with no associated files, and so have not been coded here:
    ## CHLOCI GSM QAA ZLEE
    ## platforms yet to do: "Q","H" (are different folder structure to the others)
    tribble(~platform, ~parameter, ~pattern, ~prod_id,
            "SATCO", "Kd", "KD490[_\\.]Kd_490", "Kd_490",
            "SATCO", "Kd", "KD[_\\.]Kd_490", "Kd_490",
            "SATCO", "NSST", "NSST", "NSST",
            "SATCO", "Rrs", "RRS[_\\.]Rrs_[[:digit:]]+", "Rrs_[[:digit:]]+",
            "SATCO", "SST", "SST", "sst",
            "SATCO", "SST", "SST[_\\.]sst", "sst",
            "SATCO", "SST4", "SST4", "sst4",
            "SATCO", "a", "IOP[_\\.]a_.*", "",
            "SATCO", "adg", "IOP[_\\.]adg_.*", "",
            "SATCO", "angstrom", "RRS[_\\.]angstrom", "angstrom",
            "SATCO", "aot", "RRS[_\\.]aot_[[:digit:]]+", "aot_[[:digit:]]+",
            "SATCO", "aph", "IOP[_\\.]aph_.*", "",
            "SATCO", "bb", "IOP[_\\.]bb_.*", "",
            "SATCO", "bbp", "IOP[_\\.]bbp_.*", "",
            "SATCO", "cdom", "CDOM[_\\.]cdom_index", "",
            "SATCOV", "chl", "CHL[_\\.]chl_ocx", "chl_ocx",
            "SATCOV", "chlor", "CHL[_\\.]chlor_a", "chlor_a",
            "SATCO", "ipar", "FLH[_\\.]ipar", "ipar",
            "SATCO", "nflh", "FLH[_\\.]nflh", "nflh",
            "SATCOV", "par", "PAR[_\\.]par", "par",
            "SATCOV", "pic", "PIC[_\\.]pic", "pic",
            "SATCOV", "poc", "POC[_\\.]poc", "poc",
            "S", "NDVI", "LAND[_\\.]NDVI", "ndvi",
            "V", "KD490", "KD490[_\\.]Kd_490", "Kd_490",
            "V", "KD490", "KD[_\\.]Kd_490", "Kd_490",
#            "V", "chl", "CHL[_\\.]chl_ocx",
#            "V", "chlor", "CHL[_\\.]chlor_a",
            "V", "IOP", "IOP[_\\.].*", "",
#            "V", "par", "PAR[_\\.]par",
#            "V", "pic", "PIC[_\\.]pic",
#            "V", "poc", "POC[_\\.]poc",
            "SATV", "RRS", "RRS", "Rrs",
            "SATV", "RRS", "RRS[_\\.][[:digit:]]+", "Rrs_[[:digit:]]+")
}


# Parameter names used in Oceancolor URLs and file names
# Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
# For example, \url{https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{https://oceandata.sci.gsfc.nasa.gov/ob/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the Oceancolor visual browser or file search facility)
# maps to \url{https://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). Locally, this file will be stored in oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
# The \code{oceandata_parameter_map} function maps the URL parameter component ("NPP_PAR_par" in this example) to the corresponding directory name ("par").
# @references \url{https://oceandata.sci.gsfc.nasa.gov/}
# @param urlparm string: the parameter component of the URL (e.g. "KD490_Kd_490" for MODIS diffuse attenuation coefficient at 490 nm)
# @param platform character: the platform abbreviation (currently one of "Q" (Aquarius), "C" (CZCS), "H" (HICO), "M" (MERIS), "A" (MODISA), "T" (MODIST), "O" (OCTS), "S" (SeaWiFS), "V" (VIIRS)
# @param error_no_match logical: should an error be thrown if the urlparm is not matched?
# @return Either the directory string corresponding to the URL code, if \code{abbrev} supplied, or a data.frame of all URL regexps and corresponding directory name strings if \code{urlparm} is missing
# @export
oceandata_parameter_map <- function(platform, urlparm, error_no_match = FALSE) {
    if (missing(platform) || !(is.string(platform) && nchar(platform) == 1)) stop("platform must be specified as a one-letter character")
    parm_map <- oceandata_parameters()
    parm_map <- parm_map[grepl(platform, parm_map$platform), ]
    if (!missing(urlparm)) {
        if (nrow(parm_map) > 0) {
            this_parm_folder <- vapply(parm_map$pattern, function(z) grepl(paste0("^", z, "$"), urlparm), FUN.VALUE = TRUE)
            out <- unlist(parm_map$parameter[this_parm_folder])
        } else {
            out <- as.character(NULL)
        }
        if (error_no_match & length(out) < 1) {
            stop("oceandata parameter \"", urlparm, "\" not recognized for platform ", platform)
        }
        out
    } else {
        parm_map
    }
}

## platform component of filenames was changed from e.g. "A" to "AQUA_MODIS" starting 2019
oceandata_platform_to_abbrev <- function(p) {
    p[p %in% "AQUA_MODIS"] <- "A"
    p[p %in% "SEASTAR_SEAWIFS_GAC"] <- "S"
    p[p %in% "TERRA_MODIS"] <- "T"
    p[p %in% "NIMBUS7_CZCS"] <- "C"
    p[p %in% c("SNPP_VIIRS", "JPSS1_VIIRS")] <- "V"
    p
}

oceandata_find_platform <- function(x) {
    ## find the full platform name in a URL or search spec
    ## look for full first, because SNPP_VIIRS is ambiguous with S (old SeaWiFS abbrev)
    ## if full not found, look for abbreviation but return its full equivalent
    chk <- stringr::str_match(x, "(AQUA_MODIS|SEASTAR_SEAWIFS_GAC|TERRA_MODIS|NIMBUS7_CZCS|SNPP_VIIRS|JPSS1_VIIRS)")
    if (nrow(chk) < 1 || is.na(chk[1, 2])) {
        ## abbreviated platform, but it has to be at the start of the string or after a /
        chk <- stringr::str_match(x, "^([ASTCV])")
        if (nrow(chk) < 1) chk <- stringr::str_match(x, "/([ASTCV])")
        if (nrow(chk) == 1) chk[1, 2] <- oceandata_platform_map(chk[1, 2])
    }
    if (nrow(chk) == 1) chk[1, 2] else NULL
}

# Map Oceancolor URL to file path
# Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
# For example, \url{https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{https://oceandata.sci.gsfc.nasa.gov/ob/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the Oceancolor visual browser or file search facility)
# maps to \url{https://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). Locally, this file will be stored in oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
# Newer files previously mapped to YYYY/DOY folders are now mapped to YYYY/MM/DD, e.g. <https://oceandata.sci.gsfc.nasa.gov/ob/getfile/AQUA_MODIS.20230109.L3b.DAY.RRS.nc will map to oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2023/01/09/AQUA_MODIS.20230109.L3b.DAY.RRS.nc
# The \code{oceandata_url_mapper} function maps the URL parameter component ("NPP_PAR_par" in this example) to the corresponding directory name ("par").
# @references \url{https://oceandata.sci.gsfc.nasa.gov/}
# @param this_url string: the Oceancolor URL, e.g. https://oceandata.sci.gsfc.nasa.gov/ob/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km.bz2
# @param path_only logical: if TRUE, do not append the file name to the path
# @param sep string: the path separator to use
# @return Either the directory string corresponding to the URL code, if \code{abbrev} supplied, or a data.frame of all URL regexps and corresponding directory name strings if \code{urlparm} is missing
# @export
oceandata_url_mapper <- function(this_url, path_only = FALSE, no_host = FALSE, sep = .Platform$file.sep) {
    ## take getfile URL or base filename and return (relative) path to put the file into
    ## this_url should look like e.g. (old format) https://oceandata.sci.gsfc.nasa.gov/ob/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km.bz2
    ## or (newer format) https://oceandata.sci.gsfc.nasa.gov/ob/getfile/AQUA_MODIS.20230109.L3b.DAY.RRS.nc
    ## Mapped files (L3m) should become oceandata.sci.gsfc.nasa.gov/platform/Mapped/timeperiod/spatial/parm/[yyyy/]basename
    ## [yyyy] only for 8Day,Daily,Rolling_32_Day
    ## Binned files (L3b) should become oceandata.sci.gsfc.nasa.gov/platform/L3BIN/yyyy/ddd/basename
    assert_that(is.string(this_url))
    assert_that(is.flag(path_only), !is.na(path_only))
    assert_that(is.string(sep))
    if (grepl("\\.L3m[_\\.]", this_url)) {
        ## mapped file
        url_parts <- str_match(basename(this_url), "^([ASTCV]|AQUA_MODIS|SEASTAR_SEAWIFS_GAC|TERRA_MODIS|NIMBUS7_CZCS|SNPP_VIIRS|JPSS1_VIIRS)\\.?([[:digit:]_]+)\\.(L3m)[_\\.]([[:upper:][:digit:]]+)[_\\.](.*?)[_\\.](9|4)(km)?\\..*?(bz2|nc)")
        ## e.g. [1,] "https://oceandata.sci.gsfc.nasa.gov/ob/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km"
        ## [,2] [,3]      [,4]  [,5]  [,6]          [,7]
        ## "A"  "2002359" "L3m" "DAY" "CHL_chlor_a" "9"
        url_parts <- as.data.frame(url_parts, stringsAsFactors = FALSE)
        colnames(url_parts) <- c("full_url", "platform", "date", "type", "timeperiod", "parm", "spatial", "spatial_unit", "ext")
        ## map back to old sensor abbreviations, at least temporarily
        url_parts$platform <- oceandata_platform_to_abbrev(url_parts$platform)
    } else if (grepl("\\.L3b[_\\.]", this_url)) {

        url_parts <- str_match(basename(this_url), "^([ASTCV]|AQUA_MODIS|SEASTAR_SEAWIFS_GAC|TERRA_MODIS|NIMBUS7_CZCS|SNPP_VIIRS|JPSS1_VIIRS)\\.?([[:digit:]]+)\\.(L3b)[_\\.]([[:upper:][:digit:]]+)[_\\.](.*?)\\.(bz2|nc)")
        ## https://oceandata.sci.gsfc.nasa.gov/ob/getfile/A20090322009059.L3b_MO_KD490.main.bz2

        ## e.g. [1,] "https://oceandata.sci.gsfc.nasa.gov/ob/getfile/A20090322009059.L3b_MO_KD490.main.bz2" "A"  "20090322009059" "L3b" "MO" "KD490"
        ## https://oceandata.sci.gsfc.nasa.gov/ob/getfile/A2015016.L3b_DAY_RRS.nc
        url_parts <- as.data.frame(url_parts, stringsAsFactors = FALSE)
        colnames(url_parts) <- c("full_url", "platform", "date", "type", "timeperiod", "parm")
        url_parts$platform <- oceandata_platform_to_abbrev(url_parts$platform)
    } else if (grepl("\\.L2", this_url)) {
      # "https://oceandata.sci.gsfc.nasa.gov/ob/getfile/A2017002003000.L2_LAC_OC.nc"
      url_parts <- str_match(basename(this_url), "^([ASTCV]|AQUA_MODIS|SEASTAR_SEAWIFS_GAC|TERRA_MODIS|NIMBUS7_CZCS|SNPP_VIIRS|JPSS1_VIIRS)\\.?([[:digit:]]+)\\.(L2)[_\\.]([[:upper:][:digit:]]+)[_\\.](.*?)\\.(bz2|nc)")
      url_parts <- as.data.frame(url_parts, stringsAsFactors = FALSE)
      colnames(url_parts) <- c("full_url", "platform", "date", "type", "coverage", "parm", "extension")
        url_parts$platform <- oceandata_platform_to_abbrev(url_parts$platform)
    } else {
        stop("not a L2 or L3 binned or L3 mapped file")
    }
    this_year <- substr(url_parts$date, 1, 4)
    if (is.na(url_parts$type)) {
        ## no type provided? we can't proceed with the download, anyway
        stop("cannot ascertain file type from oceancolor URL: ",this_url)
    } else {
        switch(url_parts$type,
               L3m = {
                   this_parm_folder <- oceandata_parameter_map(url_parts$platform, url_parts$parm, error_no_match = TRUE)
                   out <- paste(if (isTRUE(no_host)) "" else "oceandata.sci.gsfc.nasa.gov", oceandata_platform_map(url_parts$platform, error_no_match=TRUE), "Mapped", oceandata_timeperiod_map(url_parts$timeperiod, error_no_match=TRUE), paste0(url_parts$spatial, "km"), this_parm_folder, sep=sep)
                   if (url_parts$timeperiod %in% c("8D", "DAY", "R32")) {
                       out <- paste(out, this_year, sep=sep)
                   }
                   if (!path_only) {
                       out <- paste(out, basename(this_url), sep=sep)
                   } else {
                       out <- paste0(out, sep) ## trailing path separator
                   }
               },
               L3b = {
                   this_doy <- if (nchar(url_parts$date) == 8) paste0(substr(url_parts$date, 5, 6), sep, substr(url_parts$date, 7, 8)) else substr(url_parts$date, 5, 7)
                   out <- paste(if (isTRUE(no_host)) "" else "oceandata.sci.gsfc.nasa.gov", oceandata_platform_map(url_parts$platform, error_no_match=TRUE), "L3BIN", this_year, this_doy, sep=sep)
                   if (!path_only) {
                       out <- paste(out, basename(this_url), sep=sep)
                   } else {
                       out <- paste0(out, sep) ## trailing path separator
                   }
               },
               L2 = {
                   this_doy <- if (nchar(url_parts$date) == 8) paste0(substr(url_parts$date, 5, 6), sep, substr(url_parts$date, 7, 8)) else substr(url_parts$date, 5, 7)
                   out <- paste(if (isTRUE(no_host)) "" else "oceandata.sci.gsfc.nasa.gov", oceandata_platform_map(url_parts$platform, error_no_match=TRUE), "L2", this_year, this_doy, sep=sep)
                   if (!path_only) {
                       out <- paste(out, basename(this_url), sep=sep)
                   } else {
                       out <- paste0(out, sep) ## trailing path separator
                   }
               },
               stop("unrecognized file type: ", url_parts$type, "\n", capture.output(str(url_parts)))
               )
    }
    sub("^/", "", out)
}


#' Postprocessing: remove redundant NRT oceandata files
#'
#' This function is not intended to be called directly, but rather is specified as a \code{postprocess} option in \code{\link{bb_source}}.
#'
#' This function will remove near-real-time (NRT) files from an oceandata collection that have been superseded by their non-NRT versions.
#'
#' @param ... : extra parameters passed automatically by \code{bb_sync}
#'
#' @return a list, with components \code{status} (TRUE on success) and \code{deleted_files} (character vector of paths of files that were deleted)
#'
#' @export
bb_oceandata_cleanup <- function(...) {
    bb_nrt_cleanup_inner(findnrt = function(z) grep("\\.NRT\\.nc$", z), nrt2rt = function(z) sub("\\.NRT\\.nc$", ".nc", z), ...)
}


## unfinished function to create oceandata source definition given platform, parm, etc
##
##oceandata_source <- function(platform, parameter, processing_level, time_resolution, spatial_resolution, years) {
##    ## platform
##    assert_that(is.string(platform))
##    plat_str <- oceandata_platform_map(platform) ## full platform name, also checks that platform is recognized
##    ## parameter
##    parm_str <- oceandata_parameter_map(platform, parameter)
##    ## processing level
##    assert_that(is.string(processing_level))
##    processing_level <- match.arg(processing_level, c("L3m", "L3b", "L2"))
##    ## readable processing level string
##    pl_str <- switch(processing_level,
##                     "L3m"="Level-3 mapped",
##                     "L3b"="Level-3 binned",
##                     "L2"="Level 2",
##                     "unknown processing level")
##    ## time res
##    tr_str <- oceandata_timeperiod_map(time_resolution)
##    ## spatial res
##    assert_that(is.string(spatial_resolution))
##    spatial_resolution <- match.arg(tolower(spatial_resolution), c("9km", "4km"))
##
##        name=paste("Oceandata", plat_str, pl_str, tr_str, spatial_resolution, parameter),
##        id=paste(plat_str, processing_level, time_resolution, parm_str, spatial_resolution, sep="_"),
##        description="8-day remote-sensing chlorophyll-a from the MODIS Aqua satellite at 9km spatial resolution",
##        doc_url="http://oceancolor.gsfc.nasa.gov/",
##        citation="See https://oceancolor.gsfc.nasa.gov/citations",
##        license="Please cite",
##        method=list("bb_handler_oceandata", search="A*L3m_8D_CHL_chlor_a_9km.nc"),
##        postprocess=NULL,
##        collection_size=8,
##        comment="Collection size is approximately 500MB per year",
##        data_group="Ocean colour"),
##
##}
