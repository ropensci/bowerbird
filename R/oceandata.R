#' Handler for Oceandata data sets
#'
#' This is a handler function to be used with data sets from NASA's Oceandata system. This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}.
#'
#' Note that users will need an Earthdata login, see https://urs.earthdata.nasa.gov/. Users will also need to authorize the application 'OB.DAAC Data Access' (see 'My Applications' at https://urs.earthdata.nasa.gov/profile)
#'
#' Oceandata uses standardized file naming conventions (see https://oceancolor.gsfc.nasa.gov/docs/format/), so once you know which products you want you can construct a suitable file name pattern to search for. For example, "S*L3m_MO_CHL_chlor_a_9km.nc" would match monthly level-3 mapped chlorophyll data from the SeaWiFS satellite at 9km resolution, in netcdf format. This pattern is passed as the \code{search} argument. Note that the \code{bb_handler_oceandata} does not take need `source_url` to be specified in the \code{bb_source} call.
#'
#' @references https://oceandata.sci.gsfc.nasa.gov/
#' @param search string: (required) the search string to pass to the oceancolor file searcher (https://oceandata.sci.gsfc.nasa.gov/api/file_search)
#' @param dtype string: (optional) the data type (e.g. "L3m") to pass to the oceancolor file searcher. Valid options at the time of writing are aquarius, seawifs, aqua, terra, meris, octs, czcs, hico, viirs (for snpp), viirsj1, s3olci (for sentinel-3a), s3bolci (see https://oceancolor.gsfc.nasa.gov/data/download_methods/)
#' @param sensor string: (optional) the sensor (e.g. "seawifs") to pass to the oceancolor file searcher. Valid options at the time of writing are L0, L1, L2, L3b (for binned data), L3m (for mapped data), MET (for ancillary data), misc (for sundry products)
#' @param ... : extra parameters passed automatically by \code{bb_sync}
#'
#' @return TRUE on success
#'
#' @examples
#'
#' my_source <- bb_source(
#'   name="Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a",
#'   id="SeaWiFS_L3m_MO_CHL_chlor_a_9km",
#'   description="Monthly remote-sensing chlorophyll-a from the SeaWiFS satellite at
#'     9km spatial resolution",
#'   doc_url="https://oceancolor.gsfc.nasa.gov/",
#'   citation="See https://oceancolor.gsfc.nasa.gov/citations",
#'   license="Please cite",
#'   method=list("bb_handler_oceandata",search="S*L3m_MO_CHL_chlor_a_9km.nc"),
#'   postprocess=NULL,
#'   collection_size=7.2,
#'   data_group="Ocean colour")
#'
#' @export
bb_handler_oceandata <- function(search, dtype, sensor, ...) {
    assert_that(is.string(search), nzchar(search))
    if (!missing(dtype)) {
        if (!is.null(dtype)) assert_that(is.string(dtype), nzchar(dtype))
    } else {
        dtype <- NULL
    }
    if (!missing(sensor)) {
        if (!is.null(sensor)) assert_that(is.string(sensor), nzchar(sensor))
    } else {
        sensor <- NULL
    }
    do.call(bb_handler_oceandata_inner, list(..., search = search, dtype = dtype, sensor = sensor))
}


# @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
# @param verbose logical: if TRUE, provide additional progress output
# @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_oceandata_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, search, search_method = "api", dtype = NULL, sensor = NULL, stop_on_download_error = FALSE, target_s3_args, ...) {
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
    assert_that(is.string(search), nzchar(search))
    assert_that(is.string(search_method), nzchar(search_method))
    search_method <- match.arg(tolower(search_method), c("api", "scrape"))
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
        if (search_method == "scrape") {
            ## url will be https://oceandata.sci.gsfc.nasa.gov/SeaWiFS/Mapped/something
            ## we just want the host and first two path components
            temp <- httr::parse_url(search)
            out <- strsplit(temp$path, "/")[[1]]
            if (is.null(temp$hostname) || !nzchar(temp$hostname) || length(out) < 2) {
                stop("could not figure out local directory from scrape search URL")
            }
            out <- file.path(out[1], out[2])
            if (!isTRUE(dots$no_host)) out <- c(temp$hostname, out)
        } else {
            ## note that we can't use url_mapper here, because the search string in the source definition is unlikely to conform to the expected full pattern
            ## highest-level dir
            out <- "" ## optional prefix with hostname, below
            ## refine by platform. Find platform, either full or one-letter abbrev
            this_platform <- oceandata_find_platform(search)
            if (!is.na(this_platform) && nzchar(this_platform)) {
                ## want old platform name for backwards compatibility
                if (!this_platform %in% oceandata_platform_map()$platform) {
                    this_platform <- oceandata_platform_map(oceandata_platform_to_abbrev(this_platform))
                }
                out <- file.path(out, this_platform)
            }
            if (grepl("L3m", search)) {
                out <- file.path(out, "Mapped")
            } else if (grepl("L3", search)) {
                out <- file.path(out, "L3BIN")
            }
            ## time period, if it's specified in the search string
            tp <- stringr::str_detect(search, paste0("\\.", oceandata_alltp$abbrev, "\\."))
            if (sum(tp, na.rm = TRUE) == 1) {
                out <- file.path(out, oceandata_alltp$time_period[which(tp)])
            }
            ## spatial res
            if (grepl("4km", search)) out <- file.path(out, "4km") else if (grepl("9km", search)) out <- file.path(out, "9km")
            ## next level down is parameter name
            ptbl <- oceandata_parameters()
            pn <- stringr::str_detect(search, ptbl$pattern)
            if (sum(tp, na.rm = TRUE) > 0) {
                dest_p <- unique(ptbl$parameter[which(pn)])
                ## we can match on multiple parameters so long as they all map to the same parameter directory name
                if (length(dest_p) == 1) out <- file.path(out, dest_p)
            }
            out <- sub("^/", "", out)
            if (!isTRUE(dots$no_host)) out <- file.path("oceandata.sci.gsfc.nasa.gov", out)
        }
        return(file.path(this_att$local_file_root, sub("/$", "", out)))
    }
    if (is.null(thisds$user) || is.null(thisds$password) || na_or_empty(thisds$user) || na_or_empty(thisds$password)) stop("Oceandata sources require an Earthdata login: provide your user and password in the source configuration")
    tries <- 0
    if (verbose) cat("Downloading file list ... \n")
    if (search_method == "api") {
        ## this can be super slow for large queries, but it gives a checksum
        while (tries < 3) {
            ## previously we used a GET request but this no longer seems to deliver json format with cdate, but POST does
            qry <- Filter(Negate(is.null), list(search = search, dtype = if (!is.null(dtype)) dtype, sensor = if (!is.null(sensor)) sensor, format = "json", cksum = 1))
            myfiles <- httr::POST("https://oceandata.sci.gsfc.nasa.gov/api/file_search",
                                  body = paste(sapply(seq_along(qry), function(i) paste0(names(qry)[i], "=", qry[i])), collapse = "&"),
                                  encode = "form", httr::content_type("application/x-www-form-urlencoded"), httr::accept("*/*"))
            ## httr::accept("*/*") seems to be necessary to receive json format
            if (!httr::http_error(myfiles)) break
            tries <- tries + 1
        }
        if (httr::http_error(myfiles)) stop("error with oceancolour data file search: could not retrieve file list (query: ", search, ")")
        myfiles <- httr::content(myfiles, as = "text")
        ## look for an empty body or "No results found" message
        if (length(myfiles) < 1 || !any(nzchar(myfiles)) || any(grepl("no results found", myfiles, ignore.case = TRUE), na.rm = TRUE)) stop("No files matched the supplied oceancolour data file search query (", search, ")")
        myfiles <- jsonlite::fromJSON(myfiles)
        myfiles <- cbind(do.call(rbind, lapply(myfiles, as_tibble)), filename = names(myfiles))
        myfiles$checksum <- sub("^sha1:", "", myfiles$checksum)
        names(myfiles)[which(names(myfiles) == "cdate")] <- "last_modified" ## cdate is (presumably) the last-modified date
        myfiles <- myfiles[, c("checksum", "filename", "last_modified")]
    } else if (search_method == "scrape") {
        ## potentially override accept_download etc
        myfiles <- bb_rget(search, level = 3,
                           accept_download = if ("accept_download" %in% names(dots)) dots[["accept_download"]] else "/getfile/",
                           ## follow 3-digit (day of year) or 4-digit (year) folders by default
                           accept_follow = if ("accept_follow" %in% names(dots)) dots[["accept_follow"]] else "/[[:digit:]]{3}[[:digit:]]?/$",
                           dry_run = TRUE, no_parent = FALSE, relative = FALSE, verbose = FALSE)
        myfiles <- tibble(filename = basename(myfiles$files[[1]]$url), checksum = NA_character_, last_modified = NA)
    }

    myfiles <- myfiles[order(myfiles$filename), ]
    if (verbose) cat(sprintf("\n%d file%s to download\n", nrow(myfiles), if (nrow(myfiles)>1) "s" else ""))
    ## for each file, download if needed and store in appropriate directory
    ok <- TRUE
    downloads <- tibble(url = NA_character_, file = myfiles$filename, was_downloaded = FALSE, to_download = FALSE)
    cookies_file <- tempfile()
    my_curl_config <- build_curl_config(debug = FALSE, show_progress = verbose, user = thisds$user, password = thisds$password, enforce_basic_auth = TRUE)
    ## and some more configs specifically for earthdata
    my_curl_config$options$followlocation <- 1
    my_curl_config$options$cookiefile <- cookies_file ## reads cookies from here
    my_curl_config$options$cookiejar <- cookies_file ## saves cookies here
    my_curl_config$options$unrestricted_auth <- 1L ## prior to curl 5.2.1 this was the default, and without it the authentication won't be properly passed to earthdata servers that serve data from a different hostname to the landing hostname
    myfiles$local_filename <- vapply(myfiles$filename, oceandata_url_mapper, no_host = isTRUE(dots$no_host), FUN.VALUE = "", USE.NAMES = FALSE) ## where local copy will go

    bx <- if (!isTRUE(this_att$dry_run) && s3_target) aws_list_objects(target_s3_args, create = TRUE) else tibble(Key = character(), LastModified = as.POSIXct(c()))
    f_exists <- if (s3_target) { myfiles$filename %in% basename(bx$Key) } else { file.exists(myfiles$local_filename) }
    myfiles$existing_checksum <- NA_character_
    ## iterate through file list and figure out which ones we'll actually download
    for (idx in seq_len(nrow(myfiles))) {
        this_url <- paste0("https://oceandata.sci.gsfc.nasa.gov/ob/getfile/", myfiles$filename[idx]) ## full URL
        downloads$url[idx] <- this_url
        this_fullfile <- myfiles$local_filename[idx]
        downloads$file[idx] <- this_fullfile
        download_this <- !f_exists[idx]
        ## - with search_method == "api", we can get a cdate (last-modified) for files on the oceandata side, and we can retrieve the local file last-modified dates (or retrieve the target bucket list with last-modified dates)
        ## - so we could either explicitly compare dates and only queue downloads for appropriate files
        ## - or we can set clobber = 1 and rget will use the file or bucket last-modified date to set the if-modified-since header, which makes a lot more requests but the code is simpler because we delegate the comparisons to the existing code in rget
        if (this_att$clobber < 1) {
            ## don't clobber existing
        } else if (this_att$clobber == 1) {
            ## replace existing if server copy newer than local copy
            if (search_method == "api" && !s3_target) {
                ## use checksum rather than dates for this
                if (f_exists[idx]) {
                    existing_checksum <- file_hash(myfiles$local_filename[idx], hash = "sha1")
                    download_this <- existing_checksum != myfiles$checksum[idx]
                }
            } else if (s3_target) {
                ## check date of object in bucket list, download if oceandata's copy is newer
                bidx <- which(basename(bx$Key) == myfiles$filename[idx])
                if (length(bidx) == 1 && !is.na(myfiles$last_modified[idx]) && myfiles$last_modified[idx] > bx$LastModified[bidx]) {
                    download_this <- TRUE
                }
            } else {
                download_this <- TRUE
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
                cat("not downloading ", myfiles$filename[idx], ", local copy exists with identical checksum\n", sep = "")
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
        res <- bb_rget(downloads$url[idx], force_local_filename = downloads$file[idx], use_url_directory = FALSE, clobber = this_att$clobber, curl_opts = my_curl_config$options, verbose = verbose, target_s3_args = target_s3_args) ## TODO check are there any other args in dots to use here? dots[intersect(names(dots), names(formals("bb_rget")))]
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
    tribble(~platform, ~parameter, ~pattern,
            "SATCO", "Kd", "KD490[_\\.]Kd_490",
            "SATCO", "Kd", "KD[_\\.]Kd_490", ## changed?
            "SATCO", "NSST", "NSST",
            "SATCO", "Rrs", "RRS[_\\.]Rrs_[[:digit:]]+",
            "SATCO", "SST", "SST",
            "SATCO", "SST", "SST[_\\.]sst",
            "SATCO", "SST4", "SST4",
            "SATCO", "a", "IOP[_\\.]a_.*",
            "SATCO", "adg", "IOP[_\\.]adg_.*",
            "SATCO", "angstrom", "RRS[_\\.]angstrom",
            "SATCO", "aot", "RRS[_\\.]aot_[[:digit:]]+",
            "SATCO", "aph", "IOP[_\\.]aph_.*",
            "SATCO", "bb", "IOP[_\\.]bb_.*",
            "SATCO", "bbp", "IOP[_\\.]bbp_.*",
            "SATCO", "cdom", "CDOM[_\\.]cdom_index",
            "SATCO", "chl", "CHL[_\\.]chl_ocx",
            "SATCO", "chlor", "CHL[_\\.]chlor_a",
            "SATCO", "ipar", "FLH[_\\.]ipar",
            "SATCO", "nflh", "FLH[_\\.]nflh",
            "SATCO", "par", "PAR[_\\.]par",
            "SATCO", "pic", "PIC[_\\.]pic",
            "SATCO", "poc", "POC[_\\.]poc",
            "S", "NDVI", "LAND[_\\.]NDVI",
            "V", "KD490", "KD490[_\\.]Kd_490",
            "V", "KD490", "KD[_\\.]Kd_490",
            "V", "chl", "CHL[_\\.]chl_ocx",
            "V", "chlor", "CHL[_\\.]chlor_a",
            "V", "IOP", "IOP[_\\.].*",
            "V", "par", "PAR[_\\.]par",
            "V", "pic", "PIC[_\\.]pic",
            "V", "poc", "POC[_\\.]poc",
            "V", "RRS", "RRS[_\\.].*")
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
oceandata_parameter_map <- function(platform,urlparm,error_no_match=FALSE) {
    if (missing(platform) || !(is.string(platform) && nchar(platform)==1)) stop("platform must be specified as a one-letter character")
    parm_map <- oceandata_parameters()
    parm_map <- parm_map[grepl(platform,parm_map$platform),]
    if (!missing(urlparm)) {
        if (nrow(parm_map)>0) {
            this_parm_folder <- vapply(parm_map$pattern,function(z)grepl(paste0("^",z,"$"),urlparm),FUN.VALUE=TRUE)
            out <- unlist(parm_map$parameter[this_parm_folder])
        } else {
            out <- as.character(NULL)
        }
        if (error_no_match & length(out)<1) {
            stop("oceandata parameter \"",urlparm,"\" not recognized for platform ",platform)
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
                   this_parm_folder <- oceandata_parameter_map(url_parts$platform, url_parts$parm, error_no_match=TRUE)
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
