#' Query the NASA/OB.DAAC Oceancolor file search API
#'
#' Previously it was possible to search based on filename pattern alone, e.g. "S*L3m_MO_CHL_chlor_a_9km.nc" would match monthly level-3 mapped chlorophyll data from the SeaWiFS satellite at 9km resolution, in netcdf format. However, as of June 2026 the OB.DAAC search API requires other parameters to be set, in order to reduce the search load on the server, see \url{https://www.earthdata.nasa.gov/data/alerts-outages/ob.daac-file-search-utility-now-requires-more-specificity}. It is not obvious which or how many parameters are required for a given search. If you are constructing an oceandata bowerbird source you are advised to use the file search web page \url{https://oceandata.sci.gsfc.nasa.gov/api/file_search} and its help page \url{https://oceandata.sci.gsfc.nasa.gov/file_search/file_search_help/} to help find the correct parameters.
#'
#' It is possible to provide multiple values for one or more search parameters: the POST request to the file search API will be repeated for all possible combinations of parameters (constructed via \code{expand.grid}). This might be helpful, for example, by allowing \code{dtid} to be specified as a two-element vector with one value for refined data and the second for NRT data. Specifying two \code{dtid} values will yield a much faster search than omitting \code{dtid} and relying on the \code{search} parameter to achieve the same refined/NRT file inclusions.
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
#'
#' @return A tibble with columns \code{filename}, \code{size}, \code{checksum} (SHA1 file hash), \code{last_modified}
#'
#' @examples
#'
#' ## Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a
#' res <- bb_oceandata_filesearch(sensor = "SeaWiFS", dtype = "L3m", period = "MO", dtid = 1123,
#'   resolution_id = "9km", prod_id = "chlor_a", sdate = "1997-09-01", edate = "2011-01-01")
#'
#' @export
bb_oceandata_filesearch <- function(search = NULL, sensor = NULL, dtype = NULL, dtid = NULL, period = NULL, resolution_id = NULL, prod_id = NULL, suite_id = NULL, sdate = NULL, edate = NULL) {
    if (!is.null(search)) assert_that(is.character(search), all(nzchar(search)))
    if (!is.null(dtype)) assert_that(is.character(dtype), all(nzchar(dtype)))
    if (!is.null(sensor)) assert_that(is.character(sensor), all(nzchar(sensor)))
    if (!is.null(period)) assert_that(is.character(period), all(nzchar(period)))
    if (!is.null(resolution_id)) assert_that(is.character(resolution_id), all(nzchar(resolution_id)))
    if (!is.null(prod_id)) assert_that(is.character(prod_id), all(nzchar(prod_id)))
    if (!is.null(suite_id)) assert_that(is.character(suite_id), all(nzchar(suite_id)))
    if (!is.null(dtid)) assert_that(is.numeric(dtid), !any(is.na(dtid)))
    if (!is.null(sdate)) {
        if (inherits(sdate, "Date")) sdate <- format(sdate, "%Y-%m-%d")
        assert_that(is.character(sdate), all(nzchar(sdate)), !any(is.na(sdate)))
    }
    if (is.null(edate)) edate <- format(Sys.Date() + 40, "%Y-%m-%d") ## edate seems to be required, so insert today's date plus a bit if not provided by the user
    if (!is.null(edate)) {
        if (inherits(edate, "Date")) edate <- format(edate, "%Y-%m-%d")
        assert_that(is.character(edate), all(nzchar(edate)), !any(is.na(edate)))
    }

    qry <- Filter(Negate(is.null), list(search = search, dtype = dtype, sensor = sensor, period = period, resolution_id = resolution_id, prod_id = prod_id, suite_id = suite_id, dtid = dtid, sdate = sdate, edate = edate, format = "json", cksum = 1))
    ## if any parm is of length > 1, we repeat the API call across all combinations
    temp <- do.call(expand.grid, c(qry, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))
    qrys <- lapply(seq_len(nrow(temp)), function(i) as.list(temp[i, ]))
    ## qrys is now a list of lists, with each sub-list a set of search parms. qrys can be length > 1 if we have multiple sets of search parms
    allfiles <- list()
    for (qi in seq_along(qrys)) {
        qry <- qrys[[qi]]
        for (tries in 1:3) {
            myfiles <- httr::POST("https://oceandata.sci.gsfc.nasa.gov/api/file_search", body = paste(sapply(seq_along(qry), function(i) paste0(names(qry)[i], "=", qry[i])), collapse = "&"), encode = "form", httr::content_type("application/x-www-form-urlencoded"), httr::accept("*/*"))
            ## httr::accept("*/*") seems to be necessary to receive json format
            if (!httr::http_error(myfiles)) break
            tries <- tries + 1
        }
        if (httr::http_error(myfiles)) stop("error with oceancolour data file search: could not retrieve file list (query: ", search, ")")
        myfiles <- httr::content(myfiles, as = "text", encoding = "UTF-8")
        if (any(grepl("the database had an error", myfiles, ignore.case = TRUE), na.rm = TRUE)) stop("The oceancolour data file search failed with a database error: try again later and/or check the status page at https://www.earthdata.nasa.gov/data/alerts-outages")
        ## look for an empty body or "No results found" message
        if (length(myfiles) < 1 || !any(nzchar(myfiles)) || any(grepl("no results found", myfiles, ignore.case = TRUE), na.rm = TRUE)) {
            ## do nothing
        } else {
            allfiles <- c(allfiles, jsonlite::fromJSON(myfiles))
        }
    }
    myfiles <- cbind(do.call(rbind, lapply(allfiles, as_tibble)), filename = names(allfiles))
    if (length(allfiles) < 1) {
        tibble(filename = character(), size = integer(), checksum = character(), last_modified = as.POSIXct(numeric()))
    } else {
        names(myfiles)[which(names(myfiles) == "cdate")] <- "last_modified" ## cdate is (presumably) the last-modified date
        myfiles$checksum <- sub("^sha1:", "", myfiles$checksum)
        myfiles$size <- as.integer(myfiles$size)
        myfiles$last_modified <- lubridate::ymd_hms(myfiles$last_modified, tz = "UTC")
        myfiles
    }
}
