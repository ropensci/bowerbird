#' Handler for NASA MODIS Rapid Response files
#'
#' @references https://earthdata.nasa.gov/earth-observation-data/near-real-time/rapid-response
#' @param data_source tibble: single-row tibble defining a data source, e.g. as returned by \code{bb_source}
#' @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
#'
#' @return TRUE on success
#'
#' @export
rapid_response_get <- function(data_source,local_dir_only=TRUE) {
    assert_that(is.flag(local_dir_only))
    if (local_dir_only) {
        dummy <- data_source
        dummy$source_url <- "http://lance-modis.eosdis.nasa.gov/imagery/subsets/"
        return(bb_wget(dummy,local_dir_only=TRUE))
    }
    ## NASA MODIS rapid response synchronisation handler

    ## URLs of the form:  http://lance-modis.eosdis.nasa.gov/imagery/subsets/?mosaic=Antarctica.2014342.terra.4km.tif
    ## where
    ##  resolution can be "4km" "2km" "1km". (Individual image tiles can be at resolution down to 250m, e.g. http://lance-modis.eosdis.nasa.gov/imagery/subsets/?subset=Antarctica_r06c01.2014342.terra.250m.tif)
    ##  platform can be "terra" or "aqua"
    ## can also ask for other bands: http://lance-modis.eosdis.nasa.gov/imagery/subsets/?mosaic=Antarctica.2014342.terra.721.4km http://lance-modis.eosdis.nasa.gov/imagery/subsets/?mosaic=Antarctica.2014342.terra.367.4km
    ## note that band 6 on aqua is not functional (http://lance-modis.eosdis.nasa.gov/imagery/rapid/About_Antarctica_mosaic.html)

    ## we only provide sync for the 4km truecolor mosaic images so far

    ## simple loop from current day back to 2013 day 345 (Dec 11)

    ## note some restrictions with the NASA server: it does not provide timestamps nor file sizes in the HTTP headers
    ## so we can't do timestamping or selective retrieval based on file sizes
    ## if clobber setting is 0 (don't replace existing files) or 1 (only replace existing files if remote version is newer than local) then existing local files will not be updated. If clobber==2 then existing files get replaced unconditionally

    this_resolution <- "4km"
    d <- Sys.Date()
    while (d>=format.Date("2013-12-11")) {
        rapid_response_do_download(d,"terra",this_resolution,data_source)
        rapid_response_do_download(d,"aqua",this_resolution,data_source)
        d <- d-1
    }
    TRUE
}

rapid_response_do_download <- function(d,platform,resolution,data_source) {
    this_url <- sprintf("http://lance-modis.eosdis.nasa.gov/imagery/subsets/?mosaic=Antarctica.%s%s.%s.%s.tif",format(d,"%Y"),format(d,"%j"),platform,resolution)
    dummy <- data_source
    ##dummy$method_flags=paste("--progress=dot:giga","--recursive",sep=" ")
    ## this gives output filenames like "index.html@mosaic=Antarctica.2014003.terra.4km.tif" - would prefer these to be just "Antarctica.*"
    ## note that we can't use --output_document option with --timestamping, but since the server doesn't support timestamping anyway
    dummy$method_flags <- paste0("--output-document=lance-modis.eosdis.nasa.gov/imagery/subsets/",sub("^http.*mosaic=","",this_url))
    if (file.exists(paste0("lance-modis.eosdis.nasa.gov/imagery/subsets/",sub("^http.*mosaic=","",this_url))) & (data_source$clobber<2)) {
        cat(sprintf("not downloading %s, local file exists and clobber setting is <2\n",this_url))
    } else {
        dummy$source_url <- this_url
        bb_wget(dummy)
    }
}
