#' Handler for files downloaded from the Australian Antarctic Data Centre EDS system
#'
#' AADC EDS files have a URL of the form https://data.aad.gov.au/eds/file/wxyz/ or https://data.aad.gov.au/eds/wxyz/download where wxyz is a numeric file identifier.
#' 
#' @references http://data.aad.gov.au
#' @param data_source tibble: single-row tibble defining a data source, e.g. as returned by \code{bb_source}
#'
#' @return TRUE on success
#'
#' @export
aadc_eds_get <- function(data_source) {
    assert_that(is.data.frame(data_source))
    assert_that(nrow(data_source)==1)
    ## clumsy way to get around AADC EDS file naming issues
    ## e.g. if we ask for http://data.aad.gov.au/eds/file/4494
    ## then we get local file named data.aad.gov.au/eds/file/4494 (which is most likely a zipped file)
    ## if we unzip this here, we get this zip's files mixed with others
    ## change into subdirectory named by file_id of file, so that we don't get files mixed together in data.aad.gov.au/eds/file/
    ## note that this requires the "--recursive" flag NOT TO BE USED
    if (grepl("/download",data_source$source_url)) {
        this_file_id <- str_match(data_source$source_url,"/eds/(\\d+)/download$")[2]
        if (is.na(this_file_id)) stop("could not determine AADC EDS file_id")
        trailing_path <- file.path("data.aad.gov.au","eds",this_file_id)
    } else {
        this_file_id <- str_match(data_source$source_url,"/eds/file/(\\d+)/?$")[2]
        if (is.na(this_file_id)) stop("could not determine AADC EDS file_id")
        trailing_path <- file.path("data.aad.gov.au","eds","file",this_file_id)
        if (!grepl("/$",data_source$source_url)) data_source$source_url <- paste0(data_source$source_url,"/") ## this form needs trailing /
    }
    if (!file.exists(file.path(data_source$local_file_root,trailing_path))) {
        dir.create(file.path(data_source$local_file_root,trailing_path),recursive=TRUE)
    }
    setwd(file.path(data_source$local_file_root,trailing_path))
    if (is.na(data_source$method_flags)) data_source$method_flags <- ""
    ## don't set the --content-disposition flag. It seems to cause problems with used with --timestamping on large files, and
    ##  isn't really needed anyway. Without it, we just get the downloaded file names as "download" (for the /download url form)
    ##  or "index.html" (for the /eds/file/wxyz/ form). But these are still valid zip files
    ##
    ##if (!grepl("--content-disposition",data_source$method_flags,ignore.case=TRUE)) {
    ##    data_source$method_flags <- paste(data_source$method_flags,"--content-disposition",sep=" ")
    ##}
    ## these two should be doable in a single regex, but done separately until I can figure it out
    if (grepl("--recursive ",data_source$method_flags,ignore.case=TRUE)) {
        data_source$method_flags <- str_trim(sub("--recursive ","",data_source$method_flags))
    }
    if (grepl("--recursive$",data_source$method_flags,ignore.case=TRUE)) {
        data_source$method_flags <- str_trim(sub("--recursive$","",data_source$method_flags))
    }
    ok <- bb_wget(data_source)
    setwd(data_source$local_file_root)
    ok
}
