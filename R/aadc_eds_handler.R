#' Handler for files downloaded from the Australian Antarctic Data Centre EDS system
#'
#' @references http://data.aad.gov.au
#' @param data_source tibble: single-row tibble defining a data source, e.g. as returned by \code{bb_source}
#'
#' @return TRUE on success
#'
#' @export
aadc_eds_handler <- function(data_source) {
    assert_that(is.data.frame(data_source))
    assert_that(nrow(data_source)==1)
    ## clumsy way to get around AADC EDS file naming issues
    ## e.g. if we ask for http://data.aad.gov.au/eds/file/4494
    ## then we get local file named data.aad.gov.au/eds/file/4494 (which is most likely a zipped file)
    ## if we unzip this here, we get this zip's files mixed with others
    ## change into subdirectory named by file_id of file, so that we don't get files mixed together in data.aad.gov.au/eds/file/
    ## note that this requires the "--recursive" flag NOT TO BE USED
    this_file_id <- str_match(data_source$source_url,"/file/(\\d+)$")[2]
    if (!file.exists(file.path(data_source$local_file_root,"data.aad.gov.au","eds","file",this_file_id))) {
        dir.create(file.path(data_source$local_file_root,"data.aad.gov.au","eds","file",this_file_id),recursive=TRUE)
    }
    setwd(file.path(data_source$local_file_root,"data.aad.gov.au","eds","file",this_file_id))
    if (!grepl("--content-disposition",data_source$method_flags,ignore.case=TRUE)) {
        data_source$method_flags <- paste(data_source$method_flags,"--content-disposition",sep=" ")
    }
    ## these two should be doable in a single regex, but done separately until I can figure it out
    if (grepl("--recursive ",data_source$method_flags,ignore.case=TRUE)) {
        data_source$method_flags <- str_trim(sub("--recursive ","",data_source$method_flags))
    }
    if (grepl("--recursive$",data_source$method_flags,ignore.case=TRUE)) {
        data_source$method_flags <- str_trim(sub("--recursive$","",data_source$method_flags))
    }
    ##do_wget(build_wget_call(data_source),data_source)
    ok <- webget(data_source)
    setwd(data_source$local_file_root)
    ok
}
