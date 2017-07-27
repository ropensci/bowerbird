#' Handler for AMPS data (Antarctic mesoscale prediction system)
#'
#' @references http://www2.mmm.ucar.edu/rt/amps/
#' @param data_source data.frame: single-row data.frame defining a data source, e.g. as returned by \code{bb_source}
#' @param verbose logical: if TRUE, provide additional progress output
#' @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
#'
#' @return the directory if local_dir_only is TRUE, otherwise TRUE on success
#'
#' @export
amps_get <- function(data_source,verbose=FALSE,local_dir_only=FALSE) {
    assert_that(is.data.frame(data_source))
    assert_that(nrow(data_source)==1)
    assert_that(is.flag(verbose))
    assert_that(is.flag(local_dir_only))
    ## shouldn't need any specific method_flags for this
    ## could potentially set e.g. --progress=dot:giga
    ## --timestamping not needed (handled through clobber config setting)
    ## --recursive, etc not needed
    if (sub("/$","",data_source$source_url)!="http://www2.mmm.ucar.edu/rt/amps/wrf_grib") {
        stop(sprintf("source_url (%s) not as expected, not processing.\n",data_source$source_url))
    }
    if (local_dir_only) return(bb_wget(data_source,verbose=verbose,local_dir_only=TRUE))
    x <- html_session(data_source$source_url)
    n <- html_attr(html_nodes(x,"a"),"href")
    idx <- which(sapply(n,function(z)grepl("[[:digit:]]+",z,ignore.case=TRUE))) ## links that are all digits
    accept <- function(z) grepl("\\.txt$",html_attr(z,"href"),ignore.case=TRUE) || grepl("d[12]_f(000|003|006|009|012|015|018|021|024|027)\\.grb$",html_attr(z,"href"),ignore.case=TRUE) ## which files to accept
    this_path_no_trailing_sep <- sub("[\\/]$","",directory_from_url(data_source$source_url))
    for (i in idx) { ## loop through directories
        target_dir <- sub("/$","",n[i])
        target_dir <- file.path(this_path_no_trailing_sep,sub("(00|12)$","",target_dir))
        ## make target_dir if it doesn't exist
        if (!dir.exists(target_dir)) {
            ok <- dir.create(target_dir)
            if (!ok) {
                stop(sprintf("Could not create target directory %s: aborting.\n",target_dir))
            }
        }
        x2 <- jump_to(x,n[i])
        files <- html_attr(Filter(accept,html_nodes(x2,"a")),"href")
        ## change into target directory, with no recursive fetch, to allow --timestamping on retrievals
        cwd <- getwd()
        setwd(target_dir)
        for (f in files) {
            ## loop through files to download
            file_url <- xml2::url_absolute(f,x2$url)
            dummy <- data_source
            dummy$source_url <- file_url
            bb_wget(dummy,verbose=verbose)
        }
        setwd(cwd)
    }
}
