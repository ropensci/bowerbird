## list of global flags, stored as attributes on the configuration tibble
## internal function
bb_global_atts <- function() c("wget_default_flags","wget_global_flags","http_proxy","ftp_proxy","local_file_root","clobber","skip_downloads")


#' Initialize a bowerbird configuration
#'
#' @param local_file_root string: location of data repository on local file system
#' @param wget_default_flags string: default flags to be passed to wget. These are overridden on a per-data source basis if the data source defines its own wget_flags
#' @param wget_global_flags string: wget flags that will be applied to all data sources. These will be appended to either the data source wget flags (if specified), or the wget_default_flags
#' @param http_proxy string: URL of HTTP proxy to use e.g. 'http://your.proxy:8080' (NULL for no proxy)
#' @param ftp_proxy string: URL of FTP proxy to use e.g. 'http://your.proxy:21' (NULL for no proxy)
#' @param clobber numeric: 0=do not overwrite existing files, 1=overwrite if the remote file is newer than the local copy, 2=always overwrite existing files. For data sources that use method 'wget', an appropriate flag will be added to the wget call according to the clobber setting ("--no-clobber" to not overwrite existing files, "--timestamping" to overwrite if the remote file is newer than the local copy)
#' @param skip_downloads logical: if TRUE, \code{bb_sync} will do a dry run of the synchronisation process but without actually downloading files. For data sources using method bb_wget, this means that the wget calls will not be executed, so e.g. any recursion handled by wget itself will not be simulated
#' @return configuration tibble
#'
#' @seealso \code{\link{bb_source}}
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     add(bb_source("seaice_smmr_ssmi_nasateam"))
#' 
#'   ## save to file
#'   saveRDS(cf,file="my_config.rds")
#'   ## load previously saved config
#'   cf <- readRDS(file="my_config.rds")
#' }
#'
#' @export
bb_config <- function(local_file_root,wget_default_flags=NULL,wget_global_flags="--restrict-file-names=windows --progress=dot:giga",http_proxy=NULL,ftp_proxy=NULL,clobber=1,skip_downloads=FALSE) {
    assert_that(is.string(local_file_root))
    assert_that(clobber %in% c(0,1,2))
    assert_that(is.flag(skip_downloads))
    cf <- tibble()
    attr(cf,"wget_default_flags") <- wget_default_flags
    attr(cf,"wget_global_flags") <- wget_global_flags
    attr(cf,"http_proxy") <- http_proxy
    attr(cf,"ftp_proxy") <- ftp_proxy
    attr(cf,"local_file_root") <- local_file_root
    attr(cf,"clobber") <- clobber
    attr(cf,"skip_downloads") <- skip_downloads
    cf
}

#' Add a new data source to a bowerbird configuration
#' 
#' @param cf tibble: configuration, as returned by \code{bb_config}
#' @param source tibble: data source definition to add to the configuration, as returned by \code{bb_source}
#'
#' @return configuration tibble
#'
#' @seealso \code{\link{bb_source}} \code{\link{bb_config}}
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     add(bb_source("seaice_smmr_ssmi_nasateam"))
#' }
#' @export
add <- function(cf,source) {
    copy_bb_attributes(dplyr::bind_rows(cf,source),cf)
}


#' Returns a configuration object's bowerbird attributes
#'
#' @param cf tibble: configuration, as returned by \code{bb_config}
#'
#' @return named list
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#' cf <- bb_config(local_file_root="/your/data/directory")
#' bb_attributes(cf)
#'
#' @export
bb_attributes <- function(cf) {
    out <- attributes(cf)
    out[names(out) %in% bb_global_atts()]
}

## helper functions to manage attributes
## copy attributes
copy_bb_attributes <- function(to,from) {
    attributes(to) <- c(attributes(to),bb_attributes(from))
    to
}

## subset one or more rows
bb_slice <- function(obj,rows) copy_bb_attributes(obj[rows,],obj)

## copy each bb attribute into column
bb_attributes_to_cols <- function(obj) {
    for (nm in bb_global_atts()) {
        if (!is.null(attr(obj,nm))) obj[,nm] <- attr(obj,nm)
    }
    obj
}
