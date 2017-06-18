## list of global flags, stored as attributes on the configuration tibble
## internal function
bb_global_atts <- function() c("wget_default_flags","wget_global_flags","http_proxy","ftp_proxy","local_file_root","clobber")


#' Load or initialize bowerbird configuration
#'
#' If \code{file} is provided to \code{bb_config}, load that configuration. Otherwise initialize an empty configuration.
#' @param file string: file name of configuration to load (for \code{bb_config}) or save to (for \code{bb_save_config}). For \code{bb_config}, all other parameters are ignored if \code{file} is provided
#' @param local_file_root string: location of data repository on local file system
#' @param wget_default_flags string: default flags to be passed to wget. These are overridden on a per-data source basis if the data source defines its own wget_flags
#' @param wget_global_flags string: wget flags that will be applied to all data sources. These will be appended to either the data source wget flags (if specified), or the wget_default_flags
#' @param http_proxy string: URL of HTTP proxy to use e.g. 'http://your.proxy:8080' (NULL for no proxy)
#' @param ftp_proxy string: URL of FTP proxy to use e.g. 'http://your.proxy:21' (NULL for no proxy)
#' @param clobber numeric: 0=do not overwrite existing files, 1=overwrite if the remote file is newer than the local copy, 2=always overwrite existing files. For data sources that use method 'wget', an appropriate flag will be added to the wget call according to the clobber setting ("--no-clobber" to not overwrite existing files, "--timestamping" to overwrite if the remote file is newer than the local copy)
#' @param cf tibble: configuration, as returned by \code{bb_config}
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
#'   bb_save_config(cf,file="my_config.rds")
#'   ## load previously saved config
#'   cf <- bb_config(file="my_config.rds")
#' }
#'
#' @export
bb_config <- function(local_file_root,wget_default_flags=NULL,wget_global_flags="--restrict-file-names=windows --progress=dot:giga",http_proxy=NULL,ftp_proxy=NULL,clobber=1,file) {
    if (!missing(file)) {
        readRDS(file)
    } else {
        cf <- tibble()
        attr(cf,"wget_default_flags") <- wget_default_flags
        attr(cf,"wget_global_flags") <- wget_global_flags
        attr(cf,"http_proxy") <- http_proxy
        attr(cf,"ftp_proxy") <- ftp_proxy
        attr(cf,"local_file_root") <- local_file_root
        attr(cf,"clobber") <- clobber
        ## do we need user, password, skip_downloads?
        cf
    }
}

#' @rdname bb_config
#' @export
bb_save_config <- function(cf,file) {
    assert_that(is.string(file))
    saveRDS(cf,file=file)
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
    ## check that name is unique?
    dplyr::bind_rows(cf,source)
}

