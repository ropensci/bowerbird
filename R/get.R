#' Convenience function to define and synchronize a bowerbird data collection
#'
#' This is a convenience function that provides a shorthand method for synchronizing a small number of data sources. The call \code{bb_get(...)} is roughly equivalent to \code{bb_sync(bb_add(bb_config(...), ...), ...)} (don't take the dots literally here, they are just indicating argument placeholders).
#'
#' Note that the \code{local_file_root} directory must exist or \code{create_root=TRUE} must be passed.
#'
#' @param local_file_root string: location of data repository on local file system
#' @param data_sources tibble: one or more data sources to download, as returned by e.g. \code{bb_example_sources}
#' @param clobber numeric: 0=do not overwrite existing files, 1=overwrite if the remote file is newer than the local copy, 2=always overwrite existing files
#' @param http_proxy string: URL of HTTP proxy to use e.g. 'http://your.proxy:8080' (NULL for no proxy)
#' @param ftp_proxy string: URL of FTP proxy to use e.g. 'http://your.proxy:21' (NULL for no proxy)
#' @param create_root logical: should the data root directory be created if it does not exist? If this is \code{FALSE} (default) and the data root directory does not exist, an error will be generated
#' @param verbose logical: if \code{TRUE}, provide additional progress output
#' @param confirm_downloads_larger_than numeric or NULL: if non-negative, \code{bb_sync} will ask the user for confirmation to download any data source of size greater than this number (in GB). A value of zero will trigger confirmation on every data source. A negative or NULL value will not prompt for confirmation. Note that this only applies when R is being used interactively. The expected download size is taken from the \code{collection_size} parameter of the data source, and so its accuracy is dependent on the accuracy of the data source definition
#' @param dry_run logical: if \code{TRUE}, \code{bb_sync} will do a dry run of the synchronization process without actually downloading files
#' @param ... : additional parameters passed through to \code{bb_config} or \code{bb_sync}
#'
#' @return a tibble, as for \code{\link{bb_sync}}
#'
#' @seealso \code{\link{bb_config}}, \code{\link{bb_example_sources}}, \code{\link{bb_source}}, \code{\link{bb_sync}}
#'
#' @examples
#' \dontrun{
#'   my_source <- bb_example_sources("Australian Election 2016 House of Representatives data")
#'   status <- bb_get(local_file_root = tempdir(), data_sources = my_source, verbose = TRUE)
#'
#'   ## the files that have been downloaded:
#'   status$files[[1]]
#'
#'   ## Define a new source: Geelong bicycle paths from data.gov.au
#'   my_source <- bb_source(
#'     name = "Bike Paths - Greater Geelong",
#'     id = "http://data.gov.au/dataset/7af9cf59-a4ea-47b2-8652-5e5eeed19611",
#'     doc_url = "https://data.gov.au/dataset/geelong-bike-paths",
#'     citation = "See https://data.gov.au/dataset/geelong-bike-paths",
#'     source_url = "https://data.gov.au/dataset/7af9cf59-a4ea-47b2-8652-5e5eeed19611",
#'     license = "CC-BY",
#'     method = list("bb_handler_rget", accept_download = "\\.zip$", level = 1),
#'     postprocess = list("bb_unzip"))
#'
#'   ## get the data
#'   status <- bb_get(data_sources = my_source, local_file_root = tempdir(), verbose = TRUE)
#'
#'   ## find the .shp file amongst the files, and plot it
#'   shpfile <- status$files[[1]]$file[grepl("shp$", status$files[[1]]$file)]
#'   library(sf)
#'   bx <- read_st(shpfile)
#'   plot(bx)
#' }
#' @export
bb_get <- function(data_sources, local_file_root, clobber = 1, http_proxy = NULL, ftp_proxy = NULL, create_root = FALSE, verbose = FALSE, confirm_downloads_larger_than = 0.1, dry_run = FALSE, ...) {
    extra_args <- list(...)
    ## build config
    cf_args <- list(local_file_root = local_file_root, clobber = clobber, http_proxy = http_proxy, ftp_proxy = ftp_proxy)
    if ("wget_global_flags" %in% names(extra_args)) cf_args <- c(cf_args, list(wget_global_flags = extra_args[["wget_global_flags"]]))
    cf <- do.call(bb_config, cf_args)
    ## add source(s)
    cf <- bb_add(cf, data_sources)
    ## sync
    sync_args <- list(config = cf, verbose = verbose, create_root = create_root, confirm_downloads_larger_than = confirm_downloads_larger_than, dry_run = dry_run)
    if ("catch_errors" %in% names(extra_args)) sync_args <- c(sync_args, list(catch_errors = extra_args[["catch_errors"]]))
    do.call(bb_sync, sync_args)
}
