#' Mirror an external thredds data source using bowerbird's bb_rget utility
#'
#' This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}.
#'
#' @param ... : parameters passed to \code{\link{bb_rget}}
#'
#' @return TRUE on success
#'
#' @seealso \code{\link{bb_rget}}, \code{\link{bb_source}}, \code{\link{bb_sync}}
#'
#' @examples
#' my_source <- bb_source(
#'     name = "OSI SAF Global Low Resolution Sea Ice Drift",
#'     id = "10.15770/EUM_SAF_OSI_NRT_2007",
#'     description = "Example dataset.",
#'     doc_url = "https://osi-saf.eumetsat.int/products/osi-405-c",
#'     ## just the 2009 subset for demo purposes
#'     source_url =
#'     "https://thredds.met.no/thredds/catalog/osisaf/met.no/ice/drift_lr/merged/2009/catalog.html",
#'     citation = "See https://doi.org/10.15770/EUM_SAF_OSI_NRT_2007",
#'     license = "Please cite",
#'     method = list("bb_handler_thredds", level = 2),
#'     access_function = "",
#'     data_group = "Sea ice")
#'
#' \dontrun{
#'   result <- bb_get(my_source, local_file_root = tempdir(), verbose = TRUE)
#' }
#'
#' @export
bb_handler_thredds <- function(...) {
    bb_handler_thredds_inner(...)
}

## @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
## @param verbose logical: if TRUE, provide additional progress output
## @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_thredds_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, ...) {
    assert_that(is(config, "bb_config"))
    assert_that(nrow(bb_data_sources(config)) == 1)
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(local_dir_only), !is.na(local_dir_only))

    cfrow <- bb_settings_to_cols(config)
    this_flags <- list(...)

    ## add flags for clobber behaviour
    if (!is.null(cfrow$clobber) && !is.na(cfrow$clobber)) this_flags$clobber <- cfrow$clobber

    ## add user, password flags
    if (!is.na(cfrow$user) && nzchar(cfrow$user)) this_flags$user <- cfrow$user
    if (!is.na(cfrow$password) && nzchar(cfrow$password)) this_flags$password <- cfrow$password

    ## add global target_s3_args parms that were passed as part of the config to any that were passed as part of this particular data source
    if (!"target_s3_args" %in% names(this_flags)) this_flags$target_s3_args <- list()
    if (length(cfrow$target_s3_args[[1]]) > 0) this_flags$target_s3_args <- c(this_flags$target_s3_args, cfrow$target_s3_args[[1]])

    ## if dry_run, still call bb_rget
    if (!is.null(cfrow[["dry_run"]]) && !is.na(cfrow$dry_run)) this_flags$dry_run <- cfrow$dry_run

    if (local_dir_only) {
        if ("bucket" %in% names(this_flags$target_s3_args)) {
            ## if we are syncing to s3, then there is no concept of the target directory beyond the bucket. Objects in the bucket can be named with directory-like prefixes, but there are no directories in an s3 bucket
            return(get_aws_s3_url(bucket = this_flags$target_s3_args$bucket, region = this_flags$target_s3_args$region, base_url = this_flags$target_s3_args$base_url, path = ""))
        } else {
            mth <- bb_data_sources(config)$method[[1]]
            no_host <- if ("no_host" %in% names(mth)) mth$no_host else FALSE
            cut_dirs <- if ("cut_dirs" %in% names(mth)) mth$cut_dirs else 0L
            return(file.path(bb_settings(config)$local_file_root, directory_from_url(bb_data_sources(config)$source_url, no_host = no_host, cut_dirs = cut_dirs)))
        }
    }

    this_urls <- if (is.list(cfrow$source_url) && length(cfrow$source_url) == 1) cfrow$source_url[[1]] else cfrow$source_url
    this_urls <- sub("catalog\\.html$", "catalog.xml", this_urls) ## we will use the xml, not the html catalog

    ## we could work directly with the catalog xml as an actual xml document, but then we would have to re-implement much of the functionality in bb_rget(). Instead let's treat the xml as html, and use bb_rget with some workarounds for the fact that it's not actually html so the e.g. link syntax is different

    ## the links in the xml document (given as the `urlPath` attribute of each dataset entity) are relative to the thredds base URL plus the HTTPServer service base attribute (typically "/thredds/fileServer/")
    ## for example, a dataset urlPath is:
    ##   osisaf/met.no/ice/drift_lr/merged/2009/12/ice_drift_nh_polstere-625_multi-oi_200912291200-200912311200.nc
    ## extracted from the catalog.xml URL at
    ##   https://thredds.met.no/thredds/catalog/osisaf/met.no/ice/drift_lr/merged/2009/12/catalog.xml
    ## so we need to:
    ##   1. trim the catalog URL back to "https://thredds.met.no", (noting that this might NOT be just the host, there could be an additional path appended to it) ??? No, according to <https://docs.unidata.ucar.edu/tds/current/userguide/basic_client_catalog.html#constructing-an-access-url> there can't be any extra path elements, dataset URLs are always constructed relative to the server root http(s)://hostname:port
    ##   2. append the HTTPServer base attribute ("/thredds/fileServer/") plus the dataset urlPath

    ## we can use rget almost as-is, but we will need to preprocess download links to follow that procedure
    this_flags <- c(list(url = this_urls), this_flags,
                    list(verbose = verbose,
                         link_css = "catalogref, dataset", link_href = c("xlink:href", "urlpath"),
                         accept_follow = "catalog\\.xml", no_parent_download = FALSE,
                         download_link_rewrite = function(x, url, content) {
                             ## find the HTTPServer base attribute
                             ## content is a document as returned by read_html
                             httpserver_base <- tryCatch({
                                 xml2::xml_attr(xml2::xml_find_all(content, ".//service[@name='HTTPServer']"), "base")
                             }, error = function(e) NULL)
                             if (length(httpserver_base) != 1 || is.na(httpserver_base)) httpserver_base <- "/thredds/fileServer/" ## fallback
                             ## find the server root
                             temp <- httr::parse_url(url)
                             temp$path <- temp$query <- temp$params <- temp$fragment <- NULL ## just the scheme, hostname, port (and username, password if set)
                             thredds_base <- sub("/+$", "", httr::build_url(temp))
                             ## finally get the dataset urlpath, which was given directly in the xml but we are working here with absolute URLs that have been constructed from the urlPath plus the catalog url
                             dataset_urlpath <- fs::path_rel(x, start = fs::path_dir(url))
                             paste0(thredds_base, httpserver_base, dataset_urlpath)
                         }
                         ))
    if (!"show_progress" %in% names(this_flags)) this_flags <- c(this_flags, list(show_progress = verbose && sink.number() < 1))
    do.call(bb_rget, this_flags)
}
