#' Handler for public AWS S3 data sources
#'
#' This is a handler function to be used with AWS S3 data providers. This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}. Note that this currently only works with public data sources that are accessible without an S3 key.
#' The method arguments accepted by \code{bb_handler_aws_s3} are currently:
#' \itemize{
#'   \item "bucket" string: name of the bucket (defaults to "")
#'   \item "base_url" string: as for \code{\link[aws.s3]{s3HTTP}}
#'   \item "region" string: as for \code{\link[aws.s3]{s3HTTP}}
#'   \item "use_https" logical: as for \code{\link[aws.s3]{s3HTTP}}
#'   \item "prefix" string: as for \code{\link[aws.s3]{get_bucket}}; only keys in the bucket that begin with the specified prefix will be processed
#'   \item and other parameters passed to the \code{\link{bb_rget}} function, including "accept_download", "accept_download_extra", "reject_download"
#' }
#' Note that the "prefix", "accept_download", "accept_download_extra", "reject_download" parameters can be used to restrict which files are downloaded from the bucket.
#'
#' @param ... : parameters, see Description
#'
#' @return A tibble with columns \code{ok}, \code{files}, \code{message}
#' @examples
#' \dontrun{
#'   ## an example AWS S3 data source
#'   src <- bb_source(
#'            name = "SILO climate data",
#'            id = "silo-open-data",
#'            description = "Australian climate data from 1889 to yesterday.
#'                           This source includes a single example monthly rainfall data file.
#'                           Adjust the 'accept_download' parameter to change this.",
#'            doc_url = "https://www.longpaddock.qld.gov.au/silo/gridded-data/",
#'            citation = "SILO datasets are constructed by the Queensland Government using
#'                        observational data provided by the Australian Bureau of Meteorology
#'                        and are available under the Creative Commons Attribution 4.0 license",
#'            license = "CC-BY 4.0",
#'            method = list("bb_handler_aws_s3", region = "silo-open-data.s3",
#'                          base_url = "amazonaws.com", prefix = "annual/monthly_rain/",
#'                          accept_download = "2005\\.monthly_rain\\.nc$"),
#'            comment = "The unusual specification of region and base_url is a workaround for
#'                       an aws.s3 issue, see https://github.com/cloudyr/aws.s3/issues/318",
#'            postprocess = NULL,
#'            collection_size = 0.02,
#'            data_group = "Climate")
#'    temp_root <- tempdir()
#'    status <- bb_get(src, local_file_root = temp_root, verbose = TRUE)
#' }
#'
#' @export
bb_handler_aws_s3 <- function(...) {
    if (!requireNamespace("aws.s3", quietly = TRUE))
        stop("the aws.s3 package is needed if you want to use `bb_handler_aws_s3`. Install it with `install.packages('aws.s3')`")
    bb_handler_aws_s3_inner(...)
}

# @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
# @param verbose logical: if TRUE, provide additional progress output
# @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
#
# @return TRUE on success or the directory name if local_dir_only is TRUE
bb_handler_aws_s3_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, ...) {

    assert_that(is(config,"bb_config"))
    assert_that(nrow(bb_data_sources(config))==1)
    assert_that(is.flag(verbose),!is.na(verbose))
    assert_that(is.flag(local_dir_only),!is.na(local_dir_only))

    myargs <- list(...)
    if (is.null(myargs[["accept_download"]])) myargs$accept_download <- bowerbird::bb_rget_default_downloads()

    s3args <- myargs
    if (is.null(s3args$bucket)) s3args$bucket <- ""
    ##if (!"max" %in% names(s3args)) s3args$max <- 1000L
    ## args to pass to all s3HTTP calls
    s3HTTP_args <- s3args[intersect(names(s3args), c("bucket", "base_url", "region", "use_https"))]
    ## "key" and "secret" could go here but aren't (yet) propagated into the rget calls below

    if (local_dir_only) {
        if (TRUE) {
            ## use get_aws_s3_url, but note that this relies on the unexported aws.s3:::setup_s3_url function
            this_url <- file.path(do.call(get_aws_s3_url, c(list(path = paste0("/", s3args$prefix)), s3HTTP_args)), "dummy")
        } else {
            ## make an s3HTTP call and get its URL
            query <- s3args[names(s3args) %in% c("prefix", "delimiter", "marker", "use_https")]
            query$`max-keys` <- 0L
            r <- do.call(aws.s3::s3HTTP, c(list(verb = "GET", query = query, parse_response = FALSE), s3HTTP_args))
            this_endpoint <- httr::parse_url(r$url)
            this_endpoint$path <- gsub("//", "/", file.path(this_endpoint$path, this_endpoint$query$prefix, "dummy")) ## "dummy" to represent objects within this prefix space
            this_endpoint$query <- NULL
            this_url <- httr::build_url(this_endpoint)
        }
        config$data_sources <- bb_modify_source(config$data_sources, method = list("bb_rget"), source_url = this_url)
        return(bb_handler_rget(config, verbose = verbose, local_dir_only = TRUE))
    }

    ## get list of objects in bucket that match our specs
    bx <- do.call(aws.s3::get_bucket, c(s3args[intersect(names(s3args), c("max", "prefix"))], s3HTTP_args, list(verbose = TRUE)))
    ## apply accept_download etc filters
    idx <- rep(TRUE, length(bx))
    if (!is.null(myargs[["accept_download"]]) && length(myargs[["accept_download"]]) > 0) {
        idx <- vapply(bx, function(z) {
            this_url <- do.call(get_aws_s3_url, c(list(path = paste0("/", z$Key)), s3HTTP_args))
            all(vapply(myargs[["accept_download"]], function(rgx) grepl(rgx, this_url), FUN.VALUE = TRUE))
        }, FUN.VALUE = TRUE)
    }
    if (!is.null(myargs[["accept_download_extra"]]) && length(myargs[["accept_download_extra"]]) > 0) {
        idx <- idx | vapply(bx, function(z) {
            this_url <- do.call(get_aws_s3_url, c(list(path = paste0("/", z$Key)), s3HTTP_args))
            all(vapply(myargs[["accept_download_extra"]], function(rgx) grepl(rgx, this_url), FUN.VALUE = TRUE))
        }, FUN.VALUE = TRUE)
    }
    bx <- bx[idx]
    if (!is.null(myargs[["reject_download"]]) && length(myargs[["reject_download"]]) > 0) {
        idx <- vapply(bx, function(z) {
            this_url <- do.call(get_aws_s3_url, c(list(path = paste0("/", z$Key)), s3HTTP_args))
            !any(vapply(myargs[["reject_download"]], function(rgx) grepl(rgx, this_url), FUN.VALUE = TRUE))
        }, FUN.VALUE = TRUE)
        bx <- bx[idx]
    }
    status <- tibble(ok = TRUE, files = list(tibble(url = character(), file = character(), was_downloaded = logical())), message = "")
    ## note that this gives a sequence of "downloading file 1 of 1" messages, rather than "1 of x", "2 of x", etc
    for (oi in seq_along(bx)) {
        ## figure out the actual URL (and destination path?) for each object and hand that to bb_rget via a dummy config
        ## asking for options will give us the URL without actually downloading anything substantial
        ##this_url <- do.call(aws.s3::s3HTTP, c(list(verb = "OPTIONS", parse_response = FALSE, path = paste0("/", bx[[oi]]$Key)), s3HTTP_args))$url
        this_url <- do.call(get_aws_s3_url, c(list(path = paste0("/", bx[[oi]]$Key)), s3HTTP_args))
        dummy <- config
        dummy$data_sources$method[[1]] <- list("bb_rget")
        dummy$data_sources$source_url <- this_url
        rget_args <- c(list(dummy, verbose = verbose), myargs[intersect(names(myargs), names(formals("bb_rget")))])
        this_status <- do.call(bb_handler_rget, rget_args)
        status <- tibble(ok = status$ok && this_status$ok, files = list(rbind(status$files[[1]], this_status$files[[1]])), message = paste(status$message, this_status$message))
    }
    status
}

get_aws_s3_url <- function(bucket, region = NULL, path, url_style = "path", base_url, verbose = FALSE, use_https = TRUE) {
    if (missing(base_url)) {
        aws.s3:::setup_s3_url(bucketname = bucket, region = region, path = path, url_style = url_style, verbose = verbose, use_https = use_https)
    } else {
        aws.s3:::setup_s3_url(bucketname = bucket, region = region, path = path, url_style = url_style, base_url = base_url, verbose = verbose, use_https = use_https)
    }
}
