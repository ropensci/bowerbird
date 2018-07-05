#' Mirror an external data source using bowerbird's bb_rget utility
#'
#' This is a general handler function that is suitable for a range of data sets. This function is not intended to be called directly, but rather is specified as a \code{method} option in \code{\link{bb_source}}.
#'
#' This handler function makes calls to the \code{\link{bb_rget}} function. Arguments provided to \code{bb_handler_rget} are passed through to \code{\link{bb_rget}}.
#'
#' @param ... : parameters passed to \code{\link{bb_rget}}
#'
#' @return TRUE on success
#'
#' @seealso \code{\link{bb_rget}}, \code{\link{bb_source}}
#' @examples
#'
#' my_source <- bb_source(
#'    id = "gshhg_coastline",
#'    name = "GSHHG coastline data",
#'    description = "A Global Self-consistent, Hierarchical, High-resolution Geography Database",
#'    doc_url = "http://www.soest.hawaii.edu/pwessel/gshhg",
#'    citation = "Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
#'      High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
#'    source_url = "ftp://ftp.soest.hawaii.edu/gshhg/*",
#'    license = "LGPL",
#'    method = list("bb_handler_rget", recursive = TRUE, level = 1,
#'                  accept_download = "bin.+\\.zip|README.TXT"),
#'    postprocess = list("bb_unzip"),
#'    collection_size = 0.6)
#'
#' @export
bb_handler_rget <- function(...) {
    bb_handler_rget_inner(...)
}

# @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
# @param verbose logical: if TRUE, provide additional progress output
# @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_rget_inner <- function(config, verbose = FALSE, local_dir_only = FALSE, ...) {
    assert_that(is(config,"bb_config"))
    assert_that(nrow(bb_data_sources(config)) == 1)
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(local_dir_only), !is.na(local_dir_only))

    if (local_dir_only)
        return(file.path(bb_settings(config)$local_file_root,directory_from_url(bb_data_sources(config)$source_url)))

    cfrow <- bb_settings_to_cols(config)
    this_flags <- list(...)

    ## add flags for clobber behaviour
    if (!is.null(cfrow$clobber) && !is.na(cfrow$clobber)) {
        this_flags$clobber <- cfrow$clobber
    }
    ## add user, password flags
    if (!is.na(cfrow$user) && nchar(cfrow$user)>0) this_flags <- c(this_flags,list(user=cfrow$user))
    if (!is.na(cfrow$password) && nchar(cfrow$password)>0) this_flags <- c(this_flags,list(password=cfrow$password))

    ##if (!is.null(cfrow$dry_run) && cfrow$dry_run) {
    ##    if (verbose) {
    ##        temp <- vapply(seq_len(length(this_flags)),function(z)paste0(names(this_flags)[z],"=",this_flags[z]),FUN.VALUE="",USE.NAMES=FALSE)
    ##        cat(sprintf(" dry_run is TRUE, not executing bb_rget with parameters: url=\"%s\", %s\n",cfrow$source_url,paste(temp,collapse=", ")))
    ##    }
    ##    ok <- TRUE
    ##} else {

    ## if dry_run, still call bb_rget
    if (!is.null(cfrow$dry_run) && !is.na(cfrow$dry_run)) {
        this_flags$dry_run <- cfrow$dry_run
    }
    this_flags <- c(list(url = cfrow$source_url), this_flags, list(verbose = verbose))
    do.call(bb_rget, this_flags)
}


#' A wget-like recursive download utility
#'
#' This function provides similar, but simplified, functionality to the the command-line \code{wget} utility. It is based on the \code{rvest} package.
#'
#' NOTE: this is still highly experimental.
#'
#' @param url string: the URL to retrieve
#' @param level integer >=0: recursively download to this maximum depth level. Specify 0 for no recursion
#' @param wait numeric >=0: wait this number of seconds between successive retrievals. This option may help with servers that block multiple successive requests, by introducing a delay between requests
#' @param accept_follow character: character vector with one or more entries. Each entry specifies a regular expression that is applied to the complete URL. URLs matching all entries will be followed during the spidering process
#' @param reject_follow character: as for \code{accept_follow}, but specifying URL regular expressions to reject
#' @param accept_download character: character vector with one or more entries. Each entry specifies a regular expression that is applied to the complete URL. Matching URLs will be accepted for download
#' @param reject_download character: as for \code{accept_regex}, but specifying URL regular expressions to reject
#' @param user string: username used to authenticate to the remote server
#' @param password string: password used to authenticate to the remote server
#' @param clobber numeric: 0=do not overwrite existing files, 1=overwrite if the remote file is newer than the local copy, 2=always overwrite existing files
#' @param no_check_certificate logical: if \code{TRUE}, don't check the server certificate against the available certificate authorities. Also don't require the URL host name to match the common name presented by the certificate. This option might be useful if trying to download files from a server with an expired certificate, but it is clearly a security risk and so should be used with caution
#' @param verbose logical: print trace output?
#' @param show_progress logical: if \code{TRUE}, show download progress
#' @param debug logical: if \code{TRUE}, will print additional debugging information. If bb_rget is not behaving as expected, try setting this to \code{TRUE}
#' @param dry_run logical: if \code{TRUE}, spider the remote site and work out which files would be downloaded, but don't download them
#'
#' @return a list with components 'ok' (TRUE/FALSE) and 'download_files'
#'
# @export
bb_rget <- function(url, level = 0, wait = 0, accept_follow = c("(/|\\.html?)$"), reject_follow = character(), accept_download = c("\\.(asc|csv|nc|bin|txt|gz|bz|bz2|Z|zip|kmz|kml)$"), reject_download = character(), user, password, clobber = 1, no_check_certificate = FALSE, verbose = FALSE, show_progress = verbose, debug = FALSE, dry_run = FALSE) {
    ## TO ADD: no_parent probably wise
    assert_that(is.string(url))
    assert_that(is.numeric(level), level >= 0)
    assert_that(is.character(accept_follow))
    assert_that(is.character(reject_follow))
    assert_that(is.character(accept_download))
    assert_that(is.character(reject_download))
    if (missing(user)) user <- NA_character_
    assert_that(is.string(user))
    if (missing(password)) password <- NA_character_
    assert_that(is.string(password))
    assert_that(clobber %in% c(0, 1, 2))
    assert_that(is.flag(no_check_certificate), !is.na(no_check_certificate))
    assert_that(is.numeric(wait))
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(debug), !is.na(debug))
    assert_that(is.flag(dry_run), !is.na(dry_run))
    ## opts to pass to child function
    opts <- list(level = level, accept_follow = accept_follow, reject_follow = reject_follow, accept_download = accept_download, reject_download = reject_download, wait = wait, verbose = verbose, show_progress = show_progress) ##robots_off = robots_off,
    ## curl options
    ## user and password
    ##username                   CURLOPT_USERNAME   string
    ##userpwd                    CURLOPT_USERPWD   string
    ##password                   CURLOPT_PASSWORD   string
    opts$curl_config <- if (debug) httr::verbose() else httr::config() ## curl's verbose output is intense, save it for debug = TRUE
    if (show_progress) opts$curl_config$options <- c(opts$curl_config$options, httr::progress()$options)
    if (no_check_certificate) {
            temp <- opts$curl_config$options
            temp$ssl_verifypeer = 0L
            ##temp$ssl_verifyhost = 0L ## does not seem to work
            opts$curl_config$options <- temp
    }
    ok <- FALSE
    downloads <- tibble(url = character(), file = character(), was_downloaded = logical())
    tryCatch({
        rec <- spider(to_visit = url, opts = opts)
        ## download each file, or not depending on clobber behaviour
        ## if doing a dry run no download, but do enumerate the list of files that would be downloaded
        downloads <- tibble(url = unique(rec$download_queue), file = NA_character_, was_downloaded = FALSE)
        if (dry_run) {
            if (verbose && length(rec$download_queue) > 0) {
                cat(sprintf(" dry_run is TRUE, bb_rget is not downloading the following files:\n %s\n", paste(rec$download_queue, collapse="\n ")))
            }
        } else {
            ## download each file
            ## keep track of which were actually downloaded
            for (df in downloads$url) {
                mydir <- directory_from_url(df)
                if (!dir.exists(mydir)) dir.create(mydir, recursive = TRUE)
                fname <- file.path(mydir, basename(df))
                downloads$file[downloads$url == df] <- fname
                do_download <- clobber >= 1 || (!file.exists(fname))
                ## if clobber == 1, we set the if-modified-since option, so we can ask for download and it will not re-download unless needed
                if (do_download) {
                    if (verbose) cat(sprintf("downloading file: %s ...\n", df))
                    myopts <- opts$curl_config ## curl options for this particular file, may be modified below depending on clobber
                    if (file.exists(fname)) {
                        if (clobber == 1) {
                            ## timestamping via curl if-modified-since option
                            ## if this doesn't work, do a head request to get file modified time, and compare explicitly to local file
                            myopts$options$timevalue <- as.numeric(file.info(fname)$mtime)
                            myopts$options$timecondition <- 3L ## CURL_TIMECOND_IFMODSINCE is value 3
                        }
                    }
                    ## need to download to temp file, because a file of zero bytes will be written if not if-modified-since
                    ## this is all very inelegant
                    dlf <- tempfile()
                    file.copy(fname, dlf)
                    httr::with_config(myopts, httr::GET(df, write_disk(path = dlf, overwrite = TRUE)))
                    ## now if the file wasn't re-downloaded, dlf will be zero bytes
                    if (file.exists(dlf) && file.info(dlf)$size > 0) {
                        ## file was updated
                        downloads$was_downloaded[downloads$url == df] <- TRUE
                        if (file.exists(fname)) file.remove(fname)
                        file.rename(dlf, fname)
                        if (verbose) cat(if (show_progress) "\n", "done.\n")
                    } else {
                        file.remove(dlf)
                        if (verbose) cat(if (show_progress) "\n", "file unchanged on server, not downloading.\n")
                    }
                }
            }
        }
        ok <- TRUE
    }, error = function(e) {
        if (verbose) {
            ## echo the error message but don't throw it as a full blown error
            ## what happens when download is interrupted
            ## often these are unimportant (e.g. 404 response codes during recursion) so we don't want to halt the entire process
            cat(sprintf(" bb_rget exited with an error (%s)\n", e$message))
        }
    })
    list(ok = ok, download_files = downloads)
}

spider <- function(to_visit, visited = character(), download_queue = character(), opts, current_level = 0) {
    to_visit <- to_visit[!to_visit %in% visited]
    if (length(to_visit) < 1) return(list(visited = visited, download_queue = download_queue))
#    httr_opts <- httr_options()
#    on.exit(httr::set_config(httr_opts))
#    httr::set_config(opts$curl_config)
#    cat(str(options("httr_config")))
    next_level_to_visit <- character()
    first_req <- TRUE
    for (url in to_visit) {
        if (first_req && current_level < 1) {
            first_req <- FALSE
        } else {
            if (opts$wait > 0) Sys.sleep(opts$wait)
        }
        if (opts$verbose) cat(sprintf(" visiting %s ...\n", url))
        httr::with_config(opts$curl_config, {
            ##cat(str(options("httr_config")))
            x <- read_html(content(GET(url), as = "text"))
            ##x <- read_html(url) ## doesn't seem to honour the ssl_verifypeer option
            if (opts$verbose && opts$show_progress) cat("\n")
        })
        ## should we also be writing this file to disk?
        ## get all links as absolute URLs, discarding anchors (fragments)
        all_links <- unique(na.omit(vapply(html_nodes(x, "a"), function(z) clean_and_filter_url(xml2::url_absolute(html_attr(z, "href"), url)), FUN.VALUE = "", USE.NAMES = FALSE)))
        follow_idx <- rep(TRUE, length(all_links))
        for (rgx in opts$accept_follow) follow_idx <- follow_idx & vapply(all_links, function(z) grepl(rgx, z), FUN.VALUE = TRUE, USE.NAMES = FALSE)
        for (rgx in opts$reject_follow) follow_idx <- follow_idx & !vapply(all_links, function(z) grepl(rgx, z), FUN.VALUE = TRUE, USE.NAMES = FALSE)
        follow_links <- all_links[follow_idx]
        download_idx <- rep(TRUE, length(all_links))
        for (rgx in opts$accept_download) download_idx <- download_idx & vapply(all_links, function(z) grepl(rgx, z), FUN.VALUE = TRUE, USE.NAMES = FALSE)
        for (rgx in opts$reject_download) download_idx <- download_idx & !vapply(all_links, function(z) grepl(rgx, z), FUN.VALUE = TRUE, USE.NAMES = FALSE)
        download_links <- all_links[download_idx]
        download_links <- download_links[!download_links %in% download_queue]
        if (opts$verbose) cat(sprintf(" %d download links", length(download_links)))
        if (current_level < (opts$level - 1)) { ## -1 because we will download files linked from these pages, and those files will be current_level + 2
            follow_links <- setdiff(follow_links, download_links) ## can't be in both, treat as download?
            follow_links <- follow_links[!follow_links %in% visited] ## discard any already visited
            if (opts$verbose) cat(sprintf(", %d links to visit", length(follow_links)))
            next_level_to_visit <- c(next_level_to_visit, follow_links) ## add to list to visit at next recursion level
        }
        ##    lh <- lapply(all_links, httr::HEAD)
        ## all download_links to download_queue for later downloading
        download_queue <- c(download_queue, download_links)
        if (opts$verbose) cat(" ... done\n")
    }
    visited <- c(visited, to_visit)
    ## recurse to next level
    spider(next_level_to_visit, visited = visited, download_queue = download_queue, opts = opts, current_level = current_level + 1)
}

clean_and_filter_url <- function(url, accept_schemes = c("https", "http", "ftp")) {
    if (!is.string(url) || !nzchar(url) || is.na(url)) return(NA_character_)
    temp <- httr::parse_url(url)
    temp$fragment <- NULL ## discard fragment
    if (temp$scheme %in% accept_schemes) httr::build_url(temp) else NA_character_
}

