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
#' @seealso \code{\link{bb_rget}}, \code{\link{bb_source}}, \code{\link{bb_sync}}
#'
#' @examples
#' my_source <- bb_source(
#'    name = "Australian Election 2016 House of Representatives data",
#'    id = "aus-election-house-2016",
#'    description = "House of Representatives results from the 2016 Australian election.",
#'    doc_url = "http://results.aec.gov.au/",
#'    citation = "Copyright Commonwealth of Australia 2017. As far as practicable, material for
#'                which the copyright is owned by a third party will be clearly labelled. The
#'                AEC has made all reasonable efforts to ensure that this material has been
#'                reproduced on this website with the full consent of the copyright owners.",
#'    source_url = "http://results.aec.gov.au/20499/Website/HouseDownloadsMenu-20499-Csv.htm",
#'    license = "CC-BY",
#'    method = list("bb_handler_rget", level = 1, accept_download = "csv$"),
#'    collection_size = 0.01)
#'
#' my_data_dir <- tempdir()
#' cf <- bb_config(my_data_dir)
#' cf <- bb_add(cf, my_source)
#'
#' \dontrun{
#' bb_sync(cf, verbose = TRUE)
#' }
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

    if (local_dir_only) {
        mth <- bb_data_sources(config)$method[[1]]
        no_host <- if ("no_host" %in% names(mth)) mth$no_host else FALSE
        cut_dirs <- if ("cut_dirs" %in% names(mth)) mth$cut_dirs else 0L
        return(file.path(bb_settings(config)$local_file_root,directory_from_url(bb_data_sources(config)$source_url, no_host = no_host, cut_dirs = cut_dirs)))
    }

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
    if (!"show_progress" %in% names(this_flags)) this_flags <- c(this_flags, list(show_progress = verbose && sink.number() < 1))
    do.call(bb_rget, this_flags)
}


#' A recursive download utility
#'
#' This function provides similar, but simplified, functionality to the the command-line \code{wget} utility. It is based on the \code{rvest} package.
#'
#' NOTE: this is still somewhat experimental.
#'
#' @param url string: the URL to retrieve
#' @param level integer >=0: recursively download to this maximum depth level. Specify 0 for no recursion
#' @param wait numeric >=0: wait this number of seconds between successive retrievals. This option may help with servers that block users making too many requests in a short period of time
#' @param accept_follow character: character vector with one or more entries. Each entry specifies a regular expression that is applied to the complete URL. URLs matching all entries will be followed during the spidering process. Note that the first URL (provided via the \code{url} parameter) will always be visited, unless it matches the download criteria
#' @param reject_follow character: as for \code{accept_follow}, but specifying URL regular expressions to reject
#' @param accept_download character: character vector with one or more entries. Each entry specifies a regular expression that is applied to the complete URL. URLs that match all entries will be accepted for download. By default the \code{accept_download} parameter is that returned by \code{bb_rget_default_downloads}: use \code{bb_rget_default_downloads()} to see what that is
#' @param accept_download_extra character: character vector with one or more entries. If provided, URLs will be accepted for download if they match all entries in \code{accept_download} OR all entries in \code{accept_download_extra}. This is a convenient method to add one or more extra download types, without needing to re-specify the defaults in \code{accept_download}
#' @param reject_download character: as for \code{accept_regex}, but specifying URL regular expressions to reject
#' @param user string: username used to authenticate to the remote server
#' @param password string: password used to authenticate to the remote server
#' @param clobber numeric: 0=do not overwrite existing files, 1=overwrite if the remote file is newer than the local copy, 2=always overwrite existing files
#' @param no_parent logical: if \code{TRUE}, do not ever ascend to the parent directory when retrieving recursively. This is \code{TRUE} by default, bacause it guarantees that only the files below a certain hierarchy will be downloaded. Note that this check only applies to links on the same host as the starting \code{url}. If that URL links to files on another host, those links will be followed (unless \code{relative = TRUE})
#' @param no_check_certificate logical: if \code{TRUE}, don't check the server certificate against the available certificate authorities. Also don't require the URL host name to match the common name presented by the certificate. This option might be useful if trying to download files from a server with an expired certificate, but it is clearly a security risk and so should be used with caution
#' @param relative logical: if \code{TRUE}, only follow relative links. This can be useful for restricting what is downloaded in recursive mode
#' @param remote_time logical: if \code{TRUE}, attempt to set the local file's time to that of the remote file
#' @param verbose logical: print trace output?
#' @param show_progress logical: if \code{TRUE}, show download progress
#' @param debug logical: if \code{TRUE}, will print additional debugging information. If bb_rget is not behaving as expected, try setting this to \code{TRUE}
#' @param dry_run logical: if \code{TRUE}, spider the remote site and work out which files would be downloaded, but don't download them
#' @param stop_on_download_error logical: if \code{TRUE}, the download process will stop if any file download fails. If \code{FALSE}, the process will issue a warning and continue to the next file to download
#' @param force_local_filename string: if provided, then the \code{url} will be treated as a single URL (no recursion will be conducted). It will be downloaded to a file with this name, in a local directory determined by the \code{url}
#' @param use_url_directory logical: if \code{TRUE}, files will be saved into a local directory that follows the URL structure (e.g. files from \code{http://some.where/place} will be saved into directory \code{some.where/place}). If \code{FALSE}, files will be saved into the current directory
#' @param no_host logical: if \code{use_url_directory = TRUE}, specifying \code{no_host = TRUE} will remove the host name from the directory (e.g. files from files from \code{http://some.where/place} will be saved into directory \code{place})
#' @param cut_dirs integer: if \code{use_url_directory = TRUE}, specifying \code{cut_dirs} will remove this many directory levels from the path of the local directory where files will be saved (e.g. if \code{cut_dirs = 2}, files from \code{http://some.where/place/baa/haa} will be saved into directory \code{some.where/haa}. if \code{cut_dirs = 1} and \code{no_host = TRUE}, files from \code{http://some.where/place/baa/haa} will be saved into directory \code{baa/haa})
#'
#' @return a list with components 'ok' (TRUE/FALSE), 'files', and 'message' (error or other messages)
#'
#' @export
bb_rget <- function(url, level = 0, wait = 0, accept_follow = c("(/|\\.html?)$"), reject_follow = character(), accept_download = bb_rget_default_downloads(), accept_download_extra = character(), reject_download = character(), user, password, clobber = 1, no_parent = TRUE, no_check_certificate = FALSE, relative = FALSE, remote_time = TRUE, verbose = FALSE, show_progress = verbose, debug = FALSE, dry_run = FALSE, stop_on_download_error = FALSE, force_local_filename, use_url_directory = TRUE, no_host = FALSE, cut_dirs = 0L) {
    ## TO ADD: no_parent probably wise
    assert_that(is.string(url))
    assert_that(is.numeric(level), level >= 0)
    assert_that(is.character(accept_follow))
    assert_that(is.character(reject_follow))
    assert_that(is.character(accept_download))
    assert_that(is.character(accept_download_extra))
    assert_that(is.character(reject_download))
    if (missing(user)) user <- NA_character_
    assert_that(is.string(user))
    if (missing(password)) password <- NA_character_
    assert_that(is.string(password))
    assert_that(clobber %in% c(0, 1, 2))
    assert_that(is.flag(no_parent),!is.na(no_parent))
    assert_that(is.flag(no_check_certificate), !is.na(no_check_certificate))
    assert_that(is.flag(relative), !is.na(relative))
    assert_that(is.flag(remote_time), !is.na(remote_time))
    assert_that(is.numeric(wait))
    assert_that(is.flag(verbose), !is.na(verbose))
    assert_that(is.flag(debug), !is.na(debug))
    assert_that(is.flag(dry_run), !is.na(dry_run))
    assert_that(is.flag(stop_on_download_error), !is.na(stop_on_download_error))
    if (!missing(force_local_filename)) assert_that(is.string(force_local_filename))
    assert_that(is.flag(use_url_directory), !is.na(use_url_directory))
    assert_that(is.flag(no_host), !is.na(no_host))
    assert_that(cut_dirs >= 0)

    ## opts to pass to child function
    opts <- list(level = level, accept_follow = accept_follow, reject_follow = reject_follow, accept_download = accept_download, accept_download_extra = accept_download_extra, reject_download = reject_download, wait = wait, verbose = verbose, show_progress = show_progress, relative = relative, no_parent = no_parent, debug = debug) ##robots_off = robots_off,
    ## curl options
    opts$curl_config <- build_curl_config(debug = debug, show_progress = show_progress, no_check_certificate = no_check_certificate, user = user, password = password, remote_time = remote_time)
    ok <- FALSE
    msg <- "" ## error or other messages
    downloads <- tibble(url = character(), file = character(), was_downloaded = logical())
    tryCatch({
        if (missing(force_local_filename)) {
            is_ftp <- grepl("^ftp", url)
            if (is_ftp && missing(accept_follow)) {
                ## modify accept_follow, unless user has already overridden the defaults
                opts$accept_follow <- "[^\\.]" ## anything without a .
            }
            rec <- spider(to_visit = url, opts = opts, ftp = is_ftp)
            ## download each file, or not depending on clobber behaviour
            ## if doing a dry run no download, but do enumerate the list of files that would be downloaded
            downloads <- tibble(url = unique(rec$download_queue), file = NA_character_, was_downloaded = FALSE)
        } else {
            ## force_local_filename is a special case: just download this one file
            downloads <- tibble(url = url, file = force_local_filename, was_downloaded = FALSE)
        }
        if (dry_run && verbose && length(downloads$url) > 0) {
            cat(sprintf(" dry_run is TRUE, bb_rget is not downloading the following files:\n %s\n", paste(downloads$url, collapse="\n ")))
        }
        ## download each file
        ## keep track of which were actually downloaded
        for (dfi in seq_along(downloads$url)) {
            df <- downloads$url[dfi]
            mydir <- if (use_url_directory) sub("[\\/]$", "", directory_from_url(df, no_host = no_host, cut_dirs = cut_dirs)) else "" ## no trailing filesep
            if (is.na(downloads$file[downloads$url == df])) {
                fname <- if (use_url_directory) file.path(mydir, basename(df)) else basename(df)
            } else {
                ## filename has already been given, by force_local_filename
                fname <- if (use_url_directory) {
                             file.path(mydir, downloads$file[downloads$url == df]) ## prepend local path
                         } else {
                             downloads$file[downloads$url == df]
                         }
            }
            downloads$file[downloads$url == df] <- fname
            if (!dry_run) {
                if (use_url_directory && !dir.exists(mydir)) dir.create(mydir, recursive = TRUE)
                do_download <- clobber >= 1 || (!file.exists(fname))
                ## if clobber == 1, we set the if-modified-since option, so we can ask for download and it will not re-download unless needed
                if (do_download) {
                    if (verbose) cat(sprintf(" downloading file %d of %d: %s ... ", dfi, nrow(downloads), df), if (show_progress) "\n")
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
                    if (file.exists(fname)) file.copy(fname, dlf)
                    if (grepl("^ftp", df)) {
                        suppressWarnings(req <- httr::with_config(myopts, httr::GET(df, write_disk(path = dlf, overwrite = TRUE))))
                    } else {
                        req <- httr::with_config(myopts, httr::GET(df, write_disk(path = dlf, overwrite = TRUE)))
                    }
                    if (httr::http_error(req)) {
                        ## don't throw error on download
                        myfun <- if (stop_on_download_error) stop else warning
                        myfun("Error downloading ", df, ": ", httr::http_status(req)$message)
                    } else {
                        ## now if the file wasn't re-downloaded, dlf will be zero bytes
                        if (file.exists(dlf) && file.info(dlf)$size > 0) {
                            ## file was updated
                            downloads$was_downloaded[downloads$url == df] <- TRUE
                            if (file.exists(fname)) file.remove(fname)
                            file.copy(dlf, fname)
                            ## delete temp file
                            file.remove(dlf)
                            ## set local file time if appropriate
                            ## setting the filetime option in the curl request does not appear to modify the local file timestamp
                            if (remote_time) set_file_timestamp(path = fname, hdrs = httr::headers(req))
                            if (verbose) cat(if (show_progress) "\n", "done.\n")
                        } else {
                            file.remove(dlf)
                            if (verbose) cat(if (show_progress) "\n", "file unchanged on server, not downloading.\n")
                        }
                    }
                } else {
                    if (verbose) cat(sprintf(" file already exists, not downloading: %s ... ", df), if (show_progress) "\n")
                }
            }
        }
        ok <- TRUE
    }, error = function(e) {
        ## if download was aborted, use NA for ok
        ok <<- if (grepl("callback aborted", e$message, ignore.case = TRUE)) NA else FALSE
        msg <<- e$message
        if (verbose) {
            ## echo the error message but don't throw it as a full blown error
            ## what happens when download is interrupted
            ## often these are unimportant (e.g. 404 response codes during recursion) so we don't want to halt the entire process
            cat(sprintf(" bb_rget exited with an error (%s)\n", e$message))
        }
    })
    tibble(ok = ok, files = list(downloads), message = msg)
}

## ftp logical: if TRUE, use ftp
spider <- function(to_visit, visited = character(), download_queue = character(), opts, current_level = 0, ftp = FALSE) {
    ## TODO: check that opts has the names we expect
    if (ftp) opts$curl_config$options$dirlistonly <- 1L
    to_visit <- to_visit[!to_visit %in% visited]
    if (length(to_visit) < 1) return(list(visited = visited, download_queue = download_queue))
    next_level_to_visit <- character()
    first_req <- TRUE
    for (url in to_visit) {
        ## first check that this isn't a download file
        temp1 <- length(opts$accept_download) > 0
        for (rgx in opts$accept_download) temp1 <- temp1 & grepl(rgx, url)
        temp2 <- length(opts$accept_download_extra) > 0
        for (rgx in opts$accept_download_extra) temp2 <- temp2 & grepl(rgx, url)
        is_dl <- temp1 || temp2
        for (rgx in opts$reject_download) is_dl <- is_dl & !grepl(rgx, url)
        if (is_dl) {
            ## don't visit it, add to download queue
            download_queue <- c(download_queue, url)
        } else {
            if (first_req && current_level < 1) {
                first_req <- FALSE
            } else {
                if (opts$wait > 0) Sys.sleep(opts$wait)
            }
            if (opts$verbose) cat(sprintf(" visiting %s ...", url), if (opts$show_progress) "\n")
            if (ftp) {
                ## ftp is a bit funny - when recursing, we can't be sure if a link is a file or a directory
                ## we have added trailing slashes, but if it was actually a file this will throw an error
                ## also, suppress warnings else we get warnings about reading directory contents etc
                x <- tryCatch(suppressWarnings(httr::with_config(opts$curl_config, GET(url))), error = function(e) {
                    if (grepl("directory", e$message)) {
                        ## was probably a 'Server denied you to change to the given directory' message, ignore it
                        if (opts$verbose) cat(" (ignoring error: ", e$message, ") ... done.\n")
                        NULL
                    } else {
                        stop(e$message)
                    }
                })
                if (is.null(x)) next
            } else {
                x <- httr::with_config(opts$curl_config, GET(url))
            }
            ## TODO check for error?
            if (ftp) {
                ## treat as text (i.e. standard ftp directory listing)
                ## except if it comes back as text/html, try parsing it as html
                ## an FTP request through a proxy may be turned into HTML, grrr
                ## with httr 1.4, ftp headers will not be parsed
                ## TODO need to find an example of ftp:// returning http headers, and catch them
                if (is.raw(headers(x))) {
                    was_html <- FALSE
                } else {
                    was_html <- !is.null(headers(x)$`content-type`) && grepl("html", headers(x)$`content-type`)
                }
                x <- tryCatch(content(x, as = "text"), error = function (e) {
                    stop("error: ", e$message)
                })
                links_from <- "text"
                if (was_html) {
                    try({
                        x <- read_html(x)
                        links_from <- "html"
                    }, silent = TRUE)
                }
            } else {
                links_from <- "html"
                x <- tryCatch(read_html(content(x, as = "text")), error = function (e) {
                    ## not valid HTML?
                    NULL
                    #if (current_level < 1 && grepl("^ftp", url, ignore.case = TRUE)) {
                    #    ## this was the as-provided URL, and it doesn't deliver HTML
                    #    ## not supported yet
                    #    cat("recalling spider with ftp = TRUE\n")
                    #    return(spider(to_visit, visited = character(), download_queue = character(), opts = opts, current_level = 0, ftp = TRUE))
                    #    stop("rget currently only supports ftp:// urls if they deliver a valid HTML response")
                    #} else {
                    #    NULL
                    #}
                })
            }
            if (opts$verbose && opts$show_progress) cat("\n")
            if (!is.null(x)) {
                ## if this url matches download criteria, should we also be writing this file to disk?
                ## grab all link hrefs
                if (links_from == "text") {
                    all_links <- strsplit(x, "[\r\n]+")[[1]]
                    #cat("all links:")
                    #cat(all_links)
                    #cat("\n")
                } else {
                    all_links <- unique(na.omit(vapply(html_nodes(x, "a"), function(z) html_attr(z, "href"), FUN.VALUE = "", USE.NAMES = FALSE)))
                }
                ## discard non-relative links, if opts$relative
                if (opts$relative) all_links <- all_links[vapply(all_links, is_relative_url, FUN.VALUE = TRUE, USE.NAMES = FALSE)]
                ## get all links as absolute URLs, discarding anchors (fragments)
                all_links <- vapply(all_links, function(z) clean_and_filter_url(xml2::url_absolute(z, url)), FUN.VALUE = "", USE.NAMES = FALSE)
                if (opts$no_parent) all_links <- all_links[vapply(all_links, is_nonparent_url, parent = url, FUN.VALUE = TRUE, USE.NAMES = FALSE)]
                follow_idx <- rep(length(opts$accept_follow) > 0, length(all_links))
                for (rgx in opts$accept_follow) follow_idx <- follow_idx & vapply(all_links, function(z) grepl(rgx, z), FUN.VALUE = TRUE, USE.NAMES = FALSE)
                for (rgx in opts$reject_follow) follow_idx <- follow_idx & !vapply(all_links, function(z) grepl(rgx, z), FUN.VALUE = TRUE, USE.NAMES = FALSE)
                follow_links <- all_links[follow_idx]
                temp1 <- rep(length(opts$accept_download) > 0, length(all_links))
                for (rgx in opts$accept_download) temp1 <- temp1 & vapply(all_links, function(z) grepl(rgx, z), FUN.VALUE = TRUE, USE.NAMES = FALSE)
                temp2 <- rep(length(opts$accept_download_extra) > 0, length(all_links))
                for (rgx in opts$accept_download_extra) temp2 <- temp2 & vapply(all_links, function(z) grepl(rgx, z), FUN.VALUE = TRUE, USE.NAMES = FALSE)
                download_idx <- temp1 | temp2
                for (rgx in opts$reject_download) download_idx <- download_idx & !vapply(all_links, function(z) grepl(rgx, z), FUN.VALUE = TRUE, USE.NAMES = FALSE)
                download_links <- all_links[download_idx]
                download_links <- download_links[!download_links %in% download_queue]
                if (current_level < (opts$level - 1)) { ## -1 because we will download files linked from these pages, and those files will be current_level + 2
                    if (opts$verbose) cat(sprintf(" %d download links", length(download_links)))
                    follow_links <- setdiff(follow_links, download_links) ## can't be in both, treat as download?
                    if (ftp) {
                        ## links won't have trailing /, add it
                        follow_links <- sub("/*$", "/", follow_links)
                    }
                    follow_links <- follow_links[!follow_links %in% visited] ## discard any already visited
                    if (opts$verbose) cat(sprintf(", %d links to visit", length(follow_links))) ## parent links might be included here, but excluded when attempt to visit at next recursion level
                    next_level_to_visit <- c(next_level_to_visit, follow_links) ## add to list to visit at next recursion level
                }
                ##    lh <- lapply(all_links, httr::HEAD)
                if (current_level < opts$level) {
                    ## ad download_links to download_queue for later downloading, so long as they are within our recursion limit
                    download_queue <- c(download_queue, download_links)
                }
            }
            if (opts$verbose) cat(" done.\n")
        }
    }
    visited <- c(visited, to_visit)
    ## recurse to next level
    spider(next_level_to_visit, visited = visited, download_queue = download_queue, opts = opts, current_level = current_level + 1, ftp = ftp)
}

clean_and_filter_url <- function(url, accept_schemes = c("https", "http", "ftp")) {
    if (!is.string(url) || !nzchar(url) || is.na(url)) return(NA_character_)
    temp <- httr::parse_url(url)
    temp$fragment <- NULL ## discard fragment
    if (temp$scheme %in% accept_schemes) httr::build_url(temp) else NA_character_
}

is_relative_url <- function(url) is.null(httr::parse_url(url)$hostname)
is_nonparent_url <- function(url, parent) {
    ## accept url if it is a descendant of parent, or is from a different host
    ## note that both need to be absolute URLs
    if (!nzchar(parent) || !nzchar(url)) return(FALSE)
    parhost <- httr::parse_url(parent)$hostname
    urlhost <- httr::parse_url(url)$hostname
    if (!is.null(urlhost) && !is.null(parhost) && urlhost != parhost) return(TRUE)
    ## url is a descendant of parent if its starting fragment is the parent url
    ## note that this only works if e.g. "../"'s have been removed from url and parent
    if (grepl("../", url, fixed = TRUE) || grepl("../", parent, fixed = TRUE)) {
        warning("url or parent contain '../', no_parent may not work correctly")
        FALSE
    } else {
        if (!grepl("/$", parent)) parent <- dirname(parent) ## strip trailing filename
        tolower(substr(url, 1, nchar(parent))) == tolower(parent)
    }
}

set_file_timestamp <- function(path, hdrs) {
    ok <- tryCatch({
        ftime <- hdrs[grepl("last-modified", names(hdrs), ignore.case = TRUE)]
        ## e.g. "Tue, 21 Aug 2018 16:00:00 GMT"
        if (length(ftime) == 1) {
            suppressWarnings(ftime <- lubridate::dmy_hms(ftime))
            if (!is.na(ftime)) {
                Sys.setFileTime(path = path, time = ftime)
                TRUE
            } else {
                FALSE
            }
        } else {
            FALSE
        }
    }, error = function(e) FALSE)
    invisible(ok)
}

#' @rdname bb_rget
#' @export
bb_rget_default_downloads <- function() "README|\\.(asc|csv|hdf|nc|bin|txt|gz|bz|bz2|Z|zip|kmz|kml|tar|tgz|tif|tiff)$"

build_curl_config <- function(debug = FALSE, show_progress = FALSE, no_check_certificate = FALSE, user, password, enforce_basic_auth = FALSE, remote_time = NA) {
    out <- if (!is.null(debug) && debug) httr::verbose() else httr::config() ## curl's verbose output is intense, save it for debug = TRUE
    if (!is.null(show_progress) && show_progress) out$options <- c(out$options, httr::progress()$options)
    if (!is.null(no_check_certificate) && no_check_certificate) {
            out$options$ssl_verifypeer = 0L
            ##out$options$ssl_verifyhost = 0L ## does not seem to work
    }
    if (!missing(user) && !is.null(user) && !is.na(user)) {
        out$options$username <- user
    }
    if (!missing(password) && !is.null(password) && !is.na(password)) {
        out$options$password <- password
    }
    if (enforce_basic_auth) out$options$httpauth <- 1L # force basic authentication
    if (!is.na(remote_time)) out$options$filetime <- if (remote_time) 1L else 0L
    out
}
