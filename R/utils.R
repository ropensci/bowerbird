##-------------------------------------------
## various helper functions
## not exported for user

## file checksums
file_hash <- function(filename,hash="sha1") {
    assert_that(is.string(hash))
    hash <- match.arg(tolower(hash),c("md5","sha1"))
    switch(hash,
           md5=as.character(openssl::md5(file(filename))),
           sha1=as.character(openssl::sha1(file(filename))),
           stop("unexpected hash type",hash)
           )
}

## NA or empty string
na_or_empty <- function(z) is.na(z) | !nzchar(z)

is_nonempty_string <- function(z) is.string(z) && nzchar(z)

## check method (which may be function, call, or symbol) matches expected function
check_method_is <- function(method,expected) {
    assert_that(is.function(expected))
    identical(match.fun(method),expected)
}

## internal: does something resolve to a function via match.fun?
is_a_fun <- function(z) {
    out <- FALSE
    try({match.fun(z); out <- TRUE},silent=TRUE)
    out
}

save_current_settings <- function() {
    return(list(working_dir=getwd(), ## current working directory
                env_http_proxy=Sys.getenv("http_proxy"), ## proxy env vars
                env_https_proxy=Sys.getenv("https_proxy"),
                env_ftp_proxy=Sys.getenv("ftp_proxy")
                ))
}

restore_settings <- function(settings) {
    setwd(settings$working_dir)
    Sys.setenv(http_proxy=settings$env_http_proxy)
    Sys.setenv(https_proxy=settings$env_https_proxy)
    Sys.setenv(ftp_proxy=settings$env_ftp_proxy)
    invisible(NULL)
}

dir_exists <- function(z) file.exists(dirname(z)) && !(!file.info(z)$isdir || is.na(file.info(z)$isdir))


directory_from_url <- function(this_url, no_host = FALSE, cut_dirs = 0L) {
    if (is.list(this_url)) this_url <- unlist(this_url)
    vapply(this_url, single_directory_from_url, no_host = no_host, cut_dirs = cut_dirs, FUN.VALUE = "", USE.NAMES = FALSE)
}

single_directory_from_url <- function(this_url, no_host = FALSE, cut_dirs = 0L) {
    ## for backwards compat
    if (is.na(this_url)) return(NA_character_)
    ## operate on one url string at a time
    temp <- httr::parse_url(this_url)
    if (cut_dirs > 0) {
        ## remove this many directory levels from the path
        ## note that we need to preserve the trailing slash (or not) in the path
        has_tr_s <- grepl("[/\\]$", temp$path)
        pths <- strsplit(temp$path, "[/\\]+")[[1]]
        temp$path <- if (length(pths) > cut_dirs) pths[seq(from = cut_dirs+1, to = length(pths), by = 1)] else ""
        if (has_tr_s) temp$path <- paste0(temp$path, "/")
    }
    temp$path <- paste(temp$path, collapse = "/")
    this_url <- ""
    if (!no_host) {
        this_url <- temp$host
        if (!is.null(temp$port) && nzchar(temp$port))
            this_url <- paste0(this_url, "+", temp$port)
    }
    this_url <- paste(this_url, temp$path, sep = "/")
    ## discard anything at all after the last trailing slash
    this_url <- sub("/[^/]*$","/",this_url) ## could use dirname here?
    this_url[grepl("[^/\\]$",this_url)] <- paste0(this_url[grepl("[^/\\]$",this_url)],"/") ## enforce trailing slash
    this_url <- sub("[/\\]+$", "/", this_url) ## single trailing slash only
    this_url <- sub("^[/\\]+", "", this_url) ## and finally make sure we don't have any leading slashes
    this_url ## returns char vector
}

## adapted from http://conjugateprior.org/2015/06/identifying-the-os-from-r/
get_os <- function() {
    if (.Platform$OS.type=="windows") return("windows")
    sysinf <- Sys.info()
    if (!is.null(sysinf)){
        os <- sysinf["sysname"]
        if (tolower(os)=="darwin")
            os <- "osx"
    } else {
        os <- .Platform$OS.type
        if (grepl("^darwin", R.version$os,ignore.case=TRUE))
            os <- "osx"
        if (grepl("linux-gnu", R.version$os,ignore.case=TRUE))
            os <- "linux"
    }
    os <- tolower(os)
    if (!os %in% c("windows","linux","unix","osx"))
        stop("unknown operating system: ",os)
    os
}

## standardize file paths, so that they can be more reliably compared
## if case is TRUE and we are on windows, also convert to lower case
std_path <- function(z, case = FALSE) {
    z <- normalizePath(z, mustWork = FALSE)
    ## remove trailing file separators
    z <- gsub("[/\\]+$", "", z)
    if (case) {
        iswin <- tryCatch(get_os() == "windows", error = function(e) FALSE)
        if (iswin) z <- tolower(z)
    }
    z
}

## wrapper around fs::dir_ls that acts mostly like list.files, but faster
list_files <- function(path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE) {
    types <- if (include.dirs || !recursive) "any" else c("file", "symlink", "FIFO", "socket", "character_device", "block_device")
    ## note that pattern in list.files acts on file names, but regexp in dir_ls acts on paths
    grepargs <- list()
    if (ignore.case) grepargs$ignore.case = TRUE
    out <- do.call(fs::dir_ls, c(list(path = path, regexp = pattern, type = types, recurse = recursive, all = all.files), grepargs))
    if (!full.names) {
        n_to_strip <- nchar(fs::as_fs_path(path)) + 1L ## +1 for trailing filesep
        out <- substr(out, n_to_strip+1L, 1000000L)
    }
    as.character(out)
}
