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

directory_from_url <- function(this_url) {
    ## this_url can be character or list of char
    this_url <- sub("^(http|https|ftp)://","",unlist(this_url))
    this_url <- sub(":","+",this_url) ## port
    ## discard anything at all after the last trailing slash
    this_url <- sub("/[^/]*$","/",this_url)
    this_url[grepl("[^/\\]$",this_url)] <- paste0(this_url[grepl("[^/\\]$",this_url)],"/") ## enforce trailing slash (?)
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
