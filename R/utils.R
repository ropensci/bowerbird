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
    ##identical(get_function_from_method(method),expected)
    identical(match.fun(method),expected)
}

## internal: does something resolve to a function via match.fun?
is_a_fun <- function(z) {
    out <- FALSE
    try({match.fun(z); out <- TRUE},silent=TRUE)
    out
}

## get actual function from method (which may be function, call, or symbol)
##get_function_from_method <- function(method) {
##    assert_that(is.function(method) || is.call(method) || is.symbol(method) || is.string(method))
##    if (is.function(method)) {
##        return(method)
##    } else if (is.call(method)) {
##        if (all.names(method)[1]=="quote") {
##            ## call was constructed as e.g. enquote(whatever)
##            return(eval(method))
##        } else {
##            ## call was constructed as e.g. quote(whatever())
##            return(get_function_from_method(all.names(method)[1])) ## check using name of called function
##        }
##    } else if (is.string(method)) {
##        ## passed as function name
##        if (exists(method,mode="function")) return(get(method))
##    } else {
##        ## symbol/name, by e.g. quote(whatever)
##        if (exists(deparse(method),mode="function")) return(eval(method))
##    }
##    stop("could not extract the underlying method function")
##}

## isn't there a better way to do this?
## qfun is a quoted function with arguments already provided, e.g. quote(fun(var=arg))
## we want to add some extra args (xargs, named list)
## not used any more
##inject_args <- function(qfun,xargs,extras_first=TRUE) {
##    assert_that(is.flag(extras_first),!is.na(extras_first))
##    ## xargs is the named list of extra arguments to add
##    if (extras_first) {
##        arglist <- xargs
##        if (length(qfun)>1) for (k in seq_len(length(qfun))[-1]) arglist <- c(arglist,qfun[[k]])
##    } else {
##        arglist <- list()
##        if (length(qfun)>1) for (k in seq_len(length(qfun))[-1]) arglist <- c(arglist,qfun[[k]])
##        arglist <- c(arglist,xargs)
##    }
##    arglist ## call this as e.g. do.call(all.names(qfun)[1],arglist)
##}

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
    ## discard anything after the last trailing slash if it includes asterisks (is a file mask)
    ##sub("/[^/]*\\*[^/]*$","/",this_url)
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
