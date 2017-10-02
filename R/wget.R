#' Mirror an external data source using the wget utility
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
#' @param verbose logical: if TRUE, provide additional progress output
#' @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
#'
#' @return the directory if local_dir_only is TRUE, otherwise TRUE on success
#'
#' @seealso \code{\link{bb_wget}}
#'
#' @export
bb_handler_wget <- function(config,verbose=FALSE,local_dir_only=FALSE) {
    assert_that(is(config,"bb_config"))
    assert_that(nrow(config$data_sources)==1)
    assert_that(is.flag(verbose))
    assert_that(is.flag(local_dir_only))

    if (local_dir_only)
        return(file.path(bb_settings(config)$local_file_root,directory_from_url(config$data_sources$source_url)))

    cfrow <- bb_settings_to_cols(config)
    this_flags <- flags_to_charvec(cfrow$method_flags)
    ## add wget_global_flags
    gflags <- flags_to_charvec(cfrow$wget_global_flags)
    if (length(gflags)>0)
        this_flags <- c(this_flags,gflags)

    ## proxy-user and proxy-password flags
    ## this needs to decide whether it should use http_proxy_user or ftp_proxy_user info - exclude for now
    #if (!grepl("proxy-user=",cfrow$wget_flags)) {
    #    cfrow$wget_flags=paste(cfrow$wget_flags,paste0("--proxy-user=",cfrow$http_proxy_user),sep=" ")
    #}
    #if (!grepl("proxy-password=",cfrow$wget_flags)) {
    #    cfrow$wget_flags=paste(cfrow$wget_flags,paste0("--proxy-password=",cfrow$http_proxy_password),sep=" ")
                                        #}
    ## add flags for clobber behaviour
    if (!is.null(cfrow$clobber) && !is.na(cfrow$clobber) && cfrow$clobber %in% c(0,1)) {
        if (cfrow$clobber==0) {
            this_flags <- resolve_wget_clobber_flags(this_flags,"--no-clobber")
        } else {
            this_flags <- resolve_wget_clobber_flags(this_flags,"--timestamping")
        }
    }
    ## add user, password flags
    if (!is.na(cfrow$user) && nchar(cfrow$user)>0) this_flags <- c(this_flags,"--user",cfrow$user)
    if (!is.na(cfrow$password) && nchar(cfrow$password)>0) this_flags <- c(this_flags,"--password",cfrow$password)
    ##if (cfrow$wait>0) this_flags <- c(this_flags,"--wait=",cfrow$wait)

    if (!verbose) {
        ## suppress wget's own output
        if (!any(grepl("quiet",tolower(this_flags)))) this_flags <- c(this_flags,"--quiet")
    }

    if (!is.null(cfrow$skip_downloads) && cfrow$skip_downloads) {
        if (verbose) cat(sprintf(" skip_downloads is TRUE, not executing: wget %s %s\n",paste(this_flags,collapse=" "),cfrow$source_url))
        ok <- TRUE
    } else {
        if (sink.number()>0) {
            ## we have a sink() redirection in place
            ## sink() won't catch the output of system commands, which means we miss stuff in our log
            ## workaround: send output to temporary file so that we can capture it
            output_file <- gsub("\\\\","\\\\\\\\",tempfile()) ## escape backslashes
            this_flags <- c("-o",output_file,this_flags)
            syscall_obj <- bb_wget(cfrow$source_url,this_flags,verbose=verbose)
            ## now echo the contents of output_file to console, so that sink() captures it
            if (verbose) cat(readLines(output_file),sep="\n")
        } else {
            syscall_obj <- bb_wget(cfrow$source_url,this_flags,verbose=verbose)
        }
        ## now return an appropriate indicator of success
        if (is.null(syscall_obj)) {
            ## no object returned - this happens if process is interrupted by the user on unix
            ok <- as.logical(NA)
        } else {
            ok <- syscall_obj$status==0
        }
    }
    ok
}


#' Make a wget call
#'
#' The wget system call is made using the \code{exec_wait} function from the sys package. Call \code{bb_wget("--help")} to get a message giving information about wget's command line parameters
#'
#' @param url string: the URL to retrieve
#' @param flags character: character vector of command-line flags to pass to wget
#' @param verbose logical: print trace output?
#' @param capture_stdout logical: if TRUE, return 'stdout' and 'stderr' output in the returned object (see exec_internal from the sys package). Otherwise send these outputs to the console
#'
#' @return the result of the system call (or if bb_wget("--help") was called, a message will be issued). The returned object will have components 'status' and (if capture_stdout was TRUE) 'stdout' and 'stderr'
#'
#' @seealso \code{\link{bb_install_wget}} \code{\link{bb_find_wget}}
#' @examples
#' \dontrun{
#'   ## get help about wget command line parameters
#'   bb_wget("--help")
#' }
#'
#' @export
bb_wget <- function(url,flags=character(),verbose=FALSE,capture_stdout=FALSE) {
    assert_that(is.string(url))
    assert_that(is.character(flags))
    assert_that(is.flag(verbose))
    assert_that(is.flag(capture_stdout))
    if (tolower(url) %in% c("-h","--help") || identical(tolower(flags),"-h") || identical(tolower(flags),"--help")) {
        out <- sys::exec_internal(bb_find_wget(),args="--help",error=TRUE)
        message(rawToChar(out$stdout))
    } else {
        ## sys expects flags as a char vector, not a string
        flags <- flags_to_charvec(flags) ## will split string, or replace NA/"" with empty character vector
        if (verbose) cat(sprintf(" executing wget %s %s\n",paste(flags,collapse=" "),url))
        if (capture_stdout) {
            sys::exec_internal(bb_find_wget(),args=c(flags,url),error=FALSE)
        } else {
            status <- sys::exec_wait(bb_find_wget(),args=c(flags,url),std_out=TRUE,std_err=TRUE)
            list(status=status)
        }
        ##system2(wget_exe(),args=paste(flags,url,sep=" "),...)
    }
}

#' Helper function to install wget
#'
#' Currently only works on Windows platforms. The wget.exe executable will be downloaded from https://eternallybored.org/misc/wget/current/wget.exe and installed into your appdata directory (typically something like C:/Users/username/AppData/Roaming/)
#'
#' @references https://eternallybored.org/misc/wget/current/wget.exe
#'
#' @return the path to the installed executable
#'
#' @examples
#' \dontrun{
#'   bb_install_wget()
#' }
#'
#' @seealso \code{\link{bb_find_wget}}
#'
#' @export
bb_install_wget <- function() {
    if (tolower(.Platform$OS.type)!="windows")
        stop("bb_install_wget only supports windows platforms")
    ## NOTE, could also use e.g. https://github.com/r-lib/rappdirs to find this directory
    path <- Sys.getenv("APPDATA")
    if (dir_exists(path)) {
        path <- file.path(path,"bowerbird")
        if (!dir_exists(path)) dir.create(path)
        if (!dir_exists(path)) stop("could not create directory ",path," to store the wget executable")
        ok <- download.file("https://eternallybored.org/misc/wget/current/wget.exe",
                      destfile=file.path(path,"wget.exe"),
                      mode="wb")
        if (ok==0) {
            file.path(path,"wget.exe")
        } else {
            stop("Sorry, could not install wget")
        }
    } else {
        stop("Sorry, could not find the user APPDATA directory to install wget into")
    }
}

#' Find the wget executable, and optionally install it if it is not found
#'
#' Installation (if required) currently only works on Windows platforms. The wget.exe executable will be downloaded from https://eternallybored.org/misc/wget/current/wget.exe and installed into your appdata directory (typically something like C:/Users/username/AppData/Roaming/)
#'
#' @references https://eternallybored.org/misc/wget/current/wget.exe
#'
#' @param install logical: install the executable if it is not found? (Windows only)
#'
#' @return the path to the wget executable, or NULL if it was not found
#'
#' @examples
#' \dontrun{
#'   wget_path <- bb_find_wget()
#'   wget_path <- bb_find_wget(install=TRUE) ## install (on windows) if needed
#' }
#'
#' @seealso \code{\link{bb_install_wget}}
#'
#' @export
bb_find_wget <- function(install=FALSE) {
    assert_that(is.flag(install))
    bb_opts <- getOption("bowerbird")
    if (!is.null(bb_opts)) {
        if (!is.null(bb_opts$wget_exe)) {
            ## already set, just return this
            return(bb_opts$wget_exe)
        }
    } else {
        bb_opts <- list()
    }
    if (wget_test("wget")) {
        myexe <- unname(Sys.which("wget"))
    } else {
        if (.Platform$OS.type=="windows") {
            myexe <- file.path(Sys.getenv("APPDATA"),"bowerbird","wget.exe")
            if (!wget_test(myexe)) {
                if (!install) {
                    return(NULL)
                } else {
                    ## install wget
                    bb_install_wget()
                    ## and try again
                    return(bb_find_wget(install=FALSE))
                }
            }
        } else {
            return(NULL)
        }
    }
    bb_opts$wget_exe <- myexe
    options(bowerbird=bb_opts)
    myexe
}

## internal function to return the wget executable name (possibly with path)
## if successfully identified, set the bowerbird$wget_exe option (and use this on subsequent calls)
##wget_exe <- function() {
##    bb_opts <- getOption("bowerbird")
##    if (!is.null(bb_opts)) {
##        if (!is.null(bb_opts$wget_exe)) return(bb_opts$wget_exe)
##    } else {
##        bb_opts <- list()
##    }
##    if (wget_test("wget")) {
##        myexe <- "wget"
##    } else {
##        if (.Platform$OS.type=="windows") {
##            myexe <- file.path(Sys.getenv("APPDATA"),"bowerbird","wget.exe")
##            if (!wget_test(myexe)) {
##                stop("could not find the wget executable. Try the bb_install_wget() function, or install it yourself and ensure that it is on the path")
##            }
##        } else {
##            stop("could not find the wget executable")
##        }
##    }
##    bb_opts$wget_exe <- myexe
##    options(bowerbird=bb_opts)
##    myexe
##}

## internal: test a potential wget executable path
wget_test <- function(wget_path) {
    try({system(paste0(wget_path," --help"),intern=TRUE);return(TRUE)},silent=TRUE)
    FALSE
}

## internal: merge two sets of wget flags
## take flags as either character vector, single-element list of character vector, space-separated string (as per flags_to_charvec)
resolve_wget_clobber_flags <- function(primary_flags,secondary_flags) {
    wgf <- flags_to_charvec(primary_flags)
    secondary_flags <- flags_to_charvec(secondary_flags)
    for (thisflag in setdiff(secondary_flags,wgf)) {
        switch(thisflag,
               "-N"=,
               "--timestamping"={ if (! any(c("--no-clobber","-nc") %in% wgf)) wgf <- c(wgf,thisflag) },
               "-nc"=,
               "--no-clobber"={ if (! any(c("--timestamping","-N") %in% wgf)) wgf <- c(wgf,thisflag) },
               wgf <- c(wgf,thisflag))
    }
    wgf
}


## internal: take flags and convert to character vector if needed
## flags can either be a string (will be split on white spaces)
## or list (expect single element, which is a character vector) - as would get from tibble list column
## or character vector (convert to empty character vector if null, NA, or empty string, otherwise return as is)
flags_to_charvec <- function(fl) {
    if (is.list(fl)) {
        fl[[1]]
    } else if (is.null(fl) || length(fl)<1) {
        character()
    } else if (is.character(fl)) {
        if (is.string(fl)) {
            if (is.null(fl) || is.na(fl) || !nzchar(fl)) {
                character()
            } else {
                ## split on whitespaces
                strsplit(fl,"[[:space:]]+")[[1]]
            }
        } else {
            fl
        }
    } else {
        stop("expecting flags as character vector or list")
    }
}



##dir_exists <- function(z) utils::file_test("-d",z)
##file_exists <- function(x) utils::file_test('-f', x)
