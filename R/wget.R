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
    assert_that(nrow(bb_data_sources(config))==1)
    assert_that(is.flag(verbose),!is.na(verbose))
    assert_that(is.flag(local_dir_only),!is.na(local_dir_only))

    if (local_dir_only)
        return(file.path(bb_settings(config)$local_file_root,directory_from_url(bb_data_sources(config)$source_url)))

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
    assert_that(is.flag(verbose),!is.na(verbose))
    assert_that(is.flag(capture_stdout),!is.na(capture_stdout))
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


#' Make a wget call
#'
#' The wget system call is made using the \code{exec_wait} function from the sys package. Call \code{bb_wget2("help")} to get a message giving information about wget's command line parameters
#'
#' @param url string: the URL to retrieve
#' @param recursive logical: if true, turn on recursive retrieving
#' @param level integer >=0: recursively download to this maximum depth level. Only applicable if \code{recursive} is TRUE. Specify 0 for infinite recursion. See \url{https://www.gnu.org/software/wget/manual/wget.html#Recursive-Download} for more information about wget's recursive downloading
#' @param wait numeric >=0: wait this number of seconds between successive retrievals. This option may help with servers that block multiple successive requests, by introducing a delay between requests
#' @param accept character: character vector with one or more entries. Each entry specifies a comma-separated list of filename suffixes or patterns to accept. Note that if any of the wildcard characters '*', '?', '[', or ']' appear in an element of accept, it will be treated as a filename pattern, rather than a filename suffix. In this case, you have to enclose the pattern in quotes, for example \code{accept="\"*.csv\""}
#' @param accept_regex character: character vector with one or more entries. Each entry provides a regular expression that is applied to the complete URL. Matching URLs will be accepted for download
#' @param reject character: as for \code{accept}, but specifying filename suffixes or patterns to reject
#' @param reject_regex character: as for \code{accept_regex}, but specifying regular expressions to reject
#' @param exclude_directories character: character vector with one or more entries. Each entry specifies a comma-separated list of directories you wish to exclude from download. Elements may contain wildcards
#' @param execute character: a character vector with one or more entries. Each entry is an command to be executed. A common use for this is \code{execute=c("robots=off")}, in order to avoid wget's default behaviour of identifying as a robot. Some servers will exclude robots from certain parts of their sites. See \url(https://www.gnu.org/software/wget/manual/wget.html#Robot-Exclusion} for more information about robot exclusion, and \url{https://www.gnu.org/software/wget/manual/wget.html#Wgetrc-Commands} for a full list of commands that can be specified here
#' @param restrict_file_names character: vector of one of more strings from the set "unix", "windows", "nocontrol", "ascii", "lowercase", and "uppercase". \code{restrict_file_names="windows"} is useful if you are downloading files to be used on both Windows and Unix file systems. See \url{https://www.gnu.org/software/wget/manual/wget.html#index-Windows-file-names} for more information on this parameter
#' @param progress string: the type of progress indicator you wish to use. Legal indicators are "dot" and "bar". "dot" prints progress with dots, with each dot representing a fixed amount of downloaded data. The style can be adjusted: "dot:mega" will show 64K per dot and 3M per line; "dot:giga" shows 1M per dot and 32M per line. See \url{https://www.gnu.org/software/wget/manual/wget.html#index-dot-style} for more information
#' @param no_parent logical: if TRUE, do not ever ascend to the parent directory when retrieving recursively. This is TRUE by default, bacause it guarantees that only the files below a certain hierarchy will be downloaded
#' @param no_if_modified_since logical: applies when retrieving recursively with timestamping (i.e. only downloading files that have changed since last download, which is achieved using \code{bb_config(...,clobber=1)}). The default method for timestamping is to issue an "If-Modified-Since" header on the request, which instructs the remote server not to return the file if it has not changed since the specified date. Some servers do not support this header. In these cases, trying using \code{no_if_modified_since=TRUE}, which will instead send a preliminary HEAD request to ascertain the date of the remote file
#' @param no_check_certificate logical: if TRUE, don't check the server certificate against the available certificate authorities. Also don't require the URL host name to match the common name presented by the certificate. This option might be useful if trying to download files from a server with an expired certificate, but it is clearly a security risk and so should be used with caution
#' @param relative logical: if TRUE, only follow relative links. This can sometimes be useful for restricting what is downloaded in recursive mode
#' @param adjust_extension logical: if a file of type 'application/xhtml+xml' or 'text/html' is downloaded and the URL does not end with .htm or .html, this option will cause the suffix '.html' to be appended to the local filename. This can be useful when mirroring a remote site that has page URLs that conflict with directories (e.g. http://somewhere.org/this/page which has further content below it, say at http://somewhere.org/this/page/more. If "somewhere.org/this/page" is saved as a page, that name can't also be used as a local directory in which to store the lower-level content. Setting \code{adjust_extension=TRUE} will cause the page to be saved as "somewhere.org/this/page.html", thus resolving the conflict
#' @param extra_flags character: character vector of additional command-line flags to pass to wget
#' @param verbose logical: print trace output?
#' @param capture_stdout logical: if TRUE, return 'stdout' and 'stderr' output in the returned object (see exec_internal from the sys package). Otherwise send these outputs to the console
#'
#' @return the result of the system call (or if bb_wget("--help") was called, a message will be issued). The returned object will have components 'status' and (if capture_stdout was TRUE) 'stdout' and 'stderr'
#'
#' @seealso \code{\link{bb_install_wget}} \code{\link{bb_find_wget}}
#' @examples
#' \dontrun{
#'   ## get help about wget command line parameters
#'   bb_wget2("help")
#' }
#'
# @export
## like bb_wget, but with some wget flags promoted to explicit function parms
bb_wget2 <- function(url,recursive=TRUE,level=1,wait=0,accept,reject,accept_regex,reject_regex,exclude_directories,execute,restrict_file_names,progress,no_parent=TRUE,no_if_modified_since=FALSE,no_check_certificate=FALSE,relative=FALSE,adjust_extension=FALSE,extra_flags=character(),verbose=FALSE,capture_stdout=FALSE) {
    assert_that(is.string(url))
    assert_that(is.flag(recursive),!is.na(recursive))
    if (recursive) assert_that(is.numeric(level),level>=0)
    if (missing(accept)) accept <- character()
    assert_that(is.character(accept))
    if (missing(accept_regex)) accept_regex <- character()
    assert_that(is.character(accept_regex))
    if (missing(reject)) reject <- character()
    assert_that(is.character(reject))
    if (missing(reject_regex)) reject_regex <- character()
    assert_that(is.character(reject_regex))
    if (missing(exclude_directories)) exclude_directories <- character()
    assert_that(is.character(exclude_directories))
    if (missing(restrict_file_names)) restrict_file_names <- character()
    assert_that(is.character(restrict_file_names))
    if (missing(progress)) progress <- ""
    assert_that(is.string(progress))
    assert_that(is.flag(no_parent),!is.na(no_parent))
    assert_that(is.flag(no_if_modified_since),!is.na(no_if_modified_since))
    assert_that(is.flag(no_check_certificate),!is.na(no_check_certificate))
    assert_that(is.flag(relative),!is.na(relative))
    assert_that(is.flag(adjust_extension),!is.na(adjust_extension))
    assert_that(is.numeric(wait))
    if (missing(execute)) execute <- character()
    assert_that(is.character(execute))
    assert_that(is.character(extra_flags))
    assert_that(is.flag(verbose),!is.na(verbose))
    assert_that(is.flag(capture_stdout),!is.na(capture_stdout))
    if (tolower(sub("^\\-+","",url)) %in% c("h","help")) {
        out <- sys::exec_internal(bb_find_wget(),args="--help",error=TRUE)
        message(rawToChar(out$stdout))
    } else {
        ## build wget flags
        flags <- character()
        if (recursive) flags <- c(flags,"--recursive",paste0("--level=",level))
        flags <- c(flags,vapply(accept,function(z)paste0("--accept=",z),FUN.VALUE="",USE.NAMES=FALSE))
        flags <- c(flags,vapply(accept_regex,function(z)paste0("--accept-regex=",z),FUN.VALUE="",USE.NAMES=FALSE))
        flags <- c(flags,vapply(reject,function(z)paste0("--reject=",z),FUN.VALUE="",USE.NAMES=FALSE))
        flags <- c(flags,vapply(reject_regex,function(z)paste0("--reject-regex=",z),FUN.VALUE="",USE.NAMES=FALSE))
        flags <- c(flags,vapply(exclude_directories,function(z)paste0("--exclude_directories=",z),FUN.VALUE="",USE.NAMES=FALSE))
        if (length(restrict_file_names)>0) {
            restrict_file_names <- paste(restrict_file_names,sep=",",collapse=",")
            flags <- c(flags,paste0("--restrict_file_names=",restrict_file_names))
        }
        if (length(progress)>0 && nzchar(progress)) flags <- c(flags,paste0("--progress=",progress))
        if (recursive && no_parent) flags <- c(flags,"--no-parent")
        if (no_if_modified_since) flags <- c(flags,"--no-if-modified-since")
        if (no_check_certificate) flags <- c(flags,"--no-check-certificate")
        if (relative) flags <- c(flags,"--relative")
        if (adjust_extension) flags <- c(flags,"--adjust-extension")
        if (wait>0) flags <- c(flags,paste0("--wait=",wait))
        if (length(execute)>0)
            flags <- c(flags,paste("-e",execute,sep=" ")) ## these need to be of the form '-e first_command' '-e second_command'
        ## add any extra_flags
        ## sys expects flags as a char vector, not a string
        extra_flags <- flags_to_charvec(extra_flags) ## will split string, or replace NA/"" with empty character vector
        flags <- c(flags,extra_flags)
        if (verbose) cat(sprintf(" executing wget %s %s\n",paste(flags,collapse=" "),url))
        #if (capture_stdout) {
        #    sys::exec_internal(bb_find_wget(),args=c(flags,url),error=FALSE)
        #} else {
        #    status <- sys::exec_wait(bb_find_wget(),args=c(flags,url),std_out=TRUE,std_err=TRUE)
        #    list(status=status)
        #}
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
    assert_that(is.flag(install),!is.na(install))
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
