#' Mirror an external data source using the wget utility
#'
#' This is a general handler function that is suitable for a range of data sets. This function is not intended to be called directly, but rather is specified as a \code{postprocess} option in \code{\link{bb_source}}.
#'
#' This handler function makes calls to the \code{wget} utility via the \code{\link{bb_wget}} function. Arguments provided to \code{bb_handler_wget} are passed through to \code{\link{bb_wget}}.
#'
#' @param ... : parameters passed to \code{\link{bb_wget}}
#'
#' @return TRUE on success
#'
#' @seealso \code{\link{bb_wget}}, \code{\link{bb_source}}
#' @examples
#'
#' my_source <- bb_source(
#'    id="gshhg_coastline",
#'    name="GSHHG coastline data",
#'    description="A Global Self-consistent, Hierarchical, High-resolution Geography Database",
#'    doc_url= "http://www.soest.hawaii.edu/pwessel/gshhg",
#'    citation="Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
#'      High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
#'    source_url="ftp://ftp.soest.hawaii.edu/gshhg/*",
#'    license="LGPL",
#'    method=list("bb_handler_wget",recursive=TRUE,level=1,accept="*bin*.zip,README.TXT"),
#'    postprocess=list("bb_unzip"),
#'    collection_size=0.6)
#'
#' @export
bb_handler_wget <- function(...) {
    do.call(bb_handler_wget_inner,list(...))
}

# @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
# @param verbose logical: if TRUE, provide additional progress output
# @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_wget_inner <- function(config,verbose=FALSE,local_dir_only=FALSE,...) {
    assert_that(is(config,"bb_config"))
    assert_that(nrow(bb_data_sources(config))==1)
    assert_that(is.flag(verbose),!is.na(verbose))
    assert_that(is.flag(local_dir_only),!is.na(local_dir_only))

    if (local_dir_only)
        return(file.path(bb_settings(config)$local_file_root,directory_from_url(bb_data_sources(config)$source_url)))

    cfrow <- bb_settings_to_cols(config)
    this_flags <- list(...)
    ## add wget_global_flags
    gflags <- cfrow$wget_global_flags[[1]]
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
        if (cfrow$clobber<1) {
            this_flags <- merge_wget_flags(this_flags,list(no_clobber=TRUE))
        } else {
            this_flags <- merge_wget_flags(this_flags,list(timestamping=TRUE))
        }
    }
    ## add user, password flags
    if (!is.na(cfrow$user) && nchar(cfrow$user)>0) this_flags <- c(this_flags,list(user=cfrow$user))
    if (!is.na(cfrow$password) && nchar(cfrow$password)>0) this_flags <- c(this_flags,list(password=cfrow$password))
    ##if (cfrow$wait>0) this_flags <- c(this_flags,"--wait=",cfrow$wait)

    if (!verbose) {
        ## suppress wget's own output
        if (!"quiet" %in% names(this_flags)) this_flags <- c(this_flags,list(quiet=TRUE))
    }

    if (!is.null(cfrow$dry_run) && cfrow$dry_run) {
        if (verbose) {
            temp <- vapply(seq_len(length(this_flags)),function(z)paste0(names(this_flags)[z],"=",this_flags[z]),FUN.VALUE="",USE.NAMES=FALSE)
            cat(sprintf(" dry_run is TRUE, not executing: wget %s %s\n",paste(temp,collapse=" "),cfrow$source_url))
        }
        ok <- TRUE
    } else {
        this_flags <- c(list(url=cfrow$source_url),this_flags,list(verbose=verbose))
        if (sink.number()>0) {
            ## we have a sink() redirection in place
            ## sink() won't catch the output of system commands, which means we miss stuff in our log
            ## workaround: send output to temporary file so that we can capture it
            op_file <- gsub("\\\\","\\\\\\\\",tempfile()) ## escape backslashes
            this_flags <- c(list(output_file=op_file),this_flags)
            syscall_obj <- do.call(bb_wget,this_flags)
            ## now echo the contents of op_file to console, so that sink() captures it
            if (verbose) cat(readLines(op_file),sep="\n")
        } else {
            syscall_obj <- do.call(bb_wget,this_flags)
        }
        ## now return an appropriate indicator of success
        if (is.null(syscall_obj)) {
            ## no object returned - this happens if process is interrupted by the user on unix
            ok <- as.logical(NA)
        } else {
            ok <- !syscall_obj$status ## status code of 0 means OK
        }
    }
    ok
}



#' Make a wget call
#'
#' This function is an R wrapper to the command-line \code{wget} utility, which is called using either the \code{exec_wait} or the \code{exec_internal} function from the sys package. Almost all of the parameters to \code{bb_wget} are translated into command-line flags to \code{wget}. Call \code{bb_wget("help")} to get more information about wget's command line flags. If required, command-line flags without equivalent \code{bb_wget} function parameters can be passed via the \code{extra_flags} parameter.
#'
#' @param url string: the URL to retrieve
#' @param recursive logical: if true, turn on recursive retrieving
#' @param level integer >=0: recursively download to this maximum depth level. Only applicable if \code{recursive=TRUE}. Specify 0 for infinite recursion. See \url{https://www.gnu.org/software/wget/manual/wget.html#Recursive-Download} for more information about wget's recursive downloading
#' @param wait numeric >=0: wait this number of seconds between successive retrievals. This option may help with servers that block multiple successive requests, by introducing a delay between requests
#' @param accept character: character vector with one or more entries. Each entry specifies a comma-separated list of filename suffixes or patterns to accept. Note that if any of the wildcard characters '*', '?', '[', or ']' appear in an element of accept, it will be treated as a filename pattern, rather than a filename suffix. In this case, you have to enclose the pattern in quotes, for example \code{accept="\"*.csv\""}
#' @param accept_regex character: character vector with one or more entries. Each entry provides a regular expression that is applied to the complete URL. Matching URLs will be accepted for download
#' @param reject character: as for \code{accept}, but specifying filename suffixes or patterns to reject
#' @param reject_regex character: as for \code{accept_regex}, but specifying regular expressions to reject
#' @param exclude_directories character: character vector with one or more entries. Each entry specifies a comma-separated list of directories you wish to exclude from download. Elements may contain wildcards
#' @param restrict_file_names character: vector of one of more strings from the set "unix", "windows", "nocontrol", "ascii", "lowercase", and "uppercase". See \url{https://www.gnu.org/software/wget/manual/wget.html#index-Windows-file-names} for more information on this parameter. \code{bb_config} sets this to "windows" by default: if you are downloading files from a server with a port (http://somewhere.org:1234/) Unix will allow the ":" as part of directory/file names, but Windows will not (the ":" will be replaced by "+"). Specifying \code{restrict_file_names="windows"} causes Windows-style file naming to be used
#' @param progress string: the type of progress indicator you wish to use. Legal indicators are "dot" and "bar". "dot" prints progress with dots, with each dot representing a fixed amount of downloaded data. The style can be adjusted: "dot:mega" will show 64K per dot and 3M per line; "dot:giga" shows 1M per dot and 32M per line. See \url{https://www.gnu.org/software/wget/manual/wget.html#index-dot-style} for more information
#' @param user string: username used to authenticate to the remote server
#' @param password string: password used to authenticate to the remote server
#' @param output_file string: save wget's output messages to this file
#' @param robots_off logical: by default wget considers itself to be a robot, and therefore won't recurse into areas of a site that are excluded to robots. This can cause problems with servers that exclude robots (accidentally or deliberately) from parts of their sites containing data that we want to retrieve. Setting \code{robots_off=TRUE} will add a "-e robots=off" flag, which instructs wget to behave as a human user, not a robot. See \url{https://www.gnu.org/software/wget/manual/wget.html#Robot-Exclusion} for more information about robot exclusion
#' @param timestamping logical: if \code{TRUE}, don't re-retrieve a remote file unless it is newer than the local copy (or there is no local copy)
#' @param no_if_modified_since logical: applies when retrieving recursively with timestamping (i.e. only downloading files that have changed since last download, which is achieved using \code{bb_config(...,clobber=1)}). The default method for timestamping is to issue an "If-Modified-Since" header on the request, which instructs the remote server not to return the file if it has not changed since the specified date. Some servers do not support this header. In these cases, trying using \code{no_if_modified_since=TRUE}, which will instead send a preliminary HEAD request to ascertain the date of the remote file
#' @param no_clobber logical: if \code{TRUE}, skip downloads that would overwrite existing local files
#' @param no_parent logical: if \code{TRUE}, do not ever ascend to the parent directory when retrieving recursively. This is \code{TRUE} by default, bacause it guarantees that only the files below a certain hierarchy will be downloaded
#' @param no_check_certificate logical: if \code{TRUE}, don't check the server certificate against the available certificate authorities. Also don't require the URL host name to match the common name presented by the certificate. This option might be useful if trying to download files from a server with an expired certificate, but it is clearly a security risk and so should be used with caution
#' @param relative logical: if \code{TRUE}, only follow relative links. This can sometimes be useful for restricting what is downloaded in recursive mode
#' @param adjust_extension logical: if a file of type 'application/xhtml+xml' or 'text/html' is downloaded and the URL does not end with .htm or .html, this option will cause the suffix '.html' to be appended to the local filename. This can be useful when mirroring a remote site that has file URLs that conflict with directories (e.g. http://somewhere.org/this/page which has further content below it, say at http://somewhere.org/this/page/more. If "somewhere.org/this/page" is saved as a file with that name, that name can't also be used as the local directory name in which to store the lower-level content. Setting \code{adjust_extension=TRUE} will cause the page to be saved as "somewhere.org/this/page.html", thus resolving the conflict
#' @param retr_symlinks logical: if \code{TRUE}, follow symbolic links during recursive download. Note that this will only follow symlinks to files, NOT to directories
#' @param extra_flags character: character vector of additional command-line flags to pass to wget
#' @param verbose logical: print trace output?
#' @param capture_stdout logical: if \code{TRUE}, return 'stdout' and 'stderr' output in the returned object (see exec_internal from the sys package). Otherwise send these outputs to the console
#' @param quiet logical: if \code{TRUE}, suppress wget's output
#' @param debug logical: if \code{TRUE}, wget will print lots of debugging information. If wget is not behaving as expected, try setting this to \code{TRUE}
#'
#' @return the result of the system call (or if \code{bb_wget("--help")} was called, a message will be issued). The returned object will have components 'status' and (if \code{capture_stdout} was \code{TRUE}) 'stdout' and 'stderr'
#'
#' @seealso \code{\link{bb_install_wget}}, \code{\link{bb_find_wget}}
#' @examples
#' \dontrun{
#'   ## get help about wget command line parameters
#'   bb_wget("help")
#' }
#'
# @export
## like bb_wget, but with some wget flags promoted to explicit function parms
bb_wget <- function(url,recursive=TRUE,level=1,wait=0,accept,reject,accept_regex,reject_regex,exclude_directories,restrict_file_names,progress,user,password,output_file,robots_off=FALSE,timestamping=FALSE,no_if_modified_since=FALSE,no_clobber=FALSE,no_parent=TRUE,no_check_certificate=FALSE,relative=FALSE,adjust_extension=FALSE,retr_symlinks=FALSE,extra_flags=character(),verbose=FALSE,capture_stdout=FALSE,quiet=FALSE,debug=FALSE) {
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
    if (missing(user)) user <- ""
    assert_that(is.string(user))
    if (missing(password)) password <- ""
    assert_that(is.string(password))
    if (missing(output_file)) output_file <- ""
    assert_that(is.string(output_file))
    assert_that(is.flag(robots_off),!is.na(robots_off))
    assert_that(is.flag(timestamping),!is.na(timestamping))
    assert_that(is.flag(no_if_modified_since),!is.na(no_if_modified_since))
    assert_that(is.flag(no_clobber),!is.na(no_clobber))
    assert_that(is.flag(no_parent),!is.na(no_parent))
    assert_that(is.flag(no_check_certificate),!is.na(no_check_certificate))
    assert_that(is.flag(relative),!is.na(relative))
    assert_that(is.flag(adjust_extension),!is.na(adjust_extension))
    assert_that(is.flag(retr_symlinks),!is.na(retr_symlinks))
    assert_that(is.numeric(wait))
    assert_that(is.character(extra_flags))
    assert_that(is.flag(verbose),!is.na(verbose))
    assert_that(is.flag(capture_stdout),!is.na(capture_stdout))
    assert_that(is.flag(quiet),!is.na(quiet))
    assert_that(is.flag(debug),!is.na(debug))
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
        flags <- c(flags,vapply(exclude_directories,function(z)paste0("--exclude-directories=",z),FUN.VALUE="",USE.NAMES=FALSE))
        if (length(restrict_file_names)>0) {
            restrict_file_names <- paste(restrict_file_names,sep=",",collapse=",")
            flags <- c(flags,paste0("--restrict-file-names=",restrict_file_names))
        }
        if (length(progress)>0 && nzchar(progress)) flags <- c(flags,paste0("--progress=",progress))
        if (length(user)>0 && nzchar(user)) flags <- c(flags,paste0("--user=",user))
        if (length(password)>0 && nzchar(password)) flags <- c(flags,paste0("--password=",password))
        if (length(output_file)>0 && nzchar(output_file)) flags <- c(flags,paste0("--output-file=",output_file))
        if (recursive && no_parent) flags <- c(flags,"--no-parent")
        if (timestamping) flags <- c(flags,"--timestamping")
        if (no_clobber) flags <- c(flags,"--no-clobber")
        if (no_if_modified_since) flags <- c(flags,"--no-if-modified-since")
        if (no_check_certificate) flags <- c(flags,"--no-check-certificate")
        if (relative) flags <- c(flags,"--relative")
        if (adjust_extension) flags <- c(flags,"--adjust-extension")
        if (retr_symlinks) flags <- c(flags,"--retr-symlinks")
        if (wait>0) flags <- c(flags,paste0("--wait=",wait))
        if (quiet) flags <- c(flags,"--quiet")
        if (debug) flags <- c(flags,"--debug")
        if (robots_off) flags <- c(flags,"-e robots=off")
        ## add any extra_flags
        ## sys expects flags as a char vector, not a string
        extra_flags <- flags_to_charvec(extra_flags) ## will split string, or replace NA/"" with empty character vector
        flags <- c(flags,extra_flags)
        if (verbose) cat(sprintf(" executing wget %s %s\n",paste(flags,collapse=" "),url))
        if (capture_stdout) {
            out <- sys::exec_internal(bb_find_wget(),args=c(flags,url),error=FALSE)
        } else {
            status <- sys::exec_wait(bb_find_wget(),args=c(flags,url),std_out=TRUE,std_err=TRUE)
            out <- list(status=status)
        }
        out$status_text <- decode_wget_exit_status(out$status)
        if ((is.na(out$status) || out$status>0) && verbose)
            cat(sprintf(" bb_wget exited with status code %d indicating an error (%s)\n",out$status,out$status_text))
        out
    }
}


#' Install wget
#'
#' This is a helper function to install wget. Currently it only works on Windows platforms. The wget.exe executable will be downloaded from https://eternallybored.org/misc/wget/ and saved to either a temporary directory or your user appdata directory (see the \code{use_appdata_dir} parameter).
#'
#' @references https://eternallybored.org/misc/wget/
#' @param force logical: force reinstallation if wget already exists
#' @param use_appdata_dir logical: by default, \code{bb_install_wget} will install wget into a temporary directory, which does not persist between R sessions. If you want a persistent installation, specify \code{use_appdata_dir=TRUE} to install wget into your appdata directory (on Windows, typically something like C:/Users/username/AppData/Roaming/)
#'
#' @return the path to the installed executable
#'
#' @examples
#' \dontrun{
#'   bb_install_wget()
#'
#'   ## confirm that it worked:
#'   bb_wget("help")
#' }
#'
#' @seealso \code{\link{bb_find_wget}}
#'
#' @export
bb_install_wget <- function(force=FALSE,use_appdata_dir=FALSE) {
    assert_that(is.flag(force),!is.na(force))
    assert_that(is.flag(use_appdata_dir),!is.na(use_appdata_dir))
    if (!force) {
        existing_wget <- bb_find_wget(install=FALSE,error=FALSE)
        if (!is.null(existing_wget)) {
            message("wget already exists and force is FALSE, not reinstalling")
            return(existing_wget)
        }
    }
    my_os <- get_os()
    if (my_os!="windows") {
        errmsg <- paste0("bb_install_wget only supports windows platforms.\n You will need to install wget yourself and ensure that it is on the system path.",
                         switch(my_os,
                                "osx"="\n On OSX use \"brew install wget\" or \"brew install --with-libressl wget\" if you get SSL-related errors.\n If you do not have brew installed, see https://brew.sh/ (note: you will need admin privileges to install brew).",
                                "unix"=,
                                "linux"="\n On Linux use e.g. \"sudo apt install wget\" on Debian/Ubuntu, or \"sudo yum install wget\" on Fedora/CentOS.\n Note: you will need admin privileges for this.",
                                ""))
        stop(errmsg)
    }
    ## NOTE, could also use e.g. https://github.com/r-lib/rappdirs to find this directory
    path <- if (use_appdata_dir) Sys.getenv("APPDATA") else tempdir()
    if (dir_exists(path)) {
        path <- file.path(path,"bowerbird")
        if (!dir_exists(path)) dir.create(path)
        if (!dir_exists(path)) stop("could not create directory ",path," to store the wget executable")
        ## there used to be a convenient URL to the latest wget version: https://eternallybored.org/misc/wget/current/wget.exe
        ## as of Jan-2018 this does not seem to exist any more, so we point to specific version here instead
        ## Do we want 32 or 64 bit exe? Default to 32, since 32-bit wget will run on 64-bit windows if it has to
        bits <- tryCatch(if (.Machine$sizeof.pointer==8) 64 else 32,error=function(e) 32)
        ## 8-byte address space is 64-bit. Note that we're actually detecting the R address space here, not the OS address space. But I don't think there's a reliable way of detecting the machine architecture
        wgurl <- paste0("https://eternallybored.org/misc/wget/1.19.4/",bits,"/wget.exe")
        err <- download.file(wgurl,
                      destfile=file.path(path,"wget.exe"),
                      mode="wb")
        if (!err) {
            myexe <- file.path(path,"wget.exe")
            ## update the bowerbird options so that we'll find it next time we try
            bb_opts <- getOption("bowerbird")
            if (is.null(bb_opts)) bb_opts <- list()
            bb_opts$wget_exe <- myexe
            options(bowerbird=bb_opts)
            ## return the path to the wget executable
            myexe
        } else {
            stop("Sorry, wget install failed.\n You will need to install wget yourself and ensure that it is on the system path.")
        }
    } else {
        stop("Sorry, wget install failed (could not find the user APPDATA directory to install into).\n You will need to install wget yourself and ensure that it is on the system path.")
    }
}

#' Find the wget executable
#'
#' This function will return the path to the wget executable if it can be found on the local system, and optionally install it if it is not found. Installation (if required) currently only works on Windows platforms. The wget.exe executable will be downloaded from https://eternallybored.org/misc/wget/ installed into your appdata directory (typically something like C:/Users/username/AppData/Roaming/)
#'
#' @references https://eternallybored.org/misc/wget/
#'
#' @param install logical: attempt to install the executable if it is not found? (Windows only)
#' @param error logical: if wget is not found, raise an error. If \code{FALSE}, do not raise an error but return NULL
#'
#' @return the path to the wget executable, or (if error is \code{FALSE}) NULL if it was not found
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
bb_find_wget <- function(install=FALSE,error=TRUE) {
    assert_that(is.flag(install),!is.na(install))
    bb_opts <- getOption("bowerbird")
    if (!is.null(bb_opts)) {
        if (!is.null(bb_opts$wget_exe)) {
            ## already set
            ## check that it actually exists
            if (file.exists(bb_opts$wget_exe)) {
                ## ok
                return(bb_opts$wget_exe)
            } else {
                ## it's disappeared somehow
                bb_opts$wget_exe <- NULL
                options(bowerbird=bb_opts)
                ## try again
                return(bb_find_wget(install=install,error=error))
            }
        }
    } else {
        bb_opts <- list()
    }
    if (wget_test("wget")) {
        myexe <- unname(Sys.which("wget"))
    } else {
        my_os <- get_os()
        if (my_os=="windows") {
            myexe <- file.path(Sys.getenv("APPDATA"),"bowerbird","wget.exe")
            if (!wget_test(myexe)) {
                if (!install) {
                    if (error) stop("could not find the wget executable.\n Try the bb_install_wget() function, or install wget yourself and ensure that it is on the system path")
                    ## note that if we tried bb_find_wget(install=TRUE) and it failed, we will get here and the error message will suggesting trying bb_install_wget, which is a bit circular. To fix later, perhaps ...
                    return(NULL)
                } else {
                    ## install wget
                    bb_install_wget()
                    ## and try again
                    return(bb_find_wget(install=FALSE,error=error))
                }
            }
        } else {
            if (install && my_os!="windows") {
                warning("install=TRUE only currently works on Windows platforms, ignoring")
            }
            if (error) {
                switch(my_os,
                       "osx"=stop("could not find the wget executable.\n You will need to install wget yourself and ensure that it is on the system path.\n Use \"brew install wget\" or \"brew install --with-libressl wget\" if you get SSL-related errors.\n If you do not have brew installed, see https://brew.sh/ (note: you will need admin privileges to install brew)."),
                       "unix"=,
                       "linux"=stop("could not find the wget executable.\n You will need to install wget yourself and ensure that it is on the system path.\n Use e.g. \"sudo apt install wget\" on Debian/Ubuntu, or \"sudo yum install wget\" on Fedora/CentOS.\n Note: you will need admin privileges for this."),
                       stop("could not find the wget executable.\n You will need to install wget yourself and ensure that it is on the system path.")
               )
            }
            return(NULL)
        }
    }
    bb_opts$wget_exe <- myexe
    options(bowerbird=bb_opts)
    myexe
}

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

## internal: merge two sets of wget flags
## used to add the wget_global_flags (part of the overall config) with source-specific flags
merge_wget_flags <- function(primary_flags,secondary_flags) {
    assert_that(is.list(primary_flags))
    assert_that(is.list(secondary_flags))
    f1 <- names(primary_flags)
    f2 <- names(secondary_flags)
    for (thisflag in setdiff(f2,f1)) {
        switch(thisflag,
               "timestamping"={ if (! "no_clobber" %in% f1) primary_flags <- c(primary_flags,list(timestamping=TRUE)) },
               "no_clobber"={ if (! "timestamping" %in% f1) primary_flags <- c(primary_flags,list(no_clobber=TRUE)) },
               "extra_flags"={
                   ## don't do anything with this, handled below
               },
               primary_flags <- c(primary_flags,secondary_flags[thisflag]))
    }
    if ("extra_flags" %in% f1) {
        ## note that we make no attempt to resolve conflicts between flags here
        ## but we do put the primary flags last, assuming that wget will follow the
        ## last instruction given if there are conflicts
        primary_flags$extra_flags <- c(flags_to_charvec(secondary_flags$extra_flags),flags_to_charvec(primary_flags$extra_flags))
    } else {
        primary_flags$extra_flags <- flags_to_charvec(secondary_flags$extra_flags)
    }
    primary_flags
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

## internal: turn a wget exit code into a meaningful message
decode_wget_exit_status <- function(status) {
    ## see https://www.gnu.org/software/wget/manual/html_node/Exit-Status.html
    out <- "unknown status code"
    if (is.numeric(status) && status>=0 & status<=8) {
        out <- switch(as.character(status),
                      "0"="no problems occurred",
                      "1"="generic error code",
                      "2"="parse error, for instance when parsing command-line options or the .wgetrc or .netrc files",
                      "3"="file I/O error",
                      "4"="network failure",
                      "5"="SSL verification failure",
                      "6"="username/password authentication failure",
                      "7"="protocol error",
                      "8"="server issued an error response",
                      "unknown status code")
    }
    out
}
