#' Mirror an external data source using the wget utility
#'
#' @param data_source tibble: single-row tibble defining a data source, e.g. as returned by \code{bb_source}
#' @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
#'
#' @return the directory if local_dir_only is TRUE, otherwise TRUE on success
#'
#' @seealso \code{\link{wget}}
#'
#' @export
bb_wget <- function(data_source,local_dir_only=FALSE) {
    assert_that(is.data.frame(data_source))
    assert_that(nrow(data_source)==1)
    assert_that(is.flag(local_dir_only))

    if (local_dir_only)
        return(file.path(bb_attributes(data_source)$local_file_root,directory_from_url(data_source$source_url)))
    
    this_flags <- if (is.na(data_source$method_flags)) data_source$wget_default_flags else data_source$method_flags
    ## add wget_global_flags
    if (!is.null(data_source$wget_global_flags)) this_flags <- paste(this_flags,data_source$wget_global_flags,sep=" ")
    ## proxy-user and proxy-password flags
    ## this needs to decide whether it should use http_proxy_user or ftp_proxy_user info - exclude for now
    #if (!grepl("proxy-user=",data_source$wget_flags)) {
    #    data_source$wget_flags=paste(data_source$wget_flags,paste0("--proxy-user=",data_source$http_proxy_user),sep=" ")
    #}
    #if (!grepl("proxy-password=",data_source$wget_flags)) {
    #    data_source$wget_flags=paste(data_source$wget_flags,paste0("--proxy-password=",data_source$http_proxy_password),sep=" ")
                                        #}
    ## add flags for clobber behaviour
    if (!is.null(data_source$clobber) && !is.na(data_source$clobber) && data_source$clobber %in% c(0,1)) {
        if (data_source$clobber==0) {
            this_flags <- resolve_wget_clobber_flags(this_flags,"--no-clobber")
        } else {
            this_flags <- resolve_wget_clobber_flags(this_flags,"--timestamping")
        }
    }
    ## add user, password flags
    if (!is.na(data_source$user) && nchar(data_source$user)>0) this_flags <- paste0(this_flags," --user='",data_source$user,"'")
    if (!is.na(data_source$password) && nchar(data_source$password)>0) this_flags <- paste0(this_flags," --password='",data_source$password,"'")
    ##if (data_source$wait>0) this_flags <- paste0(this_flags," --wait=",data_source$wait)

    if (!is.null(data_source$skip_downloads) && data_source$skip_downloads) {
        cat(sprintf(" skip_downloads is TRUE, not executing: wget %s %s\n",this_flags,data_source$source_url))
        ok <- TRUE
    } else {
        if (sink.number()>0) {
            ## we have a sink() redirection in place
            ## sink() won't catch the output of system commands, which means we miss stuff in our log
            ## workaround: send output to temporary file so that we can capture it
            output_file <- gsub("\\\\","\\\\\\\\",tempfile()) ## escape backslashes
            this_flags <- paste0("-o \"",output_file,"\" ",this_flags)
            ok <- wget(data_source$source_url,this_flags)
            ## now echo the contents of output_file to console, so that sink() captures it
            cat(readLines(output_file),sep="\n")
        } else {
            ok <- wget(data_source$source_url,this_flags)
        }
    }
    ok==0    
}


#' Make a wget call
#'
#' @param url string: the URL to retrieve 
#' @param flags string: command-line flags to pass to wget
#' @param verbose logical: print trace output?
#' @param ... : additional paramaters passed to \code{system2}
#'
#' @return the result of the system2 call
#'
#' @seealso \code{\link{install_wget}} \code{\link{system2}}
#' @examples
#' \dontrun{
#' ## get help about wget command line parameters
#' wget("--help")
#' }
#'
# @export
wget <- function(url,flags,verbose=TRUE,...) {
    assert_that(is.string(url))
    if (tolower(url) %in% c("-h","--help") || tolower(flags) %in% c("-h","--help")) {
        system2(wget_exe(),"--help",...)
    } else {
        if (verbose) cat(sprintf(" executing wget %s %s\n",flags,url))
        system2(wget_exe(),paste(flags,url,sep=" "),...)
    }
}

#' Helper function to install wget on Windows
#'
#' The wget.exe executable will be downloaded from https://eternallybored.org/misc/wget/current/wget.exe and installed into your appdata directory (typically something like C:/Users/username/AppData/Roaming/)
#' 
#' @references https://eternallybored.org/misc/wget/current/wget.exe
#'
#' @return TRUE (invisibly) on success
#'
#' @examples
#' \dontrun{
#'   install_wget()
#' }
#'
#' @export
install_wget <- function() {
    if (.Platform$OS.type!="windows")
        stop("install_wget only supports windows platforms")
    ## NOTE, could also use e.g. https://github.com/r-lib/rappdirs to find this directory
    path <- Sys.getenv("APPDATA")
    if (dir_exists(path)) {
        path <- file.path(path,"bowerbird")
        if (!dir_exists(path)) dir.create(path)
        if (!dir_exists(path)) stop("could not create directory ",path," to store the wget executable")
        ok <- download.file("https://eternallybored.org/misc/wget/current/wget.exe",
                      destfile=file.path(path,"wget.exe"),
                      mode="wb")
        invisible(ok==0)
    } else {
        stop("Sorry, could not find the user APPDATA directory to install wget into")
    }
}

## helper function to return the wget executable name (possibly with path)
## if successfully identified, set the bowerbird$wget_exe option (and use this on subsequent calls)
wget_exe <- function() {
    bb_opts <- getOption("bowerbird")
    if (!is.null(bb_opts)) {
        if (!is.null(bb_opts$wget_exe)) return(bb_opts$wget_exe)
    } else {
        bb_opts <- list()
    }
    if (wget_test("wget")) {
        myexe <- "wget"
    } else {
        if (.Platform$OS.type=="windows") {
            myexe <- file.path(Sys.getenv("APPDATA"),"bowerbird","wget.exe")
            if (!wget_test(myexe)) {
                stop("could not find the wget executable. Try the install_wget() function, or install it yourself and ensure that it is on the path")
            }
        } else {
            stop("could not find the wget executable")
        }
    }
    bb_opts$wget_exe <- myexe
    options(bowerbird=bb_opts)
    myexe
}
## test a potential wget executable path
wget_test <- function(wget_path) {
    try({system(paste0(wget_path," --help"),intern=TRUE);return(TRUE)},silent=TRUE)
    FALSE
}


resolve_wget_clobber_flags <- function(primary_flags,secondary_flags) {
    wgf <- str_split(primary_flags,"[ ]+")[[1]]
    secondary_flags <- str_split(secondary_flags,"[ ]+")[[1]]
    for (thisflag in setdiff(secondary_flags,wgf)) {
        switch(thisflag,
               "-N"=,
               "--timestamping"={ if (! any(c("--no-clobber","-nc") %in% wgf)) wgf <- c(wgf,thisflag) },
               "-nc"=,
               "--no-clobber"={ if (! any(c("--timestamping","-N") %in% wgf)) wgf <- c(wgf,thisflag) },
               wgf <- c(wgf,thisflag))
    }
    paste(wgf,sep="",collapse=" ")
}


##dir_exists <- function(z) utils::file_test("-d",z)
##file_exists <- function(x) utils::file_test('-f', x)
