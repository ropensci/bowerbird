##             do_wget(build_wget_call(this_dataset),this_dataset)


#' Use wget to mirror an external data repository
#'
#' @param url string: the URL to retrieve 
#' @param flags string: command-line flags to pass to wget 
#'
#' @return TRUE on success
#'
#' @seealso \code{\link{install_wget}}
#'
# @export
wget <- function(url,flags) {
    wgetexe <- wget_exe()
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
            if (!wget_text(myexe)) {
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
                
##dir_exists <- function(z) utils::file_test("-d",z)
##file_exists <- function(x) utils::file_test('-f', x)
