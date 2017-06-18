#' Helper function to install wget on Windows
#'
#' @references https://eternallybored.org/misc/wget/current/wget.exe
#'
#' @return TRUE on success
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
wget_exe <- function() {
    if (wget_test("wget")) {
        "wget"
    } else {
        if (.Platform$OS.type=="windows") {
            trythis <- file.path(Sys.getenv("APPDATA"),"bowerbird","wget.exe")
            if (wget_test(trythis)) return(trythis)
            stop("Could not find the wget executable. Try the install_wget() function, or install it yourself and ensure that it is on the path")
        }
        stop("could not find the wget executable")
    }
}
wget_test <- function(wget_path) {
    try({system(paste0(wget_path," --help"),intern=TRUE);return(TRUE)},silent=TRUE)
    FALSE
}
                
##dir_exists <- function(z) utils::file_test("-d",z)
##file_exists <- function(x) utils::file_test('-f', x)
