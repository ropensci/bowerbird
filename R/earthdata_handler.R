#' Handler for files files served up through Earthdata providers
#'
#' @references https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+With+Earthdata+Login
#' @param cfrow data.frame: a single row from a bowerbird configuration (as returned by \code{bb_config})
#' @param verbose logical: if TRUE, provide additional progress output
#' @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
#'
#' @return the directory if local_dir_only is TRUE, otherwise TRUE on success
#'
#' @export
earthdata_get <- function(cfrow,verbose=FALSE,local_dir_only=FALSE) {
    assert_that(is.data.frame(cfrow))
    assert_that(nrow(cfrow)==1)
    assert_that(is.flag(verbose))
    assert_that(is.flag(local_dir_only))

    if (local_dir_only) return(bb_wget(cfrow,verbose=verbose,local_dir_only=TRUE))
    if (na_or_empty(cfrow$user) || na_or_empty(cfrow$password))
        stop(sprintf("Earthdata source \"%s\" requires user and password",cfrow$name))
    cookies_file <- tempfile()
    ## create this file
    if (!file.exists(cookies_file)) cat("",file=cookies_file)
    dummy <- cfrow
    mflags <- strsplit(dummy$method_flags,"[[:space:]]+")[[1]]
    ##dummy$method_flags <- paste0("--http-user ",dummy$user," --http-password ",dummy$password," --load-cookies ",cookies_file," --save-cookies ",cookies_file," --keep-session-cookies --no-check-certificate --auth-no-challenge -r --reject index.html* -np -e robots=off ",dummy$method_flags)
    dummy$method_flags <- list(c("--http-user",dummy$user,"--http-password",dummy$password,"--load-cookies",cookies_file,"--save-cookies",cookies_file,"--keep-session-cookies","--no-check-certificate","--auth-no-challenge","-e","robots=off",mflags))
    dummy$user <- NA_character_
    dummy$password <- NA_character_
    bb_wget(dummy,verbose=verbose)
}
