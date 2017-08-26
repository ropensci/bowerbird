#' Handler for files files served up through Earthdata providers
#'
#' @references https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+With+Earthdata+Login
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
#' @param verbose logical: if TRUE, provide additional progress output
#' @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
#'
#' @return the directory if local_dir_only is TRUE, otherwise TRUE on success
#'
#' @export
earthdata_get <- function(config,verbose=FALSE,local_dir_only=FALSE) {
    assert_that(is(config,"bb_config"))
    assert_that(nrow(config$data_sources)==1)
    assert_that(is.flag(verbose))
    assert_that(is.flag(local_dir_only))

    if (local_dir_only)
        return(bb_wget(config,verbose=verbose,local_dir_only=TRUE))

    cfrow <- bb_attributes_to_cols(config)
    if (na_or_empty(cfrow$user) || na_or_empty(cfrow$password))
        stop(sprintf("Earthdata source \"%s\" requires user and password",cfrow$name))
    cookies_file <- gsub("\\\\","/",tempfile()) ## probably don't need the gsub, was there for windows debugging
    ## create this file
    if (!file.exists(cookies_file)) cat("",file=cookies_file)
    dummy <- cfrow
    mflags <- dummy$method_flags
    if (is.string(mflags))
        mflags <- strsplit(mflags,"[[:space:]]+")[[1]]
    ##dummy$method_flags <- paste0("--http-user ",dummy$user," --http-password ",dummy$password," --load-cookies ",cookies_file," --save-cookies ",cookies_file," --keep-session-cookies --no-check-certificate --auth-no-challenge -r --reject index.html* -np -e robots=off ",dummy$method_flags)
    dummy$method_flags <- list(c(mflags,"--http-user",dummy$user,"--http-password",dummy$password,"--load-cookies",cookies_file,"--save-cookies",cookies_file,"--keep-session-cookies","--reject=index.html*","-e","robots=off")) ##"--no-check-certificate" "--auth-no-challenge",
    dummy$user <- NA_character_
    dummy$password <- NA_character_
    bb_wget(dummy,verbose=verbose)
}
