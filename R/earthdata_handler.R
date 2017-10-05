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
bb_handler_earthdata <- function(config,verbose=FALSE,local_dir_only=FALSE) {
    assert_that(is(config,"bb_config"))
    assert_that(nrow(bb_data_sources(config))==1)
    assert_that(is.flag(verbose))
    assert_that(is.flag(local_dir_only))

    if (local_dir_only)
        return(bb_handler_wget(config,verbose=verbose,local_dir_only=TRUE))

    dummy <- bb_data_sources(config)
    if (na_or_empty(dummy$user) || na_or_empty(dummy$password))
            stop(sprintf("Earthdata source \"%s\" requires user and password",dummy$name))
    cookies_file <- gsub("\\\\","/",tempfile()) ## probably don't need the gsub, was there for windows debugging
    ## create this file
    if (!file.exists(cookies_file)) cat("",file=cookies_file)
    mflags <- flags_to_charvec(dummy$method_flags)
    dummy$method_flags <- list(c(mflags,"--http-user",dummy$user,"--http-password",dummy$password,"--load-cookies",cookies_file,"--save-cookies",cookies_file,"--keep-session-cookies","--reject=index.html*","-e","robots=off")) ##"--no-check-certificate" "--auth-no-challenge",
    dummy$user <- NA_character_
    dummy$password <- NA_character_
    bb_data_sources(config) <- dummy
    bb_handler_wget(config,verbose=verbose)
}
