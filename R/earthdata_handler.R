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

    ## the earthdata-recommended way to use wget requires a .netrc file with username and password
    ## see e.g. https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
    ## note preauthorization needed: https://wiki.earthdata.nasa.gov/display/EL/How+To+Pre-authorize+an+application

    ## https://nsidc.org/support/faq/what-options-are-available-bulk-downloading-data-https-earthdata-login-enabled

    if (local_dir_only)
        return(bb_handler_wget(config,verbose=verbose,local_dir_only=TRUE))

    dummy <- bb_data_sources(config)
    if (na_or_empty(dummy$user) || na_or_empty(dummy$password))
            stop(sprintf("Earthdata source \"%s\" requires user and password",dummy$name))
    cookies_file <- gsub("\\\\","/",tempfile()) ## probably don't need the gsub, was there for windows debugging
    ## create this file
    if (!file.exists(cookies_file)) cat("",file=cookies_file)
#    netrc_file <- tempfile()
#    on.exit(file.remove(netrc_file))
#    ## from https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
#    ## A backslash or space anywhere in your password will need to be escaped with an additional backslash. Similarly, if you use a '#' as the first character of your password, it will also need to be escaped with a preceding backslash
#    safe_pw <- gsub("\\\\","\\\\\\\\",dummy$password)
#    safe_pw <- gsub("[[:space:]]","\\ ",safe_pw)
#    safe_pw <- gsub("^#","\\#",safe_pw)
#    cat(sprintf("machine urs.earthdata.nasa.gov login %s password %s",dummy$user,safe_pw),file=netrc_file,append=FALSE)
#
    mflags <- flags_to_charvec(dummy$method_flags)
    dummy$method_flags <- list(c(mflags,"--http-user",dummy$user,"--http-password",dummy$password,"--auth-no-challenge","--load-cookies",cookies_file,"--save-cookies",cookies_file,"--keep-session-cookies","-e","robots=off")) ##"--no-check-certificate" , ##"--reject=index.html*",,"--no-hsts" "--span-hosts","--max-redirect=10",
    dummy$user <- NA_character_
    dummy$password <- NA_character_
    bb_data_sources(config) <- dummy
    ## must make the wget call twice: first time it will authenticate, and write the cookies, but then redirect to the original page and wget won't go further because it knows it's already been there and doesn't want to get into an infinite loop
    bb_handler_wget(config,verbose=verbose)
    ## but the second time it will authenticate using the stored cookie and proceed with the recursion
    bb_handler_wget(config,verbose=verbose)
}
