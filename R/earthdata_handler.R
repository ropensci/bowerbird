#' Handler for files files served up through Earthdata providers
#'
#' This function is not intended to be called directly by the user, but rather will be called internally by the \code{bb_sync} function. The typical usage of \code{bb_handler_earthdata} is to specify it in the \code{method} parameter of a definition: see the example below.
#'
#' @references https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+With+Earthdata+Login
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
#' @param verbose logical: if TRUE, provide additional progress output
#' @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
#' @param ... : additional parameters passed through to bb_handler_wget
#'
#' @return the directory if local_dir_only is TRUE, otherwise TRUE on success
#'
#' @examples
#' \dontrun{
#'
#' ## note that the full version of this data source is provided as part of bb_example_data_sources()
#'
#' my_source <- bb_source(
#'   name="Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 2",
#'   id="10.5067/EYICLBOAAJOU",
#'   description="NSIDC provides this data set ... [truncated; see bb_example_data_sources()]",
#'   doc_url="https://nsidc.org/data/NSIDC-0192/versions/2",
#'   citation="Stroeve, J. and W. Meier. 2017. ... [truncated; see bb_example_data_sources()]",
#'   source_url=c("https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0192_seaice_trends_climo_v2/"),
#'   license="Please cite, see http://nsidc.org/about/use_copyright.html",
#'   authentication_note="Requires Earthdata login, see https://urs.earthdata.nasa.gov/.
#'     Note that you will also need to authorize the application 'nsidc-daacdata'
#'     (see 'My Applications' at https://urs.earthdata.nasa.gov/profile)",
#'   method=list("bb_handler_earthdata",recursive=TRUE,level=4,no_parent=TRUE,relative=TRUE),
#'   user="your_earthdata_username",
#'   password="your_earthdata_password",
#'   collection_size=0.02)
#' }
#'
#' @export
bb_handler_earthdata <- function(config,verbose=FALSE,local_dir_only=FALSE,...) {
    assert_that(is(config,"bb_config"))
    assert_that(nrow(bb_data_sources(config))==1)
    assert_that(is.flag(verbose),!is.na(verbose))
    assert_that(is.flag(local_dir_only),!is.na(local_dir_only))

    ## the earthdata-recommended way to use wget requires a .netrc file with username and password
    ## see e.g. https://wiki.earthdata.nasa.gov/display/EL/How+To+Access+Data+With+cURL+And+Wget
    ## note preauthorization needed: https://wiki.earthdata.nasa.gov/display/EL/How+To+Pre-authorize+an+application

    ## https://nsidc.org/support/faq/what-options-are-available-bulk-downloading-data-https-earthdata-login-enabled

    if (local_dir_only)
        return(bb_handler_wget(config,verbose=verbose,local_dir_only=TRUE,...))

    dummy <- bb_data_sources(config)
    if (na_or_empty(dummy$user) || na_or_empty(dummy$password))
            stop(sprintf("Earthdata source \"%s\" requires user and password",dummy$name))
    cookies_file <- gsub("\\\\","/",tempfile()) ## probably don't need the gsub, was there for windows debugging
    ## create this file
    if (!file.exists(cookies_file)) cat("",file=cookies_file)
    on.exit(file.remove(cookies_file))
    ##mflags <- list(...)##flags_to_charvec(dummy$method_flags)
    ## must use --auth-no-challenge else the server redirects to the html login page, rather than accepting the provided credentials
    dummy$method <- list("bb_handler_wget",...,extra_flags=c("--http-user",dummy$user,"--http-password",dummy$password,"--auth-no-challenge","--load-cookies",cookies_file,"--save-cookies",cookies_file,"--keep-session-cookies"),reject="index.html*",execute=c("robots=off"))
    ##dummy$method_flags <- list(c(mflags,"--http-user",dummy$user,"--http-password",dummy$password,"--auth-no-challenge","--load-cookies",cookies_file,"--save-cookies",cookies_file,"--keep-session-cookies","--reject=index.html*","-e","robots=off"))
    dummy$user <- NA_character_
    dummy$password <- NA_character_
    bb_data_sources(config) <- dummy
    ## must make the wget call twice: first time it will authenticate, and write the cookies, but then redirect to the original page and wget won't go further because it knows it's already been there and doesn't want to get into an infinite loop
    do.call(bb_handler_wget,c(list(config,verbose=verbose),dummy$method[-1]))
    ## but the second time it will authenticate using the stored cookie and proceed with the recursion
    do.call(bb_handler_wget,c(list(config,verbose=verbose),dummy$method[-1]))
}
