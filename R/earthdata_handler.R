#' Handler for files files served up through Earthdata providers
#'
#' @references https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+With+Earthdata+Login
#' @param data_source data.frame: single-row data.frame defining a data source, e.g. as returned by \code{bb_source}
#' @param verbose logical: if TRUE, provide additional progress output
#' @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
#'
#' @return the directory if local_dir_only is TRUE, otherwise TRUE on success
#'
#' @export
earthdata_get <- function(data_source,verbose=FALSE,local_dir_only=FALSE) {
    assert_that(is.data.frame(data_source))
    assert_that(nrow(data_source)==1)
    assert_that(is.flag(verbose))
    assert_that(is.flag(local_dir_only))

    if (local_dir_only) return(bb_wget(data_source,verbose=verbose,local_dir_only=TRUE))
    if (na_or_empty(data_source$user) || na_or_empty(data_source$password))
        stop(sprintf("Earthdata source \"%s\" requires user and password",data_source$name))
    cookies_file <- tempfile()
    ## create this file
    if (!file.exists(cookies_file)) cat("",file=cookies_file)
    dummy <- data_source
    dummy$method_flags <- paste0("--http-user='",dummy$user,"' --http-password='",dummy$password,"' --load-cookies \"",cookies_file,"\" --save-cookies \"",cookies_file,"\" --keep-session-cookies --no-check-certificate --auth-no-challenge -r --reject \"index.html*\" -np -e robots=off ",dummy$method_flags," ",dummy$wget_global_flags)
    dummy$user <- NA_character_
    dummy$password <- NA_character_
    bb_wget(dummy,verbose=verbose)
}
