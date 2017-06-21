#' Handler for files files served up through Earthdata providers
#'
#' @references https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+With+Earthdata+Login
#' @param data_source tibble: single-row tibble defining a data source, e.g. as returned by \code{bb_source}
#'
#' @return TRUE on success
#'
#' @export
earthdata_get <- function(data_source) {
    assert_that(is.data.frame(data_source))
    assert_that(nrow(data_source)==1)
    if (na_or_empty(data_source$user) || na_or_empty(data_source$password))
        stop(sprintf("Earthdata source \"%s\" requires user and password",data_source$name))
    cookies_file <- tempfile()
    ## create this file
    if (!file.exists(cookies_file)) cat("",file=cookies_file)
    dummy <- data_source
    dummy$method_flags <- paste0("--http-user=\"",dummy$user,"\" --http-password=\"",dummy$password,"\" --load-cookies \"",cookies_file,"\" --save-cookies \"",cookies_file,"\" --keep-session-cookies --no-check-certificate --auth-no-challenge -r --reject \"index.html*\" -np -e robots=off ",dummy$method_flags," ",dummy$wget_global_flags)
    dummy$user <- as.character(NA)
    dummy$password <- as.character(NA)
    bb_wget(dummy)
}
