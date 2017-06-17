#' Load or initialize bowerbird configuration
#'
#' If \code{file} is provided, load that configuration. Otherwise initialize an empty configuration.
#' @param file string: file name of configuration to load. All other parameters are ignored if \code{file} is provided.
#' @param cf list: configuration, as returned by \code{bb_config}
#' @param source tibble: external data source definition to add to the configuration
#' @return configuration
#'
#' @seealso \code{\link{bb_source}}
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config() %>%
#'     add(bb_source("seaice_smmr_ssmi_nasateam"))
#' 
#'   ## save to file
#'   bb_save_config(cf,file="saved_config.json")
#'   ## load previously saved config
#'   cf <- bb_config(file="saved_config.json")
#' }
#'
#' @export
bb_config <- function(file) {
    if (!missing(file)) {
        cf <- fromJSON(file)
        if ("global" %in% names(cf)) cf$global <- tbl_df(cf$global)
        if ("sources" %in% names(cf)) cf$sources <- tbl_df(cf$sources)
    } else {
        list(global=tibble(
                 wget_flags="--progress=dot:giga",
                 http_proxy="",
                 ftp_proxy="",
                 local_file_root="",
                 clobber=1,
                 skip_downloads=FALSE,
                 wait=0),
             sources=tibble()
             )
    }
}

#' @rdname bb_config
bb_save_config <- function(cf,file) {
    assert_that(is.string(file))
    writeLines(as.character(toJSON(cf,pretty=TRUE)),con=file)
}

#' @rdname bb_config
add <- function(cf,source) {
    ## need to do this by using global parms, if not overridden by source-specific values
    cf$sources <- dplyr::bind_rows(cf$sources,source)
    cf
}

