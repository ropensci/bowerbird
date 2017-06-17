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
                 http_proxy:"",
                 ftp_proxy:"",
                 local_file_root:"",
                 clobber:1,
                 skip_downloads:false,
                 wait:0
             ),
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


#' Configuration information for external data sources
#'
#' @param ids character vector: one or more identifiers of pre-packaged source configurations
#'
#' @return tibble
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config() %>%
#'     add(bb_source("seaice_smmr_ssmi_nasateam"))
#' }
#'
#' @export
bb_source <- function(ids=c("seaice_smmr_ssmi_nasateam")) {
    if (!missing(ids)) {
        assert_that(is.character(ids))
        do.call(bind_rows,lapply(ids,bb_source_defs))
    }
}


bb_source_defs <- function(id) {
    switch(id,
           seaice_smmr_ssmi_nasateam=tibble(
               name="NSIDC SMMR-SSM/I Nasateam sea ice concentration",
               description="Passive-microwave estimates of sea ice concentration at 25km spatial resolution. Daily and monthly resolution, available from 1-Oct-1978 to present.",
               reference="http://nsidc.org/data/nsidc-0051.html",
               source_urls="ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/",
               citation="Cavalieri, D. J., C. L. Parkinson, P. Gloersen, and H. Zwally. 1996, updated yearly. Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS Passive Microwave Data. [indicate subset used]. Boulder, Colorado USA: NASA National Snow and Ice Data Center Distributed Active Archive Center. http://dx.doi.org/10.5067/8GQ8LZQVL0VL",
               license="Please cite, see http://nsidc.org/about/use_copyright.html",
               comment="",
               method="wget",
               method_flags="--exclude-directories=pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/browse,pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north --recursive --level=inf",
               postprocess="",
               access_function="readice",
               data_group="Sea ice"),
           stop("unknown data source id: ",id)
           )
}
               
