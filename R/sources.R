#' Configuration information for external data sources
#'
#' @param id string: a short, unique identifier for the data source
#' @param name string:
#' @param description string:
#' @param reference string:
#' @param source_urls character vector:
#' @param citation string:
#' @param license string:
#' @param comment string:
#' @param method string:
#' @param method_flags string:
#' @param postprocess string or function:
#' @param access_function string:
#' @param data_group string:
#'
#' @return tibble
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config() %>%
#'     add(bb_sources() %>% dplyr::filter(id=="seaice_smmr_ssmi_nasateam"))
#' }
#'
#' @export
bb_source <- function(id,name,description,reference,source_urls,citation,license,comment,method,method_flags,postprocess,access_function,data_group) {
}

#' Bowerbird configurations for various data sources
#'
#' @param data_group character vector: only return data sources belonging to these data groups
#'
#' @references See \code{reference} and \code{citation} field in each row of the returned tibble
#'
#' @return tibble
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#' bb_sources()
#'
#' @export
bb_sources <- function(data_group) {
    out <- tibble(
        id="seaice_smmr_ssmi_nasateam",
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
        data_group="Sea ice") %>%
        bind_rows(
            tibble(
                id="seaice_smmr_ssmi_nasateam_nrt",
                name="NSIDC SMMR-SSM/I Nasateam near-real-time sea ice concentration",
                description="Passive-microwave estimates of sea ice concentration at 25km, daily, near-real-time resolution.",
                reference="http://nsidc.org/data/nsidc-0081.html",
                citation="Maslanik, J. and J. Stroeve. 1999, updated daily. Near-Real-Time DMSP SSMIS Daily Polar Gridded Sea Ice Concentrations. [indicate subset used]. Boulder, Colorado USA: NASA National Snow and Ice Data Center Distributed Active Archive Center. http://dx.doi.org/10.5067/U8C09DWVX9LM",
                source_urls="ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/",
                license="Please cite, see http://nsidc.org/about/use_copyright.html",
                comment="",
                method="wget",
                method_flags="--exclude-directories=pub/DATASETS/nsidc0081_nrt_nasateam_seaice/browse,pub/DATASETS/nsidc0081_nrt_nasateam_seaice/north --recursive --level=inf",
                postprocess="",
                access_function="readice",
                data_group="Sea ice")
            ) %>%
        bind_rows(
            tibble(
                id="seaice_nsidc_supporting",
                name="NSIDC passive microwave supporting files",
                description="Grids and other support files for NSIDC passive-microwave sea ice data.",
                reference="http://nsidc.org/data/nsidc-0051.html",
                citation="See the citation details of the particular sea ice dataset used",
                source_urls="ftp://sidads.colorado.edu/pub/DATASETS/seaice/polar-stereo/",
                license="Please cite, see http://nsidc.org/about/use_copyright.html",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf",
                postprocess="",
                access_function="readice",
                data_group="Sea ice")
            )
    if (!missing(data_group)) out <- out[out$data_group %in% data_group,]
    out
}
