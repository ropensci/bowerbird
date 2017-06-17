#' Define an external data source
#'
#' @param id string: a short, unique identifier for the data source
#' @param name string: a (longer) name for the data source
#' @param description string: a description of the data source
#' @param reference string: URL to the metadata record or home page of the data source
#' @param source_urls character vector: the source URL
#' @param citation string:
#' @param license string:
#' @param comment string:
#' @param method string:
#' @param method_flags string:
#' @param postprocess string:
#' @param access_function string:
#' @param data_group string:
#'
#' @return tibble
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#'
#' my_source <- bb_source(
#'    name="GSHHG coastline data",
#'    description="A Global Self-consistent, Hierarchical, High-resolution Geography Database",
#'    reference= "http://www.soest.hawaii.edu/pwessel/gshhg",
#'    citation="Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
#'      High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
#'    source_urls="ftp://ftp.soest.hawaii.edu/gshhg/*",
#'    license="",
#'    comment="",
#'    method="wget",
#'    method_flags="--recursive --level=1 --accept=\"*bin*.zip,README.TXT\"",
#'    postprocess="unzip")
#'
#' cf <- bb_config()
#' cf <- add(cf,my_source)
#'
#' @export
bb_source <- function(id,name,description="",reference,source_urls,citation,license,comment="",method="wget",method_flags="",postprocess="",access_function="",data_group="") {
    ## todo: allow method, postprocess to be passed as functions?
    if (missing(license) || missing(citation)) 
        stop("Please provide license and citation information for the data source, so that users properly acknowledge it")
    if (missing(reference))
        stop("Please provide a reference (a URL to the data source's metadata record or home page")
    if (missing(source_urls)) {
        if (method=="wget") stop("method 'wget' requires at least one source URL")
        ##warning("no source_urls provided")
        source_urls <- as.character(NA)
    }
    if (missing(name) && missing(id)) stop("A data source requires either a name or id")
    if (missing(id)) id <- ""
    if (missing(name)) name <- ""
    tibble(
        id=if (assert_that(is.string(id))) id,
        name=if (assert_that(is.string(name))) name,
        description=if (assert_that(is.string(description))) description,
        reference=if (assert_that(is.string(reference))) reference,
        source_urls=if (assert_that(is.character(source_urls))) source_urls,
        citation=if (assert_that(is.string(citation))) citation,
        license=if (assert_that(is.string(license))) license,
        comment=if (assert_that(is.string(comment))) comment,
        method=method,
        method_flags=if (assert_that(is.string(method_flags))) method_flags,
        postprocess=if (assert_that(is.string(postprocess))) postprocess,
        access_function=if (assert_that(is.string(access_function))) access_function,
        data_group=if (assert_that(is.string(data_group))) data_group)
}



#' Bowerbird configurations for various data sources
#'
#' @param id_or_name character vector: only return data sources with id or name matching these values
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
bb_sources <- function(id_or_name,data_group) {
    out <- bb_source(
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
            bb_source(
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
            bb_source(
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
    if (!missing(id_or_name)) out <- out[out$name %in% id_or_name | out$id %in% id_or_name,]
    if (!missing(data_group)) out <- out[out$data_group %in% data_group,]
    out
}
