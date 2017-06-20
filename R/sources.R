#' Define an external data source
#'
#' @param name string: the name of the data source
#' @param description string: a description of the data source
#' @param reference string: URL to the metadata record or home page of the data source
#' @param source_url character vector: one or more source URLs
#' @param citation string:
#' @param license string:
#' @param comment string:
#' @param method function: the function that handles the synchronisation process for this data source
#' @param method_flags string:
#' @param postprocess function, call, or list thereof: functions to apply after synchronisation has completed. If NULL or an empty list, no postprocessing will be applied
#' @param user string: username, if required
#' @param password string: password, if required
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
#'    source_url="ftp://ftp.soest.hawaii.edu/gshhg/*",
#'    license="",
#'    comment="",
#'    method=bb_wget,
#'    method_flags="--recursive --level=1 --accept=\"*bin*.zip,README.TXT\"",
#'    postprocess=pp_unzip)
#'
#' cf <- bb_config("/my/repo/root")
#' cf <- add(cf,my_source)
#'
#' @export
bb_source <- function(name,description=as.character(NA),reference,source_url,citation,license,comment=as.character(NA),method=bb_wget,method_flags=as.character(NA),postprocess,user=as.character(NA),password=as.character(NA),access_function=as.character(NA),data_group=as.character(NA)) {
    assert_that(is.function(method))
    if (missing(name))
        stop("Each data source requires a name")
    if (missing(license) || missing(citation))
        stop("Please provide license and citation information for the data source, so that users properly acknowledge it")
    if (missing(reference))
        stop("Please provide a reference (a URL to the data source's metadata record or home page")
    if (missing(source_url)) {
        if (identical(method,bb_wget)) stop("method 'bb_wget' requires at least one source URL")
        ##warning("no source_url provided")
        source_url <- as.character(NA)
    }
    if (missing(postprocess) || is.null(postprocess)) {
        postprocess <- list()
    } else {
        if (is.function(postprocess) || inherits(postprocess,"call")) postprocess <- list(postprocess)
        ppchk <- is.list(postprocess) && all(sapply(postprocess,function(z)is.function(z) || inherits(z,"call")))
        if (!ppchk) stop("the postprocess argument should be a list of functions or calls (unevaluated functions)")
    }
    tibble(
        name=if (assert_that(is.string(name))) name,
        description=if (assert_that(is.string(description))) description,
        reference=if (assert_that(is.string(reference))) reference,
        source_url=if (assert_that(is.character(source_url))) source_url,
        citation=if (assert_that(is.string(citation))) citation,
        license=if (assert_that(is.string(license))) license,
        comment=if (assert_that(is.string(comment))) comment,
        method=list(method),
        method_flags=if (assert_that(is.string(method_flags))) method_flags,
        postprocess=list(postprocess),
        user=if (assert_that(is.string(user))) user,
        password=if (assert_that(is.string(password))) password,
        access_function=if (assert_that(is.string(access_function))) access_function,
        data_group=if (assert_that(is.string(data_group))) data_group)
}



#' Bowerbird configurations for various data sources
#'
#' @param name character vector: only return data sources with name matching these values
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
bb_sources <- function(name,data_group) {
    if (!missing(name)) assert_that(is.character(name))
    if (!missing(data_group)) assert_that(is.character(data_group))
    out <- bind_rows(
        if (missing(data_group) || (!missing(data_group) && "Sea ice" %in% data_group)) sources_seaice(),
        if (missing(data_group) || (!missing(data_group) && "Topography" %in% data_group)) sources_topography(),
        if (missing(data_group) || (!missing(data_group) && "Sea surface temperature" %in% data_group)) sources_sst(),
        if (missing(data_group) || (!missing(data_group) && "Altimetry" %in% data_group)) sources_altimetry(),
        if (missing(data_group) || (!missing(data_group) && "Oceanographic" %in% data_group)) sources_oceanographic(),
        if (missing(data_group) || (!missing(data_group) && "Reanalysis" %in% data_group)) sources_reanalysis()
        )
    if (!missing(name)) out <- out[out$name %in% name,]
    out
}
