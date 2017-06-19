#' Define an external data source
#'
#' @param name string: the name of the data source
#' @param description string: a description of the data source
#' @param reference string: URL to the metadata record or home page of the data source
#' @param source_urls character vector: one or more source URLs
#' @param citation string:
#' @param license string:
#' @param comment string:
#' @param method string:
#' @param method_flags string:
#' @param postprocess function, call, or list thereof: functions to apply after synchronisation has completed. If NULL or an empty list, no postprocessing will be applied
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
#'    postprocess=pp_unzip)
#'
#' cf <- bb_config("/my/repo/root")
#' cf <- add(cf,my_source)
#'
#' @export
bb_source <- function(name,description="",reference,source_urls,citation,license,comment="",method="wget",method_flags="",postprocess,access_function="",data_group="") {
    ## todo: allow method, postprocess to be passed as functions?
    if (missing(name))
        stop("Each data source requires a name")
    if (missing(license) || missing(citation))
        stop("Please provide license and citation information for the data source, so that users properly acknowledge it")
    if (missing(reference))
        stop("Please provide a reference (a URL to the data source's metadata record or home page")
    if (missing(source_urls)) {
        if (method=="wget") stop("method 'wget' requires at least one source URL")
        ##warning("no source_urls provided")
        source_urls <- as.character(NA)
    }
    if (is.character(source_urls)) source_urls <- list(source_urls)
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
        source_urls=if (assert_that(is.list(source_urls))) source_urls,
        citation=if (assert_that(is.string(citation))) citation,
        license=if (assert_that(is.string(license))) license,
        comment=if (assert_that(is.string(comment))) comment,
        method=method,
        method_flags=if (assert_that(is.string(method_flags))) method_flags,
        postprocess=list(postprocess),
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
        if (missing(data_group) || (!missing(data_group) && "Topography" %in% data_group)) sources_topography()
        )
    if (!missing(name)) out <- out[out$name %in% name,]
    out
}
