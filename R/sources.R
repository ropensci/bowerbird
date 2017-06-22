#' Define an external data source
#' 
#' @param name string: the name of the data source
#' @param description string: a description of the data source
#' @param reference string: URL to the metadata record or home page of the data source
#' @param source_url character vector: one or more source URLs
#' @param citation string:
#' @param license string:
#' @param comment string:
#' @param method function, call, or symbol: the function that handles the synchronisation process for this data source
#' @param method_flags string:
#' @param postprocess function, call, symbol, or list thereof: functions to apply after synchronisation has completed. If NULL or an empty list, no postprocessing will be applied
#' @param authentication_note string: if authentication is required in order to access this data source, make a note of the process (include a URL to the registration page, if possible)
#' @param user string: username, if required
#' @param password string: password, if required
#' @param access_function string:
#' @param data_group string:
#' @param warn_empty_auth logical: if TRUE, issue a warning if the data source requires authentication (authentication_note is not NA) but user and password have not been provided
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
#'    method=quote(bb_wget),
#'    method_flags="--recursive --level=1 --accept=\"*bin*.zip,README.TXT\"",
#'    postprocess=quote(pp_unzip))
#'
#' cf <- bb_config("/my/repo/root")
#' cf <- add(cf,my_source)
#'
#' @export
bb_source <- function(name,description=NA_character_,reference,source_url,citation,license,comment=NA_character_,method=bb_wget,method_flags=NA_character_,postprocess,authentication_note=NA_character_,user=NA_character_,password=NA_character_,access_function=NA_character_,data_group=NA_character_,warn_empty_auth=TRUE) {
    assert_that(is.function(method) || (is.symbol(method) && exists(deparse(method),mode="function")) || is.call(method))
    if (missing(name))
        stop("Each data source requires a name")
    if (warn_empty_auth && (!is.na(authentication_note) && (na_or_empty(user) || na_or_empty(password)))) {
        warning(sprintf("The data source \"%s\" requires authentication, but the user and/or password fields have not been set.\nThe authentication_note for this data source is:\n %s",name,authentication_note))
    }
    if (missing(license) || missing(citation))
        stop("Please provide license and citation information for the data source, so that users properly acknowledge it")
    if (missing(reference))
        stop("Please provide a reference (a URL to the data source's metadata record or home page")
    if (missing(source_url)) {
        if (identical(method,bb_wget)) stop("method 'bb_wget' requires at least one source URL")
        ##warning("no source_url provided")
        source_url <- NA_character_
    }
    if (missing(postprocess) || is.null(postprocess)) {
        postprocess <- list()
    } else {
        if (is.function(postprocess) || is.call(postprocess) || is.symbol(postprocess)) postprocess <- list(postprocess)
        ppchk <- is.list(postprocess) && all(sapply(postprocess,function(z)is.function(z) || is.call(z) || (is.symbol(z) && exists(deparse(z),mode="function"))))
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
        authentication_note=if (assert_that(is.string(authentication_note))) authentication_note,
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
        if (missing(data_group) || (!missing(data_group) && "sea ice" %in% tolower(data_group))) sources_seaice(),
        if (missing(data_group) || (!missing(data_group) && "topography" %in% tolower(data_group))) sources_topography(),
        if (missing(data_group) || (!missing(data_group) && "sea surface temperature" %in% tolower(data_group))) sources_sst(),
        if (missing(data_group) || (!missing(data_group) && "altimetry" %in% tolower(data_group))) sources_altimetry(),
        if (missing(data_group) || (!missing(data_group) && "oceanographic" %in% tolower(data_group))) sources_oceanographic(),
        if (missing(data_group) || (!missing(data_group) && any(c("ocean colour","ocean color") %in% tolower(data_group)))) sources_ocean_colour(),
        if (missing(data_group) || (!missing(data_group) && "meteorological" %in% tolower(data_group))) sources_meteorological(),
        if (missing(data_group) || (!missing(data_group) && "reanalysis" %in% tolower(data_group))) sources_reanalysis(),
        if (missing(data_group) || (!missing(data_group) && "satellite imagery" %in% tolower(data_group))) sources_satellite_imagery()
        )
    if (!missing(name)) out <- out[tolower(out$name) %in% tolower(name),]
    out
}
