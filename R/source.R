#' Define a data source
#'
#' @param id string: (required) a unique identifier of the data source. If the data source has a DOI, use that. Otherwise, if the original data provider has an identifier for this dataset, that is probably a good choice here (include the data version number if there is one). The ID should be something that changes when the data set changes (is updated). A DOI is ideal for this
#' @param name string: (required) a unique name for the data source. This should be a human-readable but still concise name
#' @param description string: a description of the data source
#' @param reference string: (required) URL to the metadata record or home page of the data source
#' @param source_url character vector: one or more source URLs. Generally required, although some \code{method} functions might not require one
#' @param citation string: (required) details of the citation for the data source
#' @param license string: (required) description of the license. For standard licenses (e.g. creative commons) include the license descriptor ("CC-BY", etc)
#' @param comment string: comments about the data source. If only part of the original data collection is mirrored, mention that here
#' @param method function, call, or symbol: (required) the function that handles the synchronisation process for this data source
#' @param method_flags character vector: flags to pass to the method. If method is \code{bb_wget}, these are wget flags. Run \code{wget("--help")} to get help on these flags
#' @param postprocess function, call, symbol, or list thereof: functions to apply after synchronisation has completed. If NULL or an empty list, no postprocessing will be applied
#' @param authentication_note string: if authentication is required in order to access this data source, make a note of the process (include a URL to the registration page, if possible)
#' @param user string: username, if required
#' @param password string: password, if required
#' @param access_function string: name of the R function that can be used to read these data
#' @param data_group string: the name of the group to which this data source belongs. Useful for arranging sources in terms of thematic areas
#' @param collection_size numeric: approximate disk space (in GB) used by the data collection, if known. If the data are supplied as compressed files, this size should reflect the disk space used after decompression. If the data_source definition contains multiple source_url entries, this size should reflect the overall disk space used by all combined
#' @param warn_empty_auth logical: if TRUE, issue a warning if the data source requires authentication (authentication_note is not NA) but user and password have not been provided
#'
#' @return data.frame
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#'
#' my_source <- bb_source(
#'    id="gshhg_coastline",
#'    name="GSHHG coastline data",
#'    description="A Global Self-consistent, Hierarchical, High-resolution Geography Database",
#'    reference= "http://www.soest.hawaii.edu/pwessel/gshhg",
#'    citation="Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
#'      High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
#'    source_url="ftp://ftp.soest.hawaii.edu/gshhg/*",
#'    license="",
#'    comment="",
#'    method=quote(bb_wget),
#'    method_flags=c("--recursive","--level=1","--accept=*bin*.zip,README.TXT"),
#'    postprocess=quote(bb_unzip),
#'    collection_size=0.6)
#'
#' cf <- bb_config("/my/repo/root")
#' cf <- bb_add(cf,my_source)
#'
#' @export
bb_source <- function(id,name,description=NA_character_,reference,source_url,citation,license,comment=NA_character_,method=bb_wget,method_flags=character(),postprocess,authentication_note=NA_character_,user=NA_character_,password=NA_character_,access_function=NA_character_,data_group=NA_character_,collection_size=NA,warn_empty_auth=TRUE) {
    assert_that(is.function(method) || (is.symbol(method) && exists(deparse(method),mode="function")) || is.call(method))
    if (missing(id) || !is_nonempty_string(id))
        stop("Each data source requires a non-empty id")
    if (missing(name) || !is_nonempty_string(name))
        stop("Each data source requires a non-empty name")
    if (warn_empty_auth && (!is.na(authentication_note) && (na_or_empty(user) || na_or_empty(password)))) {
        warning(sprintf("The data source \"%s\" requires authentication, but the user and/or password fields have not been set.\nThe authentication_note for this data source is:\n %s",name,authentication_note))
    }
    if (missing(license) || missing(citation))
        stop("Please provide license and citation information for the data source, so that users properly acknowledge it")
    if (missing(reference))
        stop("Please provide a reference (a URL to the data source's metadata record or home page")
    if (missing(source_url)) {
        if (check_method_is(method,bb_wget)) stop("method 'bb_wget' requires at least one source URL")
        ##warning("no source_url provided")
        source_url <- NA_character_
    }
    if (missing(postprocess) || is.null(postprocess)) {
        postprocess <- list()
    } else {
        if (is.function(postprocess) || is.call(postprocess) || is.symbol(postprocess)) postprocess <- list(postprocess)
        ppchk <- is.list(postprocess) && all(vapply(postprocess,function(z)is.function(z) || is.call(z) || (is.symbol(z) && exists(deparse(z),mode="function")),FUN.VALUE=TRUE))
        if (!ppchk) stop("the postprocess argument should be a list of functions or calls (unevaluated functions)")
    }
    assert_that(is.character(source_url))
    if (is.null(method_flags) || (!is.character(method_flags) && length(method_flags)<1) || (is.character(method_flags) && length(method_flags)==1 && !nzchar(method_flags)))
        method_flags <- character()
    tibble(
        id=id,
        name=name,
        description=if (assert_that(is.string(description))) description,
        reference=if (assert_that(is.string(reference))) reference,
        source_url=source_url,
        citation=if (assert_that(is.string(citation))) citation,
        license=if (assert_that(is.string(license))) license,
        comment=if (assert_that(is.string(comment))) comment,
        method=list(method),
        method_flags=if (assert_that(is.character(method_flags))) list(str_trim(method_flags)),
        postprocess=list(postprocess),
        authentication_note=if (assert_that(is.string(authentication_note))) authentication_note,
        user=if (assert_that(is.string(user))) user,
        password=if (assert_that(is.string(password))) password,
        access_function=if (assert_that(is.string(access_function))) access_function,
        data_group=if (assert_that(is.string(data_group))) data_group,
        collection_size=if (assert_that(is.numeric(collection_size) || is.na(collection_size))) collection_size)
}

