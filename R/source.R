#' Define a data source
#'
#' This function is used to define a data source, which can then be added to a data repository configuration. Passing the configuration object to \code{bb_sync} will trigger a download of all of the data sources in that configuration.
#' The \code{method} parameter defines the function used to synchronize this data source, and any extra parameters that need to be passed to it via its \code{...} argument. Note that \code{bb_sync} automatically passes the \code{config}, \code{verbose}, and \code{local_dir_only} parameters to the method handler, and so do not need to be listed as part of the extra parameters provided in the \code{method} argument here.
#'
#' @param id string: (required) a unique identifier of the data source. If the data source has a DOI, use that. Otherwise, if the original data provider has an identifier for this dataset, that is probably a good choice here (include the data version number if there is one). The ID should be something that changes when the data set changes (is updated). A DOI is ideal for this
#' @param name string: (required) a unique name for the data source. This should be a human-readable but still concise name
#' @param description string: a description of the data source
#' @param doc_url string: (required) URL to the metadata record or other documentation of the data source
#' @param source_url character vector: one or more source URLs. Required for \code{bb_handler_wget}, although some \code{method} functions might not require one
#' @param citation string: (required) details of the citation for the data source
#' @param license string: (required) description of the license. For standard licenses (e.g. creative commons) include the license descriptor ("CC-BY", etc)
#' @param comment string: comments about the data source. If only part of the original data collection is mirrored, mention that here
#' @param method list (required): a list object that defines the function used to synchronise this data source. The first element of the list is the function name (as a string or function). Additional list elements can be used to specify additional parameters to pass to that function. Note that \code{bb_sync} automatically passes the data repository configuration object as the first parameter to the method handler function. If the handler function uses wget (e.g. \code{bb_handler_wget}), these extra parameters are passed through to the \code{bb_wget} function
#' @param postprocess list: each element of \code{postprocess} defines a postprocessing step to be run after the main synchronization has happened. Each element of this list can be a function or string function name, or a list in the style of \code{list(fun,arg1=val1,arg2=val2)} where \code{fun} is the function to be called and \code{arg1} and \code{arg2} are additional parameters to pass to that function
#' @param authentication_note string: if authentication is required in order to access this data source, make a note of the process (include a URL to the registration page, if possible)
#' @param user string: username, if required
#' @param password string: password, if required
#' @param access_function string: name of the R function that can be used to read these data
#' @param data_group string: the name of the group to which this data source belongs. Useful for arranging sources in terms of thematic areas
#' @param collection_size numeric: approximate disk space (in GB) used by the data collection, if known. If the data are supplied as compressed files, this size should reflect the disk space used after decompression. If the data_source definition contains multiple source_url entries, this size should reflect the overall disk space used by all combined
#' @param warn_empty_auth logical: if TRUE, issue a warning if the data source requires authentication (authentication_note is not NA) but user and password have not been provided
#'
#' @return tibble
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#'
#' my_source <- bb_source(
#'    id="gshhg_coastline",
#'    name="GSHHG coastline data",
#'    description="A Global Self-consistent, Hierarchical, High-resolution Geography Database",
#'    doc_url= "http://www.soest.hawaii.edu/pwessel/gshhg",
#'    citation="Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
#'      High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
#'    source_url="ftp://ftp.soest.hawaii.edu/gshhg/*",
#'    license="LGPL",
#'    method=list("bb_handler_wget",recursive=TRUE,level=1,accept="*bin*.zip,README.TXT"),
#'    postprocess=list("bb_unzip"),
#'    collection_size=0.6)
#'
#' ## define a data repository configuration
#' cf <- bb_config("/my/repo/root")
#'
#' ## add this source to the repository
#' cf <- bb_add(cf,my_source)
#'
#' \dontrun{
#' ## sync the repo
#' bb_sync(cf)
#' }
#'
#' @export
bb_source <- function(id,name,description=NA_character_,doc_url,source_url,citation,license,comment=NA_character_,method,postprocess,authentication_note=NA_character_,user=NA_character_,password=NA_character_,access_function=NA_character_,data_group=NA_character_,collection_size=NA,warn_empty_auth=TRUE) {
    assert_that(is.list(method))
    if (!is_a_fun(method[[1]])) stop("the method parameter should be a list, with the first element being the handler function (a function or something that resolves to a function via match.fun)")
    if (missing(id) || !is_nonempty_string(id))
        stop("Each data source requires a non-empty id")
    if (missing(name) || !is_nonempty_string(name))
        stop("Each data source requires a non-empty name")
    if (warn_empty_auth && (!is.na(authentication_note) && (na_or_empty(user) || na_or_empty(password)))) {
        warning(sprintf("The data source \"%s\" requires authentication, but the user and/or password fields have not been set.\nThe authentication_note for this data source is:\n %s",name,authentication_note))
    }
    if (missing(license) || missing(citation))
        stop("Please provide license and citation information for the data source, so that users properly acknowledge it")
    if (missing(doc_url))
        stop("Please provide a doc_url (a URL to the data source's metadata record or home page")

    if (missing(source_url)) source_url <- NA_character_
    source_url <- source_url[nzchar(source_url) & !is.na(source_url)] ## drop empty and NA strings
    if (length(source_url)<1) {
        if (check_method_is(method[[1]],bb_handler_wget)) stop("method 'bb_handler_wget' requires at least one non-empty source URL")
        source_url <- NA_character_
    }
    if (missing(postprocess) || is.null(postprocess)) {
        postprocess <- list()
    } else {
        ## each element of the list should be a list, where the first element should resolve to a function and the rest are arguments. But we'll also accept a list element being a function, in which case we'll treat it as not needing extra args
        assert_that(is.list(postprocess))
        for (k in seq_len(length(postprocess))) {
            if (is_a_fun(postprocess[[k]])) {
                ## change this to a list with the function as its first (only) element
                postprocess[[k]] <- list(postprocess[[k]])
            } else if (is.list(postprocess[[k]]) && is_a_fun(postprocess[[k]][[1]])) {
                ## ok, leave as is
            } else {
                stop("postprocess entry ",k," is not a function or a list with a function as its first element")
            }
        }
    }
    assert_that(is.character(source_url))
    tibble(
        id=id,
        name=name,
        description=if (assert_that(is.string(description))) description,
        doc_url=if (assert_that(is.string(doc_url))) doc_url,
        source_url=list(source_url),
        citation=if (assert_that(is.string(citation))) citation,
        license=if (assert_that(is.string(license))) license,
        comment=if (assert_that(is.string(comment))) comment,
        method=list(method),
        postprocess=list(postprocess),
        authentication_note=if (assert_that(is.string(authentication_note))) authentication_note,
        user=if (assert_that(is.string(user))) user,
        password=if (assert_that(is.string(password))) password,
        access_function=if (assert_that(is.string(access_function))) access_function,
        data_group=if (assert_that(is.string(data_group))) data_group,
        collection_size=if (assert_that(is.numeric(collection_size) || is.na(collection_size))) collection_size)
}
