#' Define a data source
#'
#' This function is used to define a data source, which can then be added to a bowerbird data repository configuration. Passing the configuration object to \code{bb_sync} will trigger a download of all of the data sources in that configuration.
#'
#' The \code{method} parameter defines the handler function used to synchronize this data source, and any extra parameters that need to be passed to it.
#'
#' Parameters marked as "required" are the minimal set needed to define a data source. Other parameters are either not relevant to all data sources (e.g. \code{postprocess}, \code{user}, \code{password}) or provide metadata to users that is not strictly necessary to allow the data source to be synchronized (e.g. \code{description}, \code{access_function}, \code{data_group}). Note that three of the "required" parameters (namely \code{citation}, \code{license}, and \code{doc_url}) are not strictly needed by the synchronization code, but are treated as "required" because of their fundamental importance to reproducible science.
#'
#' See \code{vignette("bowerbird")} for more examples and discussion of defining data sources.
#'
#' @param id string: (required) a unique identifier of the data source. If the data source has a DOI, use that. Otherwise, if the original data provider has an identifier for this dataset, that is probably a good choice here (include the data version number if there is one). The ID should be something that changes when the data set changes (is updated). A DOI is ideal for this
#' @param name string: (required) a unique name for the data source. This should be a human-readable but still concise name
#' @param description string: a plain-language description of the data source, provided so that users can get an idea of what the data source contains (for full details they can consult the \code{doc_url} link)
#' @param doc_url string: (required) URL to the metadata record or other documentation of the data source
#' @param source_url character vector: one or more source URLs. Required for \code{bb_handler_rget}, although some \code{method} functions might not require one
#' @param citation string: (required) details of the citation for the data source
#' @param license string: (required) description of the license. For standard licenses (e.g. creative commons) include the license descriptor ("CC-BY", etc)
#' @param comment string: comments about the data source. If only part of the original data collection is mirrored, mention that here
#' @param method list (required): a list object that defines the function used to synchronize this data source. The first element of the list is the function name (as a string or function). Additional list elements can be used to specify additional parameters to pass to that function. Note that \code{bb_sync} automatically passes the data repository configuration object as the first parameter to the method handler function. If the handler function uses bb_rget (e.g. \code{bb_handler_rget}), these extra parameters are passed through to the \code{bb_rget} function
#' @param postprocess list: each element of \code{postprocess} defines a postprocessing step to be run after the main synchronization has happened. Each element of this list can be a function or string function name, or a list in the style of \code{list(fun,arg1=val1,arg2=val2)} where \code{fun} is the function to be called and \code{arg1} and \code{arg2} are additional parameters to pass to that function
#' @param authentication_note string: if authentication is required in order to access this data source, make a note of the process (include a URL to the registration page, if possible)
#' @param user string: username, if required
#' @param password string: password, if required
#' @param access_function string: can be used to suggest to users an appropriate function to read these data files. Provide the name of an R function or even a code snippet
#' @param data_group string: the name of the group to which this data source belongs. Useful for arranging sources in terms of thematic areas
#' @param collection_size numeric: approximate disk space (in GB) used by the data collection, if known. If the data are supplied as compressed files, this size should reflect the disk space used after decompression. If the data_source definition contains multiple source_url entries, this size should reflect the overall disk space used by all combined
#' @param warn_empty_auth logical: if \code{TRUE}, issue a warning if the data source requires authentication (authentication_note is not NA) but user and password have not been provided. Set this to \code{FALSE} if you are defining a data source for others to use with their own credentials: they will typically call your data source constructor and then modify the \code{user} and \code{password} components
#'
#' @return a tibble with columns as per the function arguments (excluding \code{warn_empty_auth})
#'
#' @seealso \code{\link{bb_config}}, \code{\link{bb_sync}}, \code{vignette("bowerbird")}
#'
#' @examples
#'
#' ## a minimal definition for the GSHHG coastline data set:
#'
#' my_source <- bb_source(
#'    id = "gshhg_coastline",
#'    name = "GSHHG coastline data",
#'    doc_url = "http://www.soest.hawaii.edu/pwessel/gshhg",
#'    citation = "Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
#'      High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
#'    source_url = "ftp://ftp.soest.hawaii.edu/gshhg/",
#'    license = "LGPL",
#'    method = list("bb_handler_rget",level = 1, accept_download = "README|bin.*\\.zip$"))
#'
#' ## a more complete definition, which unzips the files after downloading and also
#' ##  provides an indication of the size of the dataset
#'
#' my_source <- bb_source(
#'    id = "gshhg_coastline",
#'    name = "GSHHG coastline data",
#'    description = "A Global Self-consistent, Hierarchical, High-resolution Geography Database",
#'    doc_url = "http://www.soest.hawaii.edu/pwessel/gshhg",
#'    citation = "Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
#'      High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
#'    source_url = "ftp://ftp.soest.hawaii.edu/gshhg/*",
#'    license = "LGPL",
#'    method = list("bb_handler_rget", level = 1, accept_download = "README|bin.*\\.zip$"),
#'    postprocess = list("bb_unzip"),
#'    collection_size = 0.6)
#'
#' ## define a data repository configuration
#' cf <- bb_config("/my/repo/root")
#'
#' ## add this source to the repository
#' cf <- bb_add(cf, my_source)
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
    if (missing(license) || is.null(license) || missing(citation) || is.null(citation))
        stop("Please provide license and citation information for the data source, so that users properly acknowledge it")
    if (missing(doc_url) || is.null(doc_url))
        stop("Please provide a doc_url (a URL to the data source's metadata record or home page")

    if (missing(source_url) || is.null(source_url)) source_url <- NA_character_
    source_url <- source_url[nzchar(source_url) & !is.na(source_url)] ## drop empty and NA strings
    if (length(source_url)<1) {
        if (check_method_is(method[[1]],bb_handler_wget)) stop("method 'bb_handler_wget' requires at least one non-empty source URL")
        if (check_method_is(method[[1]],bb_handler_rget)) stop("method 'bb_handler_rget' requires at least one non-empty source URL")
        source_url <- NA_character_
    }
    if (missing(postprocess) || is.null(postprocess)) {
        postprocess <- list()
    } else {
        if ("bucket" %in% names(method$s3_args)) {
            if (length(postprocess) > 0) warning("postprocessing not supported for s3 targets, ignoring\n")
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


#' Modify a data source
#'
#' This is a helper function designed to make it easier to modify an already-defined data source. Generally, parameters passed here will replace existing entries in \code{src} if they exist, or will be added if not. The \code{method} and \code{postprocess} parameters are slightly different: see Details, below.
#'
#' With the exception of the \code{method} and \code{postprocess} parameters, any parameter provided here will entirely replace its equivalent in the \code{src} object. Pass a new value of \code{NULL} to remove an existing parameter.
#'
#' The \code{method} and \code{postprocess} parameters are lists, and modification for these takes place at the list-element level: any element of the new list will replace its equivalent element in the list in src. If the src list does not contain that element, it will be added. To illustrate, say that we have created a data source with:
#'
#' \code{src <- bb_source(method=list("bb_handler_rget", parm1 = value1, parm2 = value2), ...)}
#'
#' Calling
#'
#' \code{bb_modify_source(src, method = list(parm1 = newvalue1))}
#'
#' will result in a new \code{method} value of \code{list("bb_handler_rget", parm1 = newvalue1, parm2 = value2)}
#'
#' Modifying \code{postprocess} elements is similar. Note that it is not currently possible to entirely remove a postprocess component using this function. If you need to do so, you'll need to do it manually.
#'
#' @param src data.frame or tibble: a single-row data source (as returned by \code{bb_source})
#' @param ... : parameters as for \code{bb_source}
#'
#' @return as for \code{bb_source}: a tibble with columns as per the \code{bb_source} function arguments (excluding \code{warn_empty_auth})
#'
#' @seealso \code{\link{bb_source}}
#'
#' @examples
#'
#' ## this pre-defined source requires a username and password
#' src <- bb_example_sources(
#'           "Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 3")
#'
#' ## add username and password
#' src <- bb_modify_source(src,user="myusername",password="mypassword")
#'
#' ## or using the pipe operator
#' src <- bb_example_sources(
#'           "Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 3") %>%
#'   bb_modify_source(user="myusername",password="mypassword")
#'
#' ## remove the existing "data_group" component
#' src %>% bb_modify_source(data_group=NULL)
#'
#' ## change just the 'level' setting of an existing method definition
#' src %>% bb_modify_source(method=list(level=3))
#'
#' ## remove the 'level' component of an existing method definition
#' src %>% bb_modify_source(method=list(level=NULL))
#'
#' @export
bb_modify_source <- function(src,...) {
    parms <- list(...)
    pnames <- names(parms)
    ## check that the parameters passed here are expected
    chk <- setdiff(pnames,c("id","name","description","doc_url","source_url","citation","license","comment","method","postprocess","authentication_note","user","password","access_function","data_group","collection_size","warn_empty_auth"))
    if (length(chk)>0) stop("unexpected input parameters: ",paste(chk,collapse=", "),". See help('bb_source') for allowed parameters")
    ## construct list of new parameters to pass to bb_source
    ## do this so that checks in bb_source get applied to the new parameter values
    ## simple parameters (strings, etc) are taken from parms if provided, otherwise from src
    new_or_old <- function(prm) if (prm %in% pnames) parms[[prm]] else src[[prm]]
    newparms <- list()
    for (p in c("id","name","description","doc_url","citation","license","comment","authentication_note","user","password","access_function","data_group","collection_size"))
        newparms[[p]] <- new_or_old(p)
    ## source_url is a character vector, but stored in src as a list containing that vector
    newparms$source_url <- if ("source_url" %in% pnames) parms$source_url else src$source_url[[1]]
    ## method is a list-col in src, of the form list("handler_function_name",parm1=value1)
    if ("method" %in% pnames) {
        assert_that(is.list(parms$method))
        newmeth <- src$method[[1]] ## start from this
        modmeth <- parms$method ## modify using this
        nmm <- names(modmeth)
        if (is.null(nmm)) {
            ## none of the elements of modmeth are named
            names(modmeth) <- rep("",length(modmeth))
        }
        not_named <- !nzchar(names(modmeth))
        ## expect at most one non-named element: this is the handler function name
        if (sum(not_named)==1) {
            newmeth[[1]] <- modmeth[[which(not_named)]]
        } else if (sum(not_named)>1) {
            stop("not expecting multiple un-named elements in method parameter")
        }
        modmeth <- modmeth[which(!not_named)]
        for (k in names(modmeth)) {
            newmeth[[k]] <- modmeth[[k]]
        }
        newparms$method <- newmeth
    } else {
        newparms$method <- src$method[[1]]
    }
    ## src$postprocess is also a list
    ## each element is a list of the form list("function_name",parm1=value1) [parm1 etc are optional, provided if that function needs parameters passed]
    ## parms$postprocess is a list BUT elements can be just "function_name", or list("function_name",parm1=value1)
    if ("postprocess" %in% pnames) {
        assert_that(is.list(parms$postprocess))
        newpp <- src$postprocess[[1]] ## start with this
        newpp_funs <- vapply(newpp,function(z)z[[1]],FUN.VALUE="",USE.NAMES=FALSE)
        modpp <- parms$postprocess
        for (ppi in seq_len(length(modpp))) {
            if (is.string(modpp[[ppi]]) && length(modpp[[ppi]])==1) modpp[[ppi]] <- list(modpp[[ppi]]) ## change "function_name" element to list("function_name")
            thisfun <- modpp[[ppi]][[1]]
            if (any(thisfun %in% newpp_funs)) {
                ## this function already exists in newpp
                ## so replace the existing list element
                newpp[[which(thisfun==newpp_funs)]] <- modpp[[ppi]]
            } else {
                newpp <- c(newpp,modpp[ppi])
            }
        }
        newparms$postprocess <- newpp
    } else {
        newparms$postprocess <- src$postprocess[[1]]
    }
    if ("warn_empty_auth" %in% pnames) newparms$warn_empty_auth <- parms$warn_empty_auth
    do.call(bb_source,newparms)
}
