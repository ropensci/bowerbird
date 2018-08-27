#' Initialize a bowerbird configuration
#'
#' The configuration object controls the behaviour of the bowerbird synchronization process, run via \code{bb_sync(my_config)}. The configuration object defines the data sources that will be synchronized, where the data files from those sources will be stored, and a range of options controlling how the synchronization process is conducted. The parameters provided here are repository-wide settings, and will affect all data sources that are subsequently added to the configuration.
#'
#' Note that the \code{local_file_root} directory need not actually exist when the configuration object is created, but when \code{bb_sync} is run, either the directory must exist or \code{create_root=TRUE} must be passed (i.e. \code{bb_sync(...,create_root=TRUE)}).
#'
#' @param local_file_root string: location of data repository on local file system
#' @param wget_global_flags list: wget flags that will be applied to all data sources that call \code{bb_wget}. These will be appended to the data-source-specific wget flags provided via the source's method argument
#' @param http_proxy string: URL of HTTP proxy to use e.g. 'http://your.proxy:8080' (NULL for no proxy)
#' @param ftp_proxy string: URL of FTP proxy to use e.g. 'http://your.proxy:21' (NULL for no proxy)
#' @param clobber numeric: 0=do not overwrite existing files, 1=overwrite if the remote file is newer than the local copy, 2=always overwrite existing files
#'
#' @return configuration object
#'
#' @seealso \code{\link{bb_source}}, \code{\link{bb_sync}}
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     bb_add(bb_example_sources())
#'
#'   ## save to file
#'   saveRDS(cf,file="my_config.rds")
#'   ## load previously saved config
#'   cf <- readRDS(file="my_config.rds")
#' }
#'
#' @export
bb_config <- function(local_file_root,wget_global_flags=list(restrict_file_names="windows",progress="dot:giga"),http_proxy=NULL,ftp_proxy=NULL,clobber=1) {
    assert_that(is.string(local_file_root))
    assert_that(clobber %in% c(0,1,2))
    assert_that(is.list(wget_global_flags))
    structure(
        list(data_sources=tibble(),
             settings=list(
                 wget_global_flags=wget_global_flags,
                 http_proxy=http_proxy,
                 ftp_proxy=ftp_proxy,
                 local_file_root=normalizePath(local_file_root, mustWork = FALSE),
                 clobber=clobber)),
        class="bb_config")
}


#' Keep only selected data_sources in a bowerbird configuration
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config})
#' @param idx logical or numeric: index vector of data_source rows to retain
#'
#' @return configuration object
#'
#' @seealso \code{\link{bb_source}}, \code{\link{bb_config}}
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     bb_add(bb_example_sources()) %>%
#'     bb_subset(1:2)
#' }
#' @export
bb_subset <- function(config,idx) {
    assert_that(is(config,"bb_config"))
    temp <- bb_data_sources(config)[idx,]
    ## if we indexed past the end of this tibble, we'll have rows that are all NA or NULL - discard these
    temp <- temp[!apply(temp,1,function(z)all(vapply(z,function(w)is.null(w) || is.na(w),FUN.VALUE=TRUE))),]
    bb_data_sources(config) <- temp
    config
}

#' Add new data sources to a bowerbird configuration
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config})
#' @param source data.frame: one or more data source definitions, as returned by \code{bb_source}, to add to the configuration
#'
#' @return configuration object
#'
#' @seealso \code{\link{bb_source}}, \code{\link{bb_config}}
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     bb_add(bb_example_sources())
#' }
#' @export
bb_add <- function(config,source) {
    assert_that(is(config,"bb_config"))
    bb_data_sources(config) <- rbind(bb_data_sources(config),source)
    config
}


#' Gets or sets a bowerbird configuration object's settings
#'
#' Gets or sets a bowerbird configuration object's settings. These are repository-wide settings that are applied to all data sources added to the configuration. Use this function to alter the settings of a configuration previously created using \code{bb_config}.
#'
#' Note that an assignment along the lines of \code{bb_settings(cf) <- new_settings} replaces all of the settings in the configuration with the \code{new_settings}. The most common usage pattern is to read the existing settings, modify them as needed, and then rewrite the whole lot back into the configuration object (as per the examples here).
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config})
#' @param value list: new values to set
#'
#' @return named list
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#' cf <- bb_config(local_file_root="/your/data/directory")
#'
#' ## see current settings
#' bb_settings(cf)
#'
#' ## add an http proxy
#' sets <- bb_settings(cf)
#' sets$http_proxy <- "http://my.proxy"
#' bb_settings(cf) <- sets
#'
#' ## change the current local_file_root setting
#' sets <- bb_settings(cf)
#' sets$local_file_root <- "/new/location"
#' bb_settings(cf) <- sets
#'
#' @export
bb_settings <- function(config) {
    assert_that(is(config,"bb_config"))
    config$settings
}

#' @rdname bb_settings
#' @export
`bb_settings<-` <- function(config,value) {
    assert_that(is(config,"bb_config"))
    assert_that(is.list(value))
    temp <- setdiff(names(value),allowed_settings())
    if (length(temp)>0) {
        wstr <- if (length(temp)<2) " is not a recognized bowerbird config setting" else " are not recognized bowerbird config settings"
        warning(paste(temp,collapse=", "),wstr," and will be ignored")
        value <- value[names(value) %in% allowed_settings()]
    }
    config$settings <- value
    config
}

## internal: the list of recognized config settings
allowed_settings <- function() c("wget_global_flags","http_proxy","ftp_proxy","local_file_root","clobber","dry_run")


#' Gets or sets a bowerbird configuration object's data sources
#'
#' Gets or sets the data sources contained in a bowerbird configuration object.
#'
#' Note that an assignment along the lines of \code{bb_data_sources(cf) <- new_sources} replaces all of the sources in the configuration with the \code{new_sources}. If you wish to modify the existing sources then read them, modify as needed, and then rewrite the whole lot back into the configuration object.
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{\link{bb_config}})
#' @param value data.frame: new data sources to set (e.g. as returned by \code{\link{bb_example_sources}})
#'
#' @return a tibble with columns as specified by \code{\link{bb_source}}
#'
#' @seealso \code{\link{bb_config}}, \code{\link{bb_source}}, \code{\link{bb_example_sources}}
#'
#' @examples
#' ## create a configuration and add data sources
#' cf <- bb_config(local_file_root="/your/data/directory")
#' cf <- bb_add(cf,bb_example_sources())
#'
#' ## examine the sources contained in cf
#' bb_data_sources(cf)
#'
#' ## replace the sources with different ones
#' \dontrun{
#' bb_data_sources(cf) <- new_sources
#' }
#'
#' @export
bb_data_sources <- function(config) {
    assert_that(is(config,"bb_config"))
    config$data_sources
}

#' @rdname bb_data_sources
#' @export
`bb_data_sources<-` <- function(config,value) {
    assert_that(is(config,"bb_config"))
    config$data_sources <- value
    config
}

## internal helper function
## copy each bb setting into a column of data_sources table
## return only the augmented data_sources table
bb_settings_to_cols <- function(obj) {
    ds <- bb_data_sources(obj)
    st <- bb_settings(obj)
    ## flags handled as lists
    ds$wget_global_flags <- rep(list(st$wget_global_flags), nrow(ds))
    if (nrow(ds) > 0) {
        for (nm in setdiff(names(st), c("wget_global_flags"))) {
            thisatt <- st[[nm]]
            if (!is.null(thisatt))
                ds[, nm] <- thisatt
        }
    }
    ds
}


#' Return the local directory of each data source in a configuration
#'
#'  Return the local directory of each data source in a configuration. Files from each data source are stored locally in the associated directory.
#'
#' @param config bb_config: configuration as returned by \code{\link{bb_config}}
#'
#' @return character vector of directories
#'
#' @examples
#' cf <- bb_config("/my/file/root") %>%
#'   bb_add(bb_example_sources())
#' bb_data_source_dir(cf)
#'
#' @export
bb_data_source_dir <- function(config) {
    assert_that(is(config,"bb_config"))
    single_source_dir <- function(cfrow) {
        mth <- NULL
        try(mth <- match.fun(bb_data_sources(cfrow)$method[[1]][[1]]),silent=TRUE)
        if (is.function(mth)) {
            do.call(mth,c(list(config=cfrow,local_dir_only=TRUE),bb_data_sources(cfrow)$method[[1]][-1]))
        } else {
            as.character(NA)
        }
    }
    vapply(seq_len(nrow(bb_data_sources(config))),function(z)single_source_dir(bb_subset(config,z)),FUN.VALUE="")
}


#' Produce a summary of a bowerbird configuration
#'
#' This function produces a summary of a bowerbird configuation in HTML or Rmarkdown format. If you are maintaining a data collection on behalf of other users, or even just for yourself, it may be useful to keep an up-to-date HTML summary of your repository in an accessible location. Users can refer to this summary to see which data are in the repository and some details about them.
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config})
#' @param file string: path to file to write summary to. A temporary file is used by default
#' @param format string: produce HTML ("html") or Rmarkdown ("Rmd") file?
#' @param inc_license logical: include each source's license and citation details?
#' @param inc_auth logical: include information about authentication for each data source (if applicable)?
#' @param inc_size logical: include each source's size (disk space) information?
#' @param inc_access_function logical: include each source's access function?
#' @param inc_path logical: include each source's local file path?
#'
#' @return path to the summary file in HTML or Rmarkdown format
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     bb_add(bb_example_sources())
#'   browseURL(bb_summary(cf))
#' }
#'
#' @export
bb_summary <- function(config,file=tempfile(fileext=".html"),format="html",inc_license=TRUE,inc_auth=TRUE,inc_size=TRUE,inc_access_function=TRUE,inc_path=TRUE) {
    assert_that(is(config,"bb_config"))
    assert_that(is.string(file))
    assert_that(is.string(format))
    assert_that(is.flag(inc_license),!is.na(inc_license))
    assert_that(is.flag(inc_auth),!is.na(inc_auth))
    assert_that(is.flag(inc_size),!is.na(inc_size))
    assert_that(is.flag(inc_access_function),!is.na(inc_access_function))
    assert_that(is.flag(inc_path),!is.na(inc_path))
    format <- match.arg(tolower(format),c("html","rmd"))

    ## write summary as temporary Rmd file
    rmd_file <- tempfile(fileext=".Rmd")
    cat("---\ntitle: \"Summary of bowerbird repository\"\ndate: \"",date(),"\"\noutput:\n  html_document:\n    toc: true\n    theme: cerulean\n    highlight: default\n---\n\n",file=rmd_file,append=FALSE)

    cat("Summary of bowerbird configuration\n========\n",file=rmd_file,append=TRUE)
    cat("\nLast updated: ",format(Sys.time()),"\n",file=rmd_file,append=TRUE)

    config <- bb_settings_to_cols(config)

    config$local_file_paths <- vapply(seq_len(nrow(config)),function(z) {
        temp <- directory_from_url(config$source_url[[z]])
        temp[is.na(temp)] <- ""
        paste(unique(file.path(config$local_file_root[z],temp)),collapse=", ")},FUN.VALUE="")
    ## order by data group
    config <- unique(config[,!names(config) %in% c("source_url","method","postprocess")]) ## drop cols, take unique rows
    config$data_group[!nzchar(config$data_group)] <- NA ## so that these appear last
    config <- config[order(config$data_group,config$name), ]
    config$data_group[is.na(config$data_group)] <- ""

    last_group <- "blah"
    for (k in seq_len(nrow(config))) {
        if (last_group!=config$data_group[k]) {
            cat("\n## Data group: ",config$data_group[k],"\n",file=rmd_file,append=TRUE)
        }
        last_group <- config$data_group[k]
        cat("\n### ",config$name[k],"\n",file=rmd_file,append=TRUE)
        cat("\n",config$description[k],"\n",file=rmd_file,append=TRUE)
        if (inc_auth && !is.na(config$authentication_note[k]))
            cat("\nAuthentication note:", config$authentication_note[k],"\n",file=rmd_file,append=TRUE)
        if (inc_size)
            cat("\nApproximate size:", if (is.na(config$collection_size[k])) "not specified" else paste0(config$collection_size[k], " GB"),"\n",file=rmd_file,append=TRUE)
        cat("\nDocumentation link: ",config$doc_url[k],"\n",file=rmd_file,append=TRUE)
        if (inc_license) {
            this_citation <- config$citation[k]
            if (is.null(this_citation) || is.na(this_citation) || !nzchar(this_citation)) {
                this_citation <- "No citation details provided"
            }
            cat("\nCitation: ",this_citation,"\n",file=rmd_file,append=TRUE)
            this_license <- config$license[k]
            if (is.null(this_license) || is.na(this_license) || !nzchar(this_license)) {
                this_license <- "No formal license details provided"
            }
            cat("\nLicense: ",this_license,"\n",file=rmd_file,append=TRUE)
        }
        temp <- config$local_file_paths[[k]]
        temp <- gsub("\\\\","/",temp)
        temp <- unique(gsub("/+","/",temp))
        if (inc_path) cat("\nLocal file system paths: ",temp,"\n",file=rmd_file,append=TRUE,sep="")
        if (inc_access_function) {
            thisfun <- config$access_function[k]
            if (is.null(thisfun) || is.na(thisfun) || !nzchar(thisfun)) { thisfun <- "none suggested" }
            cat("\nAssociated access functions: ",thisfun,"\n",file=rmd_file,append=TRUE)
        }
    }

    if (format=="html") {
        ## knit to html
        rmarkdown::render(rmd_file,output_format="html_document",output_file=file)
        file
    } else {
        rmd_file
    }
}

## print method for config
#' @method print bb_config
#' @export
print.bb_config <- function(x, ...) {
    cat("Local file root:", x$settings$local_file_root, "\n")
    config <- bb_settings_to_cols(x)
    if (nrow(config) > 0) {
        config$local_file_paths <- vapply(seq_len(nrow(config)),function(z) {
            temp <- directory_from_url(config$source_url[[z]])
            temp[is.na(temp)] <- ""
            paste(unique(file.path(config$local_file_root[z],temp)),collapse=", ")},FUN.VALUE="")
        ## order by data group
        config <- unique(config[,!names(config) %in% c("source_url","method","postprocess")]) ## drop cols, take unique rows
        config$data_group[!nzchar(config$data_group)] <- NA ## so that these appear last
        config <- config[order(config$data_group,config$name), ]
        config$data_group[is.na(config$data_group)] <- ""

        last_group <- "blah"
        for (k in seq_len(nrow(config))) {
            if (last_group!=config$data_group[k]) {
                if (any(nzchar(config$data_group))) ## dont show this if NO sources have a data group defined
                    cat("\n# Data group: ",config$data_group[k],"\n")
            }
            last_group <- config$data_group[k]
            cat("\n## ",config$name[k],"\n")
            cat(config$description[k],"\n")
            if (!is.na(config$authentication_note[k]))
                cat("\nAuthentication note:", config$authentication_note[k],"\n")
            cat("Approximate size:", if (is.na(config$collection_size[k])) "not specified" else paste0(config$collection_size[k], " GB"),"\n")
            cat("Documentation link: ",config$doc_url[k],"\n")
            this_citation <- config$citation[k]
            if (is.null(this_citation) || is.na(this_citation) || !nzchar(this_citation)) {
                this_citation <- "No citation details provided"
            }
            cat("Citation: ",this_citation,"\n")
            this_license <- config$license[k]
            if (is.null(this_license) || is.na(this_license) || !nzchar(this_license)) {
                this_license <- "No formal license details provided"
            }
            cat("License: ",this_license,"\n")
            temp <- config$local_file_paths[[k]]
            temp <- gsub("\\\\","/",temp)
            temp <- unique(gsub("/+","/",temp))
            cat("Local file system paths: ",temp,"\n",sep="")
            thisfun <- config$access_function[k]
            if (is.null(thisfun) || is.na(thisfun) || !nzchar(thisfun)) { thisfun <- "none suggested" }
            cat("Associated access functions: ",thisfun,"\n")
        }
    } else {
        cat("Config contains no data sources.\n")
    }
    invisible(x)
}

## Internal function
## Validate a bowerbird configuration
##
## Runs some basic sanity checks on a bowerbird configuration.
##
## @param config bb_config: a bowerbird configuration (as returned by \code{bb_config})
##
## @return TRUE or throw error
## @examples
## \dontrun{
##   cf <- bb_config("/my/file/root") %>%
##     bb_add(bb_example_sources())
##   bb_validate() ## will complain about lacking authentication info
## }
## @seealso \code{\link{bb_config}}

bb_validate <- function(config) {
    assert_that(is(config,"bb_config"))
    cfds <- bb_data_sources(config)
    idx <- !is.na(cfds$authentication_note) & (na_or_empty(cfds$user) | na_or_empty(cfds$password))
    if (any(idx))
        stop(paste(sprintf("The data source \"%s\" requires authentication, but the user and/or password fields have not been set.\nThe authentication_note for this data source is:\n %s\n",cfds$name[idx],cfds$authentication_note[idx]),collapse="\n"))
    TRUE
}
