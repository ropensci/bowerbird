#' Initialize a bowerbird configuration
#'
#' The parameters provided here are repository-wide settings, and will be applied to all data sources that are subsequently added to the configuration.
#'
#' @param local_file_root string: location of data repository on local file system
#' @param wget_default_flags character vector: default flags to be passed to wget. These are overridden on a per-data source basis if the data source defines its own wget_flags
#' @param wget_global_flags character vector: wget flags that will be applied to all data sources. These will be appended to either the data source wget flags (if specified), or the wget_default_flags
#' @param http_proxy string: URL of HTTP proxy to use e.g. 'http://your.proxy:8080' (NULL for no proxy)
#' @param ftp_proxy string: URL of FTP proxy to use e.g. 'http://your.proxy:21' (NULL for no proxy)
#' @param clobber numeric: 0=do not overwrite existing files, 1=overwrite if the remote file is newer than the local copy, 2=always overwrite existing files. For data sources that use method 'wget', an appropriate flag will be added to the wget call according to the clobber setting ("--no-clobber" to not overwrite existing files, "--timestamping" to overwrite if the remote file is newer than the local copy)
#' @param skip_downloads logical: if TRUE, \code{bb_sync} will do a dry run of the synchronisation process but without actually downloading files. For data sources using method bb_wget, this means that the wget calls will not be executed, so e.g. any recursion handled by wget itself will not be simulated
#' @return configuration object
#'
#' @seealso \code{\link{bb_source}}
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
bb_config <- function(local_file_root,wget_default_flags=character(),wget_global_flags=c("--restrict-file-names=windows","--progress=dot:giga"),http_proxy=NULL,ftp_proxy=NULL,clobber=1,skip_downloads=FALSE) {
    assert_that(is.string(local_file_root))
    assert_that(clobber %in% c(0,1,2))
    assert_that(is.flag(skip_downloads))
    assert_that(is.character(wget_default_flags))
    assert_that(is.character(wget_global_flags))
    structure(
        list(data_sources=tibble(),
             settings=list(
                 wget_default_flags=str_trim(wget_default_flags),
                 wget_global_flags=str_trim(wget_global_flags),
                 http_proxy=http_proxy,
                 ftp_proxy=ftp_proxy,
                 local_file_root=local_file_root,
                 clobber=clobber,
                 skip_downloads=skip_downloads)),
        class="bb_config")
}


#' Keep only selected data_sources in a bowerbird configuration
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config})
#' @param idx logical or numeric: index vector of data_source rows to retain
#'
#' @return configuration object
#'
#' @seealso \code{\link{bb_source}} \code{\link{bb_config}}
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     bb_add(bb_example_sources()) %>%
#'     bb_subset(1:2)
#' }
#' @export
bb_subset <- function(config,idx) {
    assert_that(is(config,"bb_config"))
    config$data_sources <- config$data_sources[idx,]
    config
}

#' Add new data sources to a bowerbird configuration
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config})
#' @param source data.frame: one or more data source definitions, as returned by \code{bb_source}, to add to the configuration
#'
#' @return configuration object
#'
#' @seealso \code{\link{bb_source}} \code{\link{bb_config}}
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     bb_add(bb_example_sources())
#' }
#' @export
bb_add <- function(config,source) {
    assert_that(is(config,"bb_config"))
    config$data_sources <- dplyr::bind_rows(config$data_sources,source)
    config
}


#' Returns a bowerbird configuration object's settings
#'
#' These are repository-wide settings that are applied to all data sources added to the configuration.
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config})
#'
#' @return named list
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#' cf <- bb_config(local_file_root="/your/data/directory")
#' bb_settings(cf)
#'
#' @export
bb_settings <- function(config) {
    assert_that(is(config,"bb_config"))
    config$settings
}

## internal helper function
## copy each bb setting into a column of data_sources table
## return only the augmented data_sources table
bb_settings_to_cols <- function(obj) {
    ## flags handled as lists
    obj$data_sources$wget_global_flags <- rep(list(obj$settings$wget_global_flags),nrow(obj$data_sources))
    obj$data_sources$wget_default_flags <- rep(list(obj$settings$wget_default_flags),nrow(obj$data_sources))
    for (nm in setdiff(names(obj$settings),c("wget_default_flags","wget_global_flags"))) {
        thisatt <- obj$settings[[nm]]
        if (!is.null(thisatt))
            obj$data_sources[,nm] <- thisatt
    }
    obj$data_sources
}

#' Produce summary of bowerbird configuration
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
    assert_that(is.flag(inc_license))
    assert_that(is.flag(inc_auth))
    assert_that(is.flag(inc_size))
    assert_that(is.flag(inc_access_function))
    assert_that(is.flag(inc_path))
    format <- match.arg(tolower(format),c("html","rmd"))

    ## write summary as temporary Rmd file
    rmd_file <- tempfile(fileext=".Rmd")
    cat("---\ntitle: \"Summary of bowerbird repository\"\ndate: \"",date(),"\"\noutput:\n  html_document:\n    toc: true\n    theme: cerulean\n    highlight: default\n---\n\n",file=rmd_file,append=FALSE)

    cat("Summary of bowerbird configuration\n========\n",file=rmd_file,append=TRUE)
    cat("\nLast updated: ",format(Sys.time()),"\n",file=rmd_file,append=TRUE)

    config <- bb_settings_to_cols(config)

    config <- config %>% group_by_(~data_group,~name) %>% mutate_(source_urls=~paste(file.path(local_file_root,directory_from_url(source_url)),collapse=", ")) %>% ungroup() %>% select_(~-source_url,~-method,~-method_flags,~-postprocess) %>% unique()

    config$data_group[config$data_group==""] <- NA ## so that arrange puts them last
    config <- config[order(config$data_group), ]
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
        cat("\nReference: ",config$reference[k],"\n",file=rmd_file,append=TRUE)
        if (inc_license) {
            this_citation <- config$citation[k]
            if (is.null(this_citation) || is.na(this_citation) || this_citation=="") {
                this_citation <- "No citation details provided; see reference"
            }
            cat("\nCitation: ",this_citation,"\n",file=rmd_file,append=TRUE)
            this_license <- config$license[k]
            if (is.null(this_license) || is.na(this_license) || this_license=="") {
                this_license <- "No formal license details provided; see reference"
            }
            cat("\nLicense: ",this_license,"\n",file=rmd_file,append=TRUE)
        }
        temp <- config$source_urls[[k]]
        temp <- gsub("\\\\","/",temp)
        temp <- unique(gsub("/+","/",temp))
        if (inc_path) cat("\nLocal file system path:\n",paste(paste0("- ",temp),sep="\n",collapse="\n"),"\n",file=rmd_file,append=TRUE,sep="")
        if (inc_access_function) {
            thisfun <- config$access_function[k]
            if (is.null(thisfun) || is.na(thisfun) || thisfun=="") { thisfun <- "none registered" }
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



#' Validate a bowerbird configuration
#'
#' Runs some basic sanity checks on a bowerbird configuration.
#'
#' @param config bb_config: a bowerbird configuration (as returned by \code{bb_config})
#'
#' @return TRUE or throw error
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     bb_add(bb_example_sources())
#'   bb_validate() ## will complain about lacking authentication info
#' }
#' @seealso \code{\link{bb_config}}
#'
#' @export
bb_validate <- function(config) {
    assert_that(is(config,"bb_config"))
    cfds <- config$data_sources
    idx <- !is.na(cfds$authentication_note) & (na_or_empty(cfds$user) || na_or_empty(cfds$password))
    if (any(idx))
        stop(paste(sprintf("The data source \"%s\" requires authentication, but the user and/or password fields have not been set.\nThe authentication_note for this data source is:\n %s\n",cfds$name[idx],cfds$authentication_note[idx]),collapse="\n"))
    TRUE
}
