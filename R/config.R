## list of global flags, stored as attributes on the configuration tibble
## internal function
bb_global_atts <- function() c("wget_default_flags","wget_global_flags","http_proxy","ftp_proxy","local_file_root","clobber","skip_downloads")


#' Initialize a bowerbird configuration
#'
#' The parameters provided here are repository-wide settings, and will be applied to all data sources that are subsequently added to the configuration.
#' 
#' @param local_file_root string: location of data repository on local file system
#' @param wget_default_flags string: default flags to be passed to wget. These are overridden on a per-data source basis if the data source defines its own wget_flags
#' @param wget_global_flags string: wget flags that will be applied to all data sources. These will be appended to either the data source wget flags (if specified), or the wget_default_flags
#' @param http_proxy string: URL of HTTP proxy to use e.g. 'http://your.proxy:8080' (NULL for no proxy)
#' @param ftp_proxy string: URL of FTP proxy to use e.g. 'http://your.proxy:21' (NULL for no proxy)
#' @param clobber numeric: 0=do not overwrite existing files, 1=overwrite if the remote file is newer than the local copy, 2=always overwrite existing files. For data sources that use method 'wget', an appropriate flag will be added to the wget call according to the clobber setting ("--no-clobber" to not overwrite existing files, "--timestamping" to overwrite if the remote file is newer than the local copy)
#' @param skip_downloads logical: if TRUE, \code{bb_sync} will do a dry run of the synchronisation process but without actually downloading files. For data sources using method bb_wget, this means that the wget calls will not be executed, so e.g. any recursion handled by wget itself will not be simulated
#' @return configuration tibble
#'
#' @seealso \code{\link{bb_source}}
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     add(bb_source("NSIDC SMMR-SSM/I Nasateam sea ice concentration"))
#' 
#'   ## save to file
#'   saveRDS(cf,file="my_config.rds")
#'   ## load previously saved config
#'   cf <- readRDS(file="my_config.rds")
#' }
#'
#' @export
bb_config <- function(local_file_root,wget_default_flags=NULL,wget_global_flags="--restrict-file-names=windows --progress=dot:giga",http_proxy=NULL,ftp_proxy=NULL,clobber=1,skip_downloads=FALSE) {
    assert_that(is.string(local_file_root))
    assert_that(clobber %in% c(0,1,2))
    assert_that(is.flag(skip_downloads))
    cf <- tibble()
    attr(cf,"wget_default_flags") <- wget_default_flags
    attr(cf,"wget_global_flags") <- wget_global_flags
    attr(cf,"http_proxy") <- http_proxy
    attr(cf,"ftp_proxy") <- ftp_proxy
    attr(cf,"local_file_root") <- local_file_root
    attr(cf,"clobber") <- clobber
    attr(cf,"skip_downloads") <- skip_downloads
    class(cf) <- c("bb_cf",class(cf))
    cf
}

#' Select a subset of data sources within a bowerbird configuration
#'
#' @param .data list: configuration, as returned by \code{bb_config}
#' @param x list: configuration, as returned by \code{bb_config}
#' @param i numeric: integer row values
#' @param j numeric: ignored
#' @param ... : integer row values
#'
#' @return configuration
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     add(bb_sources()) %>%
#'     slice(1:5)
#' }
#'
#' @method slice bb_cf
#' @export
slice.bb_cf <- function(.data,...) {
    ##re_add_class <- function(z) {class(z) <- c("bb_cf",class(z)); z}
    ##class(.data) <- setdiff(class(.data),"bb_cf")
    ##re_add_class(copy_bb_attributes(do.call(slice_,list(.data,...)),.data))

    ## hmm, that doesn't work, so do this for now
    .data[...,]
}

#' @rdname slice.bb_cf
#' @method [ bb_cf
#' @export
`[.bb_cf` <- function(x,i,j,...) {
    re_add_class <- function(z) {class(z) <- c("bb_cf",class(z)); z}
    re_add_class(copy_bb_attributes(NextMethod("["),x))
}
    
    
#' Add a new data source to a bowerbird configuration
#' 
#' @param cf tibble: configuration, as returned by \code{bb_config}
#' @param source tibble: data source definition to add to the configuration, as returned by \code{bb_source}
#'
#' @return configuration tibble
#'
#' @seealso \code{\link{bb_source}} \code{\link{bb_config}}
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     add(bb_source("NSIDC SMMR-SSM/I Nasateam sea ice concentration"))
#' }
#' @export
add <- function(cf,source) {
    copy_bb_attributes(dplyr::bind_rows(cf,source),cf)
}


#' Returns a configuration object's bowerbird attributes
#'
#' @param cf tibble: configuration, as returned by \code{bb_config}
#'
#' @return named list
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#' cf <- bb_config(local_file_root="/your/data/directory")
#' bb_attributes(cf)
#'
#' @export
bb_attributes <- function(cf) {
    out <- attributes(cf)
    out[names(out) %in% bb_global_atts()]
}

## helper functions to manage attributes
## copy attributes
copy_bb_attributes <- function(to,from) {
    attributes(to) <- c(attributes(to),bb_attributes(from))
    to
}

## subset one or more rows
##bb_slice <- function(obj,rows) copy_bb_attributes(obj[rows,],obj)

## copy each bb attribute into column
bb_attributes_to_cols <- function(obj) {
    for (nm in bb_global_atts()) {
        if (!is.null(attr(obj,nm))) obj[,nm] <- attr(obj,nm)
    }
    obj
}

#' Produce summary of bowerbird configuration
#'
#' @param cf tibble: configuration, as returned by \code{bb_config}
#' @param file string: path to file to write summary to. A temporary file is used by default
#' @param format string: produce HTML ("html") or Rmarkdown ("Rmd") file?
#'
#' @return path to the summary file in HTML or Rmarkdown format
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     add(bb_source("NSIDC SMMR-SSM/I Nasateam sea ice concentration"))
#'   browseURL(bb_summary(cf))
#' }
#'
#' @export
bb_summary <- function(cf,file=tempfile(fileext=".html"),format="html") {
    assert_that(is.string(file))
    assert_that(is.string(format))
    format <- match.arg(tolower(format),c("html","rmd"))

    ## write summary as temporary Rmd file
    rmd_file <- tempfile(fileext=".Rmd")
    cat("---\ntitle: \"Summary of bowerbird repository\"\ndate: \"",date(),"\"\noutput:\n  html_document:\n    toc: true\n    theme: cerulean\n    highlight: default\n---\n\n",file=rmd_file,append=FALSE)

    cat("Summary of bowerbird configuration\n========\n",file=rmd_file,append=TRUE)
    cat("\nLast updated: ",format(Sys.time()),"\n",file=rmd_file,append=TRUE)

    cf <- bb_attributes_to_cols(cf)

    cf <- cf %>% group_by_(~data_group,~name) %>% mutate_(source_urls=~paste(file.path(local_file_root,directory_from_url(source_url)),collapse=", ")) %>% ungroup() %>% select_(~-source_url,~-method,~-method_flags,~-postprocess) %>% unique()
    
    cf$data_group[cf$data_group==""] <- NA ## so that arrange puts them last
    cf <- cf[order(cf$data_group), ]
    cf$data_group[is.na(cf$data_group)] <- ""
    
    last_group <- "blah"
    for (k in 1:nrow(cf)) {
        if (last_group!=cf$data_group[k]) {
            cat("\n## Data group: ",cf$data_group[k],"\n",file=rmd_file,append=TRUE)
        }
        last_group <- cf$data_group[k]
        cat("\n### ",cf$name[k],"\n",file=rmd_file,append=TRUE)
        cat("\n",cf$description[k],"\n",file=rmd_file,append=TRUE)
        cat("\nReference: ",cf$reference[k],"\n",file=rmd_file,append=TRUE)
        this_citation <- cf$citation[k]
        if (is.null(this_citation) || is.na(this_citation) || this_citation=="") {
            this_citation <- "No citation details provided; see reference"
        }
        cat("\nCitation: ",this_citation,"\n",file=rmd_file,append=TRUE)
        this_license <- cf$license[k]
        if (is.null(this_license) || is.na(this_license) || this_license=="") {
            this_license <- "No formal license details provided; see reference"
        }
        cat("\nLicense: ",this_license,"\n",file=rmd_file,append=TRUE)
        thisfun <- cf$access_function[k]
        if (is.null(thisfun) || is.na(thisfun) || thisfun=="") { thisfun <- "none registered" }
        temp <- cf$source_urls[[k]]
        temp <- gsub("\\\\","/",temp)
        temp <- unique(gsub("/+","/",temp))
        cat("\nLocal file system path:\n",paste(paste0("- ",temp),sep="\n",collapse="\n"),"\n",file=rmd_file,append=TRUE,sep="")
        cat("\nAssociated access functions: ",thisfun,"\n",file=rmd_file,append=TRUE)
    }

    if (format=="html") {
        ## knit to html
        rmarkdown::render(rmd_file,output_format="html_document",output_file=file)
        file
    } else {
        rmd_file
    }
}
