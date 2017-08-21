##-------------------------------------------
## various helper functions
## not exported for user

## file checksums
file_hash <- function(filename,hash="sha1") {
    assert_that(is.string(hash))
    hash <- match.arg(tolower(hash),c("md5","sha1"))
    switch(hash,
           md5=as.character(openssl::md5(file(filename))),
           sha1=as.character(openssl::sha1(file(filename))),
           stop("unexpected hash type",hash)
           )
}


## NA or empty string
na_or_empty <- function(z) is.na(z) | !nzchar(z)

is_nonempty_string <- function(z) is.string(z) && nzchar(z)

## check method (which may be function, call, or symbol) matches expected function
check_method_is <- function(method,expected) {
    assert_that(is.function(expected))
    identical(get_function_from_method(method),expected)
}

## get actual function from method (which may be function, call, or symbol)
get_function_from_method <- function(method) {
    assert_that(is.function(method) || is.call(method) || is.symbol(method) || is.string(method))
    if (is.function(method)) {
        return(method)
    } else if (is.call(method)) {
        if (all.names(method)[1]=="quote") {
            ## call was constructed as e.g. enquote(whatever)
            return(eval(method))
        } else {
            ## call was constructed as e.g. quote(whatever())
            return(get_function_from_method(all.names(method)[1])) ## check using name of called function
        }
    } else if (is.string(method)) {
        ## passed as function name
        if (exists(method,mode="function")) return(get(method))
    } else {
        ## symbol/name, by e.g. quote(whatever)
        if (exists(deparse(method),mode="function")) return(eval(method))
    }
    stop("could not extract the underlying method function")
}

## isn't there a better way to do this?
## qfun is a quoted function with arguments already provided, e.g. quote(fun(var=arg))
## we want to add some extra args (xargs, named list)
inject_args <- function(qfun,xargs,extras_first=TRUE) {
    assert_that(is.flag(extras_first))
    ## xargs is the named list of extra arguments to add
    if (extras_first) {
        arglist <- xargs
        if (length(qfun)>1) for (k in 2:length(qfun)) arglist <- c(arglist,qfun[[k]])
    } else {
        arglist <- list()
        if (length(qfun)>1) for (k in 2:length(qfun)) arglist <- c(arglist,qfun[[k]])
        arglist <- c(arglist,xargs)
    }
    arglist ## call this as e.g. do.call(all.names(qfun)[1],arglist)
}

save_current_settings <- function() {
    return(list(working_dir=getwd(), ## current working directory
                env_http_proxy=Sys.getenv("http_proxy"), ## proxy env vars
                env_https_proxy=Sys.getenv("https_proxy"),
                env_ftp_proxy=Sys.getenv("ftp_proxy")
                ))
}

restore_settings <- function(settings) {
    setwd(settings$working_dir)
    Sys.setenv(http_proxy=settings$env_http_proxy)
    Sys.setenv(https_proxy=settings$env_https_proxy)
    Sys.setenv(ftp_proxy=settings$env_ftp_proxy)
    invisible(NULL)
}

dir_exists <- function(z) file.exists(dirname(z)) && !(!file.info(z)$isdir || is.na(file.info(z)$isdir))


#' Return the local directory of each data source in a configuration
#'
#' Files from each data source are stored locally in the associated directory.
#'
#' @param config data.frame: configuration as returned by \code{\link{bb_config}}
#'
#' @return character vector of directories
#'
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     add(bb_sources(data_group="Sea surface temperature"))
#'   data_source_dir(cf)
#' }
#'
#' @export
data_source_dir <- function(config) {
    assert_that(is.data.frame(config))
    single_source_dir <- function(cfrow) {
        ## copy bb attrs into cfrow, in case handler relies on them
        cfrow <- bb_attributes_to_cols(cfrow)
        mth <- NULL
        try(mth <- get_function_from_method(cfrow$method[[1]]),silent=TRUE)
        if (is.function(mth)) {
            do.call(mth,list(cfrow=cfrow,local_dir_only=TRUE))
        } else {
            as.character(NA)
        }
    }
    vapply(seq_len(nrow(config)),function(z)single_source_dir(config[z,]),FUN.VALUE="")
}

directory_from_url <- function(this_url) {
    this_url <- sub("^(http|https|ftp)://","",this_url)
    this_url <- sub(":","+",this_url) ## port
    ## discard anything after the last trailing slash if it includes asterisks (is a file mask)
    ##sub("/[^/]*\\*[^/]*$","/",this_url)
    ## discard anything at all after the last trailing slash
    sub("/[^/]*$","/",this_url)
}

find_changed_files <- function(file_list_before,file_list_after,filename_pattern=".*") {
    ## expect both file_list_before and file_list_after to be a data.frame from file.info()
    ## detect changes on basis of ctime and size attributes
    ## returns names only
    changed_files <- setdiff(rownames(file_list_after),rownames(file_list_before)) ## anything that has appeared afterwards
    for (thisf in intersect(rownames(file_list_after),rownames(file_list_before))) {
        ## files in both
        thisfile_after <- file_list_after[rownames(file_list_after)==thisf,]
        thisfile_before <- file_list_before[rownames(file_list_before)==thisf,]
        if ((thisfile_after$ctime>thisfile_before$ctime) | (thisfile_after$size!=thisfile_before$size)) {
            changed_files <- c(changed_files,thisf)
        }
    }
    changed_files <- changed_files[str_detect(changed_files,filename_pattern)]
    if (is.null(changed_files)) {
        c()
    } else {
        changed_files
    }
}


do_decompress_files <- function(method,files,overwrite=TRUE) {
    ## decompress (unzip/gunzip) compressed files
    ## this function overwrites existing decompressed files if overwrite is TRUE
    assert_that(is.string(method))
    method <- match.arg(method,c("unzip","unzip_delete","gunzip","gunzip_delete","bunzip2","bunzip2_delete","uncompress","uncompress_delete"))
    if (grepl("uncompress",method)) {
        ## uncompress uses archive::file_read, which is a suggested package
        ## check that we have it
        if (!requireNamespace("archive",quietly=TRUE))
            stop("the archive package is needed for uncompress functionality")

    }
    ## unzip() issues warnings in some cases when operations have errors, and sometimes issues actual errors
    warn <- getOption("warn") ## save current setting
    options(warn=0) ## so that we can be sure that last.warning will be set
    ## MDS: this should use tail(warnings(), 1) instead
    last.warning <- NULL ## to avoid check note
    all_OK <- TRUE
    for (thisf in files) {
        ## decompress, check for errors in doing so
        cat(sprintf("  decompressing: %s ... ",thisf))
        switch(method,
               "unzip_delete"=,
               "unzip"={
                   was_ok <- FALSE
                   suppressWarnings(warning("")) ## clear last.warning message
                   ## unzip will put files in the current directory by default, so we need to extract the target directory for this file
                   target_dir <- dirname(thisf)
##cat(sprintf("\nWe are in %s\n",getwd()))
##cat(sprintf("thisf is: %s\n",thisf))
                   tryCatch({ unzipped_files <- unzip(thisf,list=TRUE) ## get list of files in archive
##cat("unzipped files: ")
##cat(str(unzipped_files),"\n")
                              files_to_extract <- unzipped_files$Name
                              if (!overwrite) {
                                  ## extract only files that don't exist
                                  files_to_extract<-files_to_extract[!file.exists(file.path(target_dir,files_to_extract))]
                              }
##cat("files to extract: ")
##cat(str(files_to_extract),"\n")
                              if (length(files_to_extract)>0) {
                                  cat(sprintf('extracting %d files into %s ... ',length(files_to_extract),target_dir))
                                  unzip(thisf,files=files_to_extract,exdir=target_dir) ## now actually unzip them
                                  was_ok <- is.null(last.warning[[1]]) && all(file.info(files_to_extract)$size>0)
                              } else {
                                  cat(sprintf('no new files to extract (not overwriting existing files) ... '))
                                  was_ok <- TRUE
                              }
                              cat("done.\n")
                          },
                            error=function(e) {
                                ## an error here might be because of an incompletely-downloaded file. Is there something more sensible to do in this case?
                                cat(sprintf("  %s failed to unzip (it may be incompletely-downloaded?)\n Error message was: %s",thisf,e))
                            })
                   if (identical(method,"unzip_delete")) {
                       ## if all looks OK, delete zipped file
                       if (was_ok) {
                           cat(sprintf("  unzip of %s appears OK, deleting\n",thisf))
                           unlink(thisf)
                       } else {
                           cat(sprintf("  problem unzipping %s, not deleting\n",thisf))
                       }
                   }
                   all_OK <- all_OK && was_ok
               },
               "gunzip_delete"=,
               "gunzip"={
                   ## gunzip takes care of deleting the compressed file if remove is TRUE
                   unzip_this <- TRUE
                   was_ok <- FALSE
                   if (!overwrite) {
                       ## check if file exists, so that we can issue a more informative trace message to the user
                       destname <- gsub("[.]gz$","",thisf,ignore.case=TRUE)
                       if (file.exists(destname)) {
                           cat(sprintf(" uncompressed file exists, skipping ... "))
                           unzip_this <- FALSE
                       }
                   }
                   if (unzip_this) {
                       ## wrap this in tryCatch block so that errors do not halt our whole process
                       tryCatch({gunzip(thisf,destname=sub("\\.gz$","",thisf),remove=method=="gunzip_delete",overwrite=overwrite); was_ok <- TRUE},
                                error=function(e){
                                    cat(sprintf("  problem gunzipping %s: %s",thisf,e))
                                }
                                )
                   }
                   all_OK <- all_OK && was_ok
                   cat(sprintf("done\n"))
               },
               "bunzip2_delete"=,
               "bunzip2"={
                   ## same as for gunzip
                   unzip_this <- TRUE
                   was_ok <- FALSE
                   if (!overwrite) {
                       ## check if file exists, so that we can issue a more informative trace message to the user
                       destname <- gsub("[.]bz2$","",thisf,ignore.case=TRUE)
                       if (file.exists(destname)) {
                           cat(sprintf(" uncompressed file exists, skipping ... "))
                           unzip_this <- FALSE
                       }
                   }
                   if (unzip_this) {
                       ## wrap this in tryCatch block so that errors do not halt our whole process
                       tryCatch({bunzip2(thisf,destname=sub("\\.bz2$","",thisf),remove=method=="bunzip2_delete",overwrite=overwrite);was_ok <- TRUE},
                                error=function(e){
                                    cat(sprintf("  problem bunzipping %s: %s",thisf,e))
                                }
                                )
                   }
                   all_OK <- all_OK && was_ok
                   cat(sprintf("done\n"))
               },
               "uncompress_delete"=,
               "uncompress"={
                   unzip_this <- TRUE
                   was_ok <- FALSE
                   destname <- gsub("\\.Z$","",thisf,ignore.case=TRUE)
                   if (!overwrite) {
                       ## check if file exists, so that we can issue a more informative trace message to the user
                       if (file.exists(destname)) {
                           cat(sprintf(" uncompressed file exists, skipping ... "))
                           unzip_this <- FALSE
                       }
                   }
                   if (unzip_this) {
                       ## wrap this in tryCatch block so that errors do not halt our whole process
                       tryCatch({
                           fsize <- ceiling(file.info(thisf)$size/1e4)*1e4
                           ff <- archive::file_read(thisf)
                           writeBin(readBin(ff,"raw",fsize),destname)
                           close(ff)
                           if (grepl("delete",method)) file.remove(thisf)
                           was_ok <- TRUE
                       },
                       error=function(e){
                           cat(sprintf("  problem uncompressing %s: %s",thisf,e))
                       })
                   }
                   all_OK <- all_OK && was_ok
                   cat(sprintf("done\n"))
               },
               stop("unsupported decompress method ",method)
               )
    }
    options(warn=warn) ## reset
    all_OK
}

