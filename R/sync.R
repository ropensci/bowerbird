#' Run a bowerbird data repository synchronisation
#'
#' This function takes a bowerbird configuration object and synchronizes each of the data sources defined within it. Data will be downloaded if they are not present on the local machine, or if the configuration has been set to update local files.
#'
#' Note that when \code{bb_sync} is run, the \code{local_file_root} directory must exist or \code{create_root=TRUE} must be specified (i.e. \code{bb_sync(...,create_root=TRUE)}). If \code{create_root=FALSE} and the directory does not exist, \code{bb_sync} will fail with an error.
#'
#' @param config bb_config: configuration as returned by \code{\link{bb_config}}
#' @param create_root logical: should the data root directory be created if it does not exist? If this is \code{FALSE} (default) and the data root directory does not exist, an error will be generated
#' @param verbose logical: if \code{TRUE}, provide additional progress output
#' @param catch_errors logical: if \code{TRUE}, catch errors and continue the synchronisation process. The sync process works through data sources sequentially, and so if \code{catch_errors} is \code{FALSE}, then an error during the synchronisation of one data source will prevent all subsequent data sources from synchronising
#'
#' @return a tibble with the \code{name}, \code{id}, \code{source_url}, and sync success \code{status} of each data source. Data sources that contain multiple source URLs will appear as multiple rows in the returned tibble, one per \code{source_url}
#'
#' @export
bb_sync <- function(config,create_root=FALSE,verbose=FALSE,catch_errors=TRUE) {
    ## general synchronization handler
    assert_that(is(config,"bb_config"))
    assert_that(is.flag(create_root),!is.na(create_root))
    assert_that(is.flag(verbose),!is.na(verbose))
    if (nrow(bb_data_sources(config))<1) {
        warning("config has no data sources: nothing for bb_sync to do")
        return(invisible(NULL))
    }
    bb_validate(config)
    ## check that wget can be found (this will also set it in the options)
    tmp <- bb_find_wget()
    ## save some current settings: path and proxy env values
    settings <- save_current_settings()
    on.exit({ restore_settings(settings) })
    ## iterate through each dataset in turn
    ## first expand the source_url list-column, so that we have one row per source_url entry
    ## tidyr::unnest does something like this, but is unhappy with the other list-columns in the data_sources tbl
    temp <- bb_data_sources(config)
    ns <- vapply(temp$source_url,length,FUN.VALUE=1) ## number of source_url entries per row
    bb_data_sources(config) <- do.call(rbind,lapply(seq_len(nrow(temp)),function(z){ out <- temp[rep(z,ns[z]),]; out$source_url <- temp$source_url[[z]]; out}))
    if (catch_errors) {
        sync_wrapper <- function(di) {
            tryCatch(do_sync_repo(bb_subset(config,di),create_root,verbose,settings),
                error=function(e) {
                    message("There was a problem synchronizing the dataset: ",bb_data_sources(config)$name[di],".\nThe error message was: ",e$message)
                    FALSE
                }
                )
        }
        sync_ok <- vapply(seq_len(nrow(bb_data_sources(config))),sync_wrapper,FUN.VALUE=TRUE)
    } else {
        sync_ok <- vapply(seq_len(nrow(bb_data_sources(config))),function(di) do_sync_repo(bb_subset(config,di),create_root,verbose,settings),FUN.VALUE=TRUE)
    }
    temp <- bb_data_sources(config)
    tibble(name=temp$name,id=temp$id,source_url=temp$source_url,status=sync_ok)
}


do_sync_repo <- function(this_dataset,create_root,verbose,settings) {
    assert_that(is(this_dataset,"bb_config"))
    on.exit({ restore_settings(settings) })
    if (nrow(bb_data_sources(this_dataset))!=1)
        stop("expecting single-row data set")
    this_att <- bb_settings(this_dataset)
    this_collection_size <- bb_data_sources(this_dataset)$collection_size
    if (interactive() && !is.null(this_collection_size) && !is.na(this_collection_size) && !is.null(this_att$warn_large_downloads) && this_collection_size>this_att$warn_large_downloads) {
        go_ahead <- menu(c("Yes","No"),title=sprintf("This data set is %.1f GB in size: are you sure you want to download it?",this_collection_size))
        if (go_ahead!=1) {
            if (verbose) cat(sprintf("\n dataset synchronization aborted: %s\n",bb_data_sources(this_dataset)$name))
            return(NA)
        }
    }

    ## check that the root directory exists
    if (!dir_exists(this_att$local_file_root)) {
        ## no, it does not exist
        ## unless create_root is TRUE, we won't create it, in case the user simply hasn't specified the right location
        if (create_root) {
            dir.create(this_att$local_file_root,recursive=TRUE)
        } else {
            stop("local_file_root: ",this_att$local_file_root," does not exist. Either create it or run bb_sync with create_root=TRUE")
        }
    }
    if (verbose) {
        cat(sprintf("\n%s\nSynchronizing dataset: %s\n",base::date(),bb_data_sources(this_dataset)$name))
        if (!all(is.na(bb_data_sources(this_dataset)$source_url))) cat(sprintf("Source URL %s\n",bb_data_sources(this_dataset)$source_url))
        cat("--------------------------------------------------------------------------------------------\n\n")
    }
    setwd(this_att$local_file_root)

    ## set proxy env vars
    if (any(c("ftp_proxy","http_proxy") %in% names(this_att))) {
        if (verbose) cat(sprintf(" setting proxy variables ... "))
        if ("http_proxy" %in% names(this_att) && !is.null(this_att$http_proxy)) {
            Sys.setenv(http_proxy=this_att$http_proxy)
            Sys.setenv(https_proxy=this_att$http_proxy)
        }
        if ("ftp_proxy" %in% names(this_att) && !is.null(this_att$ftp_proxy))
            Sys.setenv(ftp_proxy=this_att$ftp_proxy)
        if (verbose) cat(sprintf("done.\n"))
    }

    ## check postprocessing
    ## should be a nested list of (list of functions or call objects, or empty list)
    ## must be a list, each element is a list with first element resolving to a function via match.fun
    pp <- bb_data_sources(this_dataset)$postprocess[[1]]
    if (!(is.list(pp) && all(vapply(pp,function(z)is.list(z) && is_a_fun(z[[1]]),FUN.VALUE=TRUE))))
        stop("the postprocess argument should be a nested list, with each inner list having a function as its first element")
    ## do the main synchonization, usually directly with wget, otherwise with custom methods
    this_path_no_trailing_sep <- sub("[\\/]$","",bb_data_source_dir(this_dataset))
    if (verbose) cat(sprintf(" this dataset path is: %s\n",this_path_no_trailing_sep))
    ## build file list if postprocessing required
    if (length(pp)>0) {
        ## take snapshot of this directory before we start syncing
        if (verbose) cat(sprintf(" building file list ... "))
        file_list_before <- file.info(list.files(path=this_path_no_trailing_sep,recursive=TRUE,full.names=TRUE)) ## full.names TRUE so that names are relative to current working directory
        if (file.exists(this_path_no_trailing_sep)) {
            ## in some cases this points directly to a file
            temp <- file.info(this_path_no_trailing_sep)
            temp <- temp[!temp$isdir,]
            if (nrow(temp)>0) { file_list_before <- rbind(file_list_before,temp) }
        }
        if (verbose) cat(sprintf("done.\n"))
    }
    ## run the method
    mth <- match.fun(bb_data_sources(this_dataset)$method[[1]][[1]])
    ok <- do.call(mth,c(list(config=this_dataset,verbose=verbose),bb_data_sources(this_dataset)$method[[1]][-1]))
    ## postprocessing
    if (length(pp)>0) {
        if (is.na(ok) || !ok) {
            if (verbose) cat(" download failed or was interrupted: not running post-processing step\n")
        } else {
            ## build file list
            if (verbose) cat(sprintf(" building post-download file list of %s ... ",this_path_no_trailing_sep))
            file_list_after <- file.info(list.files(path=this_path_no_trailing_sep,recursive=TRUE,full.names=TRUE))
            if (file.exists(this_path_no_trailing_sep)) {
                ## in some cases this points directly to a file
                temp <- file.info(this_path_no_trailing_sep)
                temp <- temp[!temp$isdir,]
                if (nrow(temp)>0) { file_list_after <- rbind(file_list_after,temp) }
            }
            if (verbose) cat(sprintf("done.\n"))

            for (i in seq_len(length(pp))) {
                ## postprocessing steps are passed as functions or calls
                qq <- pp[[i]]
                qq <- match.fun(pp[[i]][[1]]) ## the function to call
                qq_args <- pp[[i]][-1]
                do.call(qq,c(list(config=this_dataset,file_list_before=file_list_before,file_list_after=file_list_after,verbose=verbose),qq_args))
            }
        }
    }
    if (verbose) cat(sprintf("\n%s dataset synchronization complete: %s\n",base::date(),bb_data_sources(this_dataset)$name))
    ok
}
