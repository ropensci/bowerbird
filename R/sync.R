#' Run a bowerbird data repository synchronisation
#'
#' @param config tibble: configuration as returned by \code{\link{bb_config}}
#' @param create_root logical: should the data root directory be created if it does not exist?
#' @param verbose logical: if TRUE, provide additional progress output
#' @param catch_errors logical: if TRUE, catch errors and continue the synchronisation process
#'
#' @return vector of logical values indicating success of each data source in config
#'
#' @export
bb_sync <- function(config,create_root=FALSE,verbose=TRUE,catch_errors=TRUE) {
    ## general synchronization handler
    assert_that(is.data.frame(config))
    assert_that(is.flag(create_root))
    assert_that(is.flag(verbose))
    ## check that wget can be found (this will also set it in the options)
    blah <- wget_exe()
    ## save some current settings: path and proxy env values
    settings <- save_current_settings()
    ## iterate through each dataset in turn
    if (catch_errors) {
        sync_wrapper <- function(di) {
            tryCatch({
                do_sync_repo(config %>% bb_slice(di),create_root,verbose,settings)},
                error=function(e) {
                    cat("\nThere was a problem synchronizing the dataset:",config$name[di],".\nThe error message was:",e$message,"\n")
                }
                )
        }
        sync_ok <- sapply(1:nrow(config),sync_wrapper)
    } else {
        sync_ok <- sapply(1:nrow(config),function(di) do_sync_repo(config %>% bb_slice(di),create_root,verbose,settings))
    }
    restore_settings(settings)
    sync_ok
}


do_sync_repo <- function(this_dataset,create_root,verbose,settings) {
    on.exit({ restore_settings(settings) })
    if (nrow(this_dataset)!=1) stop("expecting single-row data set")
    ## copy bb attrs into this_dataset
    this_dataset <- bb_attributes_to_cols(this_dataset)
    ## check that the root directory exists
    if (!dir_exists(this_dataset$local_file_root)) {
        ## no, it does not exist
        ## unless create_root is TRUE, we won't create it, in case the user simply hasn't specified the right location
        if (create_root) {
            dir.create(this_dataset$local_file_root,recursive=TRUE)
        } else {
            stop("local_file_root: ",this_dataset$local_file_root," does not exist. Either create it or run bb_sync with create_root=TRUE")
        }
    }
    cat(sprintf("\n%s\nSynchronizing dataset: %s with source URL %s\n----------------------------------------------------------------------------------------------------------\n\n",base::date(),this_dataset$name,this_dataset$source_url))
    setwd(this_dataset$local_file_root)

    ## set proxy env vars
    if (any(c("ftp_proxy","http_proxy") %in% names(this_dataset))) {
        if (verbose) cat(sprintf(" setting proxy variables ... "))
        if ("http_proxy" %in% names(this_dataset)) {
            Sys.setenv(http_proxy=this_dataset$http_proxy)
            Sys.setenv(https_proxy=this_dataset$http_proxy)
        }
        if ("ftp_proxy" %in% names(this_dataset)) Sys.setenv(ftp_proxy=this_dataset$ftp_proxy)
        if (verbose) cat(sprintf("done.\n"))
    }

    ## check postprocessing
    ## should be a nested list of (list of functions or call objects, or empty list)
    pp <- this_dataset$postprocess
    if (length(pp)==1) {
        pp <- pp[[1]]
    } else {
        stop("expecting nested list for the postprocess argument")
    }
    if (!(is.list(pp) && (length(pp)<1 || all(sapply(pp,function(z)is.function(z) || inherits(z,"call"))))))
        stop("the postprocess argument should be a list of functions or calls (unevaluated functions)")

    ## do the main synchonization, usually directly with wget, otherwise with custom methods
    cat(sprintf("\n---\nProcessing source_url: %s\n",this_dataset$source_url))
    this_path_no_trailing_sep <- sub("[\\/]$","",directory_from_url(this_dataset$source_url))
    if (verbose) {
        cat(sprintf(" this dataset path is: %s\n",this_path_no_trailing_sep))
    }
    file_pattern <- sub(".*/","",this_dataset$source_url)
    if (nchar(file_pattern)<1) file_pattern <- NULL
    if (identical(this_dataset$method[[1]],aadc_eds_get)) file_pattern <- NULL ## set to null so that file_list_* (below) searches the data directory
    ## build file list if postprocessing required
    if (length(pp)>0) {
        ## take snapshot of this directory before we start syncing
        if (verbose) {
            cat(sprintf(" building file list ... "))
        }
        file_list_before <- file.info(list.files(path=this_path_no_trailing_sep,pattern=file_pattern,recursive=TRUE,full.names=TRUE)) ## full.names TRUE so that names are relative to current working directory
        if (file.exists(this_path_no_trailing_sep)) {
            ## in some cases this points directly to a file
            temp <- file.info(this_path_no_trailing_sep)
            temp <- temp[!temp$isdir,]
            if (nrow(temp)>0) { file_list_before <- rbind(file_list_before,temp) }
        }
        ##cat("file list before:\n")
        ##cat(str(file_list_before),"\n")
        if (verbose) cat(sprintf("done.\n"))
    }
    this_dataset$method[[1]](this_dataset)

    ## build file list if postprocessing required
    if (length(pp)>0) {
        if (verbose) { cat(sprintf(" building post-download file list of %s ... ",this_path_no_trailing_sep)) }
        file_list_after <- file.info(list.files(path=this_path_no_trailing_sep,pattern=file_pattern,recursive=TRUE,full.names=TRUE))
        if (file.exists(this_path_no_trailing_sep)) {
            ## in some cases this points directly to a file
            temp <- file.info(this_path_no_trailing_sep)
            temp <- temp[!temp$isdir,]
            if (nrow(temp)>0) { file_list_after <- rbind(file_list_after,temp) }
        }
        ##cat("file list after:\n")
        ##cat(str(file_list_after),"\n")
        if (verbose) cat(sprintf("done.\n"))
    }

    if (length(pp)>0) {
        for (i in 1:length(pp)) {
            ## postprocessing steps are passed as functions or calls
            qq <- pp[[i]]
            if (inherits(qq,"call")) {
                ## add extra args
                qq$data_source <- this_dataset
                qq$file_list_before <- file_list_before
                qq$file_list_after <- file_list_after
                ## evaluate
                eval(qq)
            } else if (is.function(qq)) {
                ## evaluate with extra args
                do.call(qq,list(data_source=this_dataset,file_list_before=file_list_before,file_list_after=file_list_after))
            }
        }
    }
    cat(sprintf("\n%s dataset synchronization complete: %s (%s)\n",base::date(),this_dataset$name,this_dataset$source_url))
    TRUE
}
