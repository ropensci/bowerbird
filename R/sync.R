#' Run a bowerbird data repository synchronisation
#'
#' @param config data.frame: configuration as returned by \code{\link{bb_config}}
#' @param create_root logical: should the data root directory be created if it does not exist?
#' @param verbose logical: if TRUE, provide additional progress output
#' @param catch_errors logical: if TRUE, catch errors and continue the synchronisation process
#'
#' @return vector of logical values indicating success of each data source in config
#'
#' @export
bb_sync <- function(config,create_root=FALSE,verbose=TRUE,catch_errors=TRUE) {
    ## general synchronization handler
    assert_that(is(config,"bb_config"))
    assert_that(is.flag(create_root))
    assert_that(is.flag(verbose))
    if (nrow(bb_data_sources(config))<1) {
        warning("config has no data sources: nothing for bb_sync to do")
        return(invisible(NULL))
    }
    bb_validate(config)
    ## check that wget can be found (this will also set it in the options)
    if (is.null(bb_find_wget()))
        stop("could not find the wget executable. Try the bb_install_wget() function, or install it yourself and ensure that it is on the path")
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
    sync_ok
}


do_sync_repo <- function(this_dataset,create_root,verbose,settings) {
    assert_that(is(this_dataset,"bb_config"))
    on.exit({ restore_settings(settings) })
    if (nrow(bb_data_sources(this_dataset))!=1)
        stop("expecting single-row data set")
    this_att <- bb_settings(this_dataset)
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
    pp <- bb_data_sources(this_dataset)$postprocess
    if (length(pp)==1) {
        pp <- pp[[1]]
    } else {
        stop("expecting nested list for the postprocess argument")
    }
    if (!(is.list(pp) && (length(pp)<1 || all(vapply(pp,function(z)is.function(z) || is.call(z) || (is.symbol(z) && exists(deparse(z),mode="function")),FUN.VALUE=TRUE)))))
        stop("the postprocess argument should be a list of functions, calls, or symbols of functions")

    ## do the main synchonization, usually directly with wget, otherwise with custom methods
    this_path_no_trailing_sep <- sub("[\\/]$","",directory_from_url(bb_data_sources(this_dataset)$source_url))
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
    mth <- get_function_from_method(bb_data_sources(this_dataset)$method[[1]])
    ok <- do.call(mth,list(config=this_dataset,verbose=verbose))

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
                if (is.function(qq)) {
                    ## passed as function
                    ## evaluate with extra args
                    do.call(qq,list(config=this_dataset,file_list_before=file_list_before,file_list_after=file_list_after))
                } else if (is.symbol(qq)) {
                    ## passed as symbol
                    do.call(eval(qq),list(config=this_dataset,file_list_before=file_list_before,file_list_after=file_list_after))
                } else if (is.call(qq)) {
                    if (all.names(qq)[1]=="quote") {
                        ## call was constructed as e.g. enquote(fun)
                        do.call(eval(qq),list(config=this_dataset,file_list_before=file_list_before,file_list_after=file_list_after))
                    } else {
                        ## call was constructed as e.g. quote(fun()) or quote(fun(var=arg))
                        thisargs <- inject_args(qq,list(config=this_dataset,file_list_before=file_list_before,file_list_after=file_list_after))
                        do.call(all.names(qq)[1],thisargs)
                    }
                }
            }
        }
    }
    if (verbose) cat(sprintf("\n%s dataset synchronization complete: %s\n",base::date(),bb_data_sources(this_dataset)$name))
    ok
}
