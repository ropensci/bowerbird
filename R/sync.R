#' Run a bowerbird data repository synchronization
#'
#' This function takes a bowerbird configuration object and synchronizes each of the data sources defined within it. Data files will be downloaded if they are not present on the local machine, or if the configuration has been set to update local files.
#'
#' Note that when \code{bb_sync} is run, the \code{local_file_root} directory must exist or \code{create_root=TRUE} must be specified (i.e. \code{bb_sync(...,create_root=TRUE)}). If \code{create_root=FALSE} and the directory does not exist, \code{bb_sync} will fail with an error.
#'
#' @param config bb_config: configuration as returned by \code{\link{bb_config}}
#' @param create_root logical: should the data root directory be created if it does not exist? If this is \code{FALSE} (default) and the data root directory does not exist, an error will be generated
#' @param verbose logical: if \code{TRUE}, provide additional progress output
#' @param catch_errors logical: if \code{TRUE}, catch errors and continue the synchronization process. The sync process works through data sources sequentially, and so if \code{catch_errors} is \code{FALSE}, then an error during the synchronization of one data source will prevent all subsequent data sources from synchronizing
#' @param confirm_downloads_larger_than numeric or NULL: if non-negative, \code{bb_sync} will ask the user for confirmation to download any data source of size greater than this number (in GB). A value of zero will trigger confirmation on every data source. A negative or NULL value will not prompt for confirmation. Note that this only applies when R is being used interactively. The expected download size is taken from the \code{collection_size} parameter of the data source, and so its accuracy is dependent on the accuracy of the data source definition
#' @param dry_run logical: if \code{TRUE}, \code{bb_sync} will do a dry run of the synchronization process without actually downloading files
#'
#' @return a tibble with the \code{name}, \code{id}, \code{source_url}, sync success \code{status}, and \code{files} of each data source. Data sources that contain multiple source URLs will appear as multiple rows in the returned tibble, one per \code{source_url}. \code{files} is a tibble with columns \code{url} (the URL the file was downloaded from), \code{file} (the path to the file), and \code{note} (either "downloaded" for a file that was downloaded, "local copy" for a file that was not downloaded because there was already a local copy, or "decompressed" for files that were extracted from a downloaded (or already-locally-present) compressed file. \code{url} will be \code{NA} for "decompressed" files
#'
#' @seealso \code{\link{bb_config}}, \code{\link{bb_source}}
#'
#' @examples
#' \dontrun{
#'   ## Choose a location to store files on the local file system.
#'   ## Normally this would be an explicit choice by the user, but here
#'   ## we just use a temporary directory for example purposes.
#'
#'   td <- tempdir()
#'   cf <- bb_config(local_file_root = td)
#'
#'   ## Bowerbird must then be told which data sources to synchronize.
#'   ## Let's use data from the Australian 2016 federal election, which is provided as one
#'   ## of the example data sources:
#'
#'   my_source <- bb_example_sources("Australian Election 2016 House of Representatives data")
#'
#'   ## Add this data source to the configuration:
#'
#'   cf <- bb_add(cf, my_source)
#'
#'   ## Once the configuration has been defined and the data source added to it,
#'   ## we can run the sync process.
#'   ## We set \code{verbose=TRUE} so that we see additional progress output:
#'
#'   status <- bb_sync(cf, verbose = TRUE)
#'
#'   ## The files in this data set have been stored in a data-source specific
#'   ## subdirectory of our local file root:
#'
#'   status$files[[1]]
#'
#'   ## We can run this at any later time and our repository will update if the source has changed:
#'
#'   status2 <- bb_sync(cf, verbose = TRUE)
#' }
#'
#' @export
bb_sync <- function(config,create_root=FALSE,verbose=FALSE,catch_errors=TRUE,confirm_downloads_larger_than=0.1,dry_run=FALSE) {
    ## general synchronization handler
    assert_that(is(config,"bb_config"))
    assert_that(is.flag(create_root),!is.na(create_root))
    assert_that(is.flag(verbose),!is.na(verbose))
    assert_that(is.flag(dry_run),!is.na(dry_run))
    if (!is.null(confirm_downloads_larger_than)) {
        assert_that(is.numeric(confirm_downloads_larger_than),!is.na(confirm_downloads_larger_than))
        if (confirm_downloads_larger_than<0) confirm_downloads_larger_than <- Inf
    } else {
        confirm_downloads_larger_than <- Inf
    }

    if (nrow(bb_data_sources(config))<1) {
        warning("config has no data sources: nothing for bb_sync to do")
        return(invisible(NULL))
    }
    ## propagate dry_run info into the settings stored in config, so that it percolates through to handlers
    st <- bb_settings(config)
    st$dry_run <- dry_run
    bb_settings(config) <- st
    bb_validate(config)
    ## check that wget can be found (this will also set it in the options)
    ## but don't do this unless we are using the bb_handler_wget method, since the others (rget, oceandata, earthdata) don't need wget
    using_wget <- any(vapply(bb_data_sources(config)$method, function(z)grepl("wget", z[[1]][[1]]), FUN.VALUE = TRUE, USE.NAMES = FALSE))
    if (using_wget) tmp <- bb_find_wget(install = FALSE, error = TRUE)
    ## save some current settings: path and proxy env values
    settings <- save_current_settings()
    on.exit({ restore_settings(settings) })
    ## iterate through each dataset in turn
    ## previously we would expand the source_url list-column, so that we have one row per source_url entry
    ## this is no longer necessary except when using wget-based handlers
    if (using_wget) {
        temp <- bb_data_sources(config)
        ns <- vapply(temp$source_url,length,FUN.VALUE=1) ## number of source_url entries per row
        if (interactive()) temp$`__TEMPORARY_ROW_ID` <- seq_len(nrow(temp)) ## see below
        bb_data_sources(config) <- do.call(rbind,lapply(seq_len(nrow(temp)),function(z){ out <- temp[rep(z,ns[z]),]; out$source_url <- temp$source_url[[z]]; out}))
        ## a data source that had multiple source_urls will now be split across multiple rows
        ## if that data source also had a collection_size greater than our confirm_downloads_larger_than threshold, and we are running interactively, then the user will get asked to confirm each row separately
        ## so let's remove the collection_size entries from the second and subsequent rows
        if (interactive()) {
            temp <- bb_data_sources(config)
            temp$collection_size[duplicated(temp$`__TEMPORARY_ROW_ID`)] <- NA
            temp$`__TEMPORARY_ROW_ID` <- NULL
            bb_data_sources(config) <- temp
        }
    }
    if (catch_errors) {
        sync_wrapper <- function(di) {
            tryCatch(do_sync_repo(this_dataset=bb_subset(config,di),create_root=create_root,verbose=verbose,settings=settings,confirm_downloads_larger_than=confirm_downloads_larger_than),
                     error=function(e) {
                         msg <- paste0("There was a problem synchronizing the dataset: ",bb_data_sources(config)$name[di],".\nThe error message was: ",e$message)
                         if (verbose) cat(msg,"\n") else warning(msg)
                         tibble(ok = FALSE, files = list(tibble(url = character(), file = character(), note = character())))##, message = e$message)
                     }
                     )
        }
        sync_ok <- lapply(seq_len(nrow(bb_data_sources(config))), sync_wrapper)
    } else {
        sync_ok <- lapply(seq_len(nrow(bb_data_sources(config))), function(di) do_sync_repo(this_dataset = bb_subset(config, di), create_root = create_root, verbose = verbose, settings = settings, confirm_downloads_larger_than = confirm_downloads_larger_than))
    }
    sync_ok <- do.call(rbind, sync_ok)
    temp <- bb_data_sources(config)
    tibble(name = temp$name, id = temp$id, source_url = temp$source_url, status = sync_ok$ok, files = sync_ok$files)
}


do_sync_repo <- function(this_dataset,create_root,verbose,settings,confirm_downloads_larger_than) {
    assert_that(is(this_dataset,"bb_config"))
    on.exit({ restore_settings(settings) })
    if (nrow(bb_data_sources(this_dataset))!=1) stop("expecting single-row data set")
    this_att <- bb_settings(this_dataset)
    this_collection_size <- bb_data_sources(this_dataset)$collection_size
    dry_run <- !is.null(bb_settings(this_dataset)$dry_run) && !is.na(bb_settings(this_dataset)$dry_run) && bb_settings(this_dataset)$dry_run
    if (interactive() && !dry_run && !is.null(this_collection_size) && !is.na(this_collection_size) && !is.null(confirm_downloads_larger_than) && this_collection_size>confirm_downloads_larger_than) {
        go_ahead <- menu(c("Yes","No"),title=sprintf("%s\nThis data set is %.1f GB in size: are you sure you want to download it?",bb_data_sources(this_dataset)$name,this_collection_size))
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
    if (!is.null(this_att$ftp_proxy) || !is.null(this_att$http_proxy)) {
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
    method_loot <- do.call(mth, c(list(config = this_dataset, verbose = verbose), bb_data_sources(this_dataset)$method[[1]][-1]))
    if (!is.data.frame(method_loot)) method_loot <- tibble(ok = method_loot, files = NULL) ## older wget-based method handlers just return a status flag, newer (using bb_rget) return a tibble
    ok <- method_loot$ok
    ## postprocessing
    decompressed_files <- character()
    if (length(pp)>0) {
        if (is.na(ok) || !ok) {
            if (verbose) cat(" download failed or was interrupted: not running post-processing step\n")
        } else {
            if (is.null(method_loot$files[[1]])) {
                ## the method handler didn't return a list of files (likely a wget-based handler
                ## so we'll figure it out for ourselves: build list of files in our directory
                ## NOTE that this won't work if the data source downloaded files from a different server, because those files
                ## won't be in this_path_no_trailing_sep. In those cases the bb_rget based method needs to be used
                if (verbose) cat(sprintf(" building post-download file list of %s ... ", this_path_no_trailing_sep))
                file_list_after <- file.info(list.files(path = this_path_no_trailing_sep, recursive = TRUE, full.names = TRUE))
                if (file.exists(this_path_no_trailing_sep)) {
                    ## in some cases this points directly to a file
                    temp <- file.info(this_path_no_trailing_sep)
                    temp <- temp[!temp$isdir, ]
                    if (nrow(temp)>0) { file_list_after <- rbind(file_list_after, temp) }
                }
                if (verbose) cat(sprintf("done.\n"))
            } else {
                ## the handler has returned a list of files it found
                ## these may have been downloaded, or may have existed previously
                ## ultimately we should re-hash the postprocessing interface to better cope with this, but in the meantime we'll just coerce our list
                ## of files into something that the existing postprocessors will understand
                ## note that file paths returned by the handler are relative to the local_file_root
                ## if we are doing a dry run, no postprocessing
                if (dry_run) {
                    file_list_before <- character()
                    file_list_after <- character()
                } else {
                    ## all files go into both file_list_after and file_list_before
                    file_list_before <- file.info(file.path(bb_settings(this_dataset)$local_file_root, method_loot$files[[1]]$file))
                    ## the ones that were downloaded need to be marked as changed in file_list_after
                    file_list_after <- file.info(file.path(bb_settings(this_dataset)$local_file_root, method_loot$files[[1]]$file[method_loot$files[[1]]$was_downloaded]))
                    if (nrow(file_list_after) > 0) file_list_after$size <- 0 ## just modify the size of these, that is enough to have them flagged as changed in the postprocessors
                    file_list_after <- rbind(file_list_after, file.info(file.path(bb_settings(this_dataset)$local_file_root, method_loot$files[[1]]$file[!method_loot$files[[1]]$was_downloaded])))
                }
            }
            for (i in seq_len(length(pp))) {
                ## postprocessing steps are passed as functions or calls
                qq <- match.fun(pp[[i]][[1]]) ## the function to call
                qq_args <- pp[[i]][-1]
                res <- do.call(qq, c(list(config = this_dataset, file_list_before = file_list_before, file_list_after = file_list_after, verbose = verbose), qq_args))
                ok <- ok && res$status
                decompressed_files <- c(decompressed_files, res$files)
                ## deal with our record of any deleted files now
                if (length(res$deleted_files) > 0) {
                    res$deleted_files <- std_path(res$deleted_files, case = TRUE)
                    idx <- std_path(decompressed_files, case = TRUE) %in% res$deleted_files ## files that were deleted
                    decompressed_files <- decompressed_files[!idx]
                    idx <- std_path(method_loot$files[[1]]$file, case = TRUE) %in% res$deleted_files
                    method_loot$files[[1]]$file <- method_loot$files[[1]]$file[!idx]
                }
            }
        }
    }
    ## merge decompressed_files with files
    if (!is.null(method_loot$files[[1]])) method_loot$files[[1]]$file <- std_path(method_loot$files[[1]]$file) ## convert files to full paths
    temp <- method_loot$files[[1]]
    temp$note <- ifelse(temp$was_downloaded, "downloaded", "existing copy")
    temp <- temp[!names(temp) %in% c("was_downloaded")]
    if (length(decompressed_files) > 0) {
        temp <- rbind(temp, tibble(url = NA_character_, file = decompressed_files, note = "decompressed"))
    }
    if (verbose) cat(sprintf("\n%s dataset synchronization complete: %s\n", base::date(), bb_data_sources(this_dataset)$name))
    tibble(ok = ok, files = list(temp))
}
