#' Postprocessing: decompress zip, gz, bz2, Z files and optionally delete the compressed copy
#'
#' Functions for decompressing files after downloading. These functions are not intended to be called directly, but rather are specified as a \code{postprocess} option in \code{\link{bb_source}}. \code{bb_unzip}, \code{bb_gunzip}, \code{bb_bunzip2}, and \code{bb_uncompress} are convenience wrappers around \code{bb_decompress} that specify the method.
#'
#' If the data source delivers compressed files, you will most likely want to decompress them after downloading. These functions will do this for you. By default, these do not delete the compressed files after decompressing. The reason for this is so that on the next synchronization run, the local (compressed) copy can be compared to the remote compressed copy, and the download can be skipped if nothing has changed. Deleting local compressed files will save space on your file system, but may result in every file being re-downloaded on every synchronization run.
#'
#' @param method string: one of "unzip","gunzip","bunzip2","decompress"
#' @param delete logical: delete the zip files after extracting their contents?
#' @param ... : extra parameters passed automatically by \code{bb_sync}
#'
#' @return TRUE on success
#'
#' @seealso \code{\link{bb_source}}, \code{\link{bb_config}}, \code{\link{bb_cleanup}}
#' @examples
#' \dontrun{
#'   ## decompress .zip files after synchronisation but keep zip files intact
#'   my_source <- bb_source(...,postprocess=list("bb_unzip"))
#'
#'   ## decompress .zip files after synchronisation and delete zip files
#'   my_source <- bb_source(...,postprocess=list(list("bb_unzip",delete=TRUE)))
#' }
#'
#' @export
bb_decompress <- function(method,delete=FALSE,...) {
    assert_that(is.flag(delete),!is.na(delete))
    assert_that(is.string(method))
    method <- match.arg(tolower(method),c("unzip","gunzip","bunzip2","uncompress"))
    do.call(bb_decompress_inner,list(...,method=method,delete=delete))
}


# @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
# @param file_list_before data.frame: files present in the directory before synchronising, as returned by \code{file.info}. (This is not required if \code{delete} is TRUE)
# @param file_list_after data.frame: files present in the directory after synchronising, as returned by \code{file.info}. (This is not required if \code{delete} is TRUE)
# @param verbose logical: if TRUE, provide additional progress output
# @param method string: one of "unzip","gunzip","bunzip2","decompress"
# @param delete logical: delete the zip files after extracting their contents?
# @param ... : parameters passed to bb_decompress
#
# @return TRUE on success
bb_decompress_inner <- function(config,file_list_before,file_list_after,verbose=FALSE,method,delete=FALSE) {
    assert_that(is(config,"bb_config"))
    assert_that(nrow(bb_data_sources(config))==1)
    assert_that(is.flag(delete),!is.na(delete))
    assert_that(is.string(method))
    method <- match.arg(tolower(method),c("unzip","gunzip","bunzip2","uncompress"))
    ignore_case <- method=="unzip"
    file_pattern <- switch(method,
                           "unzip"="\\.zip$",
                           "gunzip"="\\.gz$",
                           "bunzip2"="\\.bz2$",
                           "uncompress"="\\.Z$",
                           stop("unrecognized decompression")
                           )
    if (delete) {
        files_to_decompress <- list.files(directory_from_url(bb_data_sources(config)$source_url),pattern=file_pattern,recursive=TRUE,ignore.case=ignore_case)
        do_decompress_files(paste0(method,"_delete"),files=files_to_decompress)
    } else {
        ## decompress but retain compressed file. decompress only if .zip/.gz/.bz2/.Z file has changed
        files_to_decompress <- find_changed_files(file_list_before,file_list_after,file_pattern)
        do_decompress_files(method,files=files_to_decompress)
        ## also decompress if uncompressed file does not exist
        files_to_decompress <- setdiff(rownames(file_list_after),files_to_decompress) ## those that we haven't just dealt with
        files_to_decompress <- files_to_decompress[str_detect(files_to_decompress,regex(file_pattern,ignore_case=ignore_case))] ## only .zip/.gz/.bz2/.Z files
        do_decompress_files(method,files=files_to_decompress,overwrite=FALSE)
        ## nb this may be slow, so might be worth explicitly checking for the existence of uncompressed files
    }
}

#' @rdname bb_decompress
#' @export
bb_unzip <- function(...) bb_decompress(...,method="unzip")

#' @rdname bb_decompress
#' @export
bb_gunzip <- function(...) bb_decompress(...,method="gunzip")

#' @rdname bb_decompress
#' @export
bb_bunzip2 <- function(...) bb_decompress(...,method="bunzip2")

#' @rdname bb_decompress
#' @export
bb_uncompress <- function(...) bb_decompress(...,method="uncompress")


#' Postprocessing: remove unwanted files
#'
#' A function for removing unwanted files after downloading. This function is not intended to be called directly, but rather is specified as a \code{postprocess} option in \code{\link{bb_source}}.
#'
#' This function can be used to remove unwanted files after a data source has been synchronized. The \code{pattern} specifies a regular expression that is passed to \code{file.info} to find matching files, which are then deleted. Note that only files in the data source's own directory (i.e. its subdirectory of the \code{local_file_root} specified in \code{bb_config}) are subject to deletion. But, beware! Some data sources may share directories, which can lead to unexpected file deletion. Be as specific as you can with the \code{pattern} parameter.
#'
#' @param pattern string: regular expression, passed to \code{file.info}
#' @param recursive logical: should the cleanup recurse into subdirectories?
#' @param ignore_case logical: should pattern matching be case-insensitive?
#' @param ... : extra parameters passed automatically by \code{bb_sync}
#'
#' @return TRUE on success
#'
#' @seealso \code{\link{bb_source}}, \code{\link{bb_config}}, \code{\link{bb_decompress}}
#'
#' @examples
#' \dontrun{
#'   ## remove .asc files after synchronisation
#'   my_source <- bb_source(...,postprocess=list(list("bb_cleanup",pattern="\\.asc$")))
#' }
#'
#' @export
bb_cleanup <- function(pattern,recursive=FALSE,ignore_case=FALSE,...) {
    assert_that(is.string(pattern))
    assert_that(is.flag(recursive),!is.na(recursive))
    assert_that(is.flag(ignore_case),!is.na(ignore_case))
    do.call(bb_cleanup_inner,list(...,pattern=pattern,recursive=recursive,ignore_case=ignore_case))
}


# @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
# @param file_list_before data.frame: files present in the directory before synchronising, as returned by \code{file.info}
# @param file_list_after data.frame: files present in the directory after synchronising, as returned by \code{file.info}
# @param verbose logical: if TRUE, provide additional progress output
# @param pattern string: regular expression, passed to \code{file.info}
# @param recursive logical: should the cleanup recurse into subdirectories?
# @param ignore_case logical: should pattern matching be case-insensitive?
#
# @return TRUE on success
#
bb_cleanup_inner <- function(config,file_list_before,file_list_after,verbose=FALSE,pattern,recursive=FALSE,ignore_case=FALSE) {
    assert_that(is(config,"bb_config"))
    assert_that(nrow(bb_data_sources(config))==1)
    to_delete <- list.files(path=bb_data_source_dir(config),pattern=pattern,recursive=recursive,ignore.case=ignore_case,full.names=TRUE)
    if (verbose) {
        if (length(to_delete)>0) {
            cat(sprintf(" cleaning up files: %s\n",paste(to_delete,collapse=",")))
        } else {
            cat(" cleanup: no files to remove\n")
        }
    }
    unlink(to_delete)==0
}
