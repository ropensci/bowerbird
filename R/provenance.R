#' Fingerprint the files associated with a data source
#'
#' @param config bb_config: configuration as returned by \code{\link{bb_config}}
#' @param hash string: algorithm to use to calculate file hashes: "md5", "sha1", or "none". Note that file hashing can be slow for large file collections
#' @param verbose logical: if TRUE, provide additional progress output
#'
#' @return tibble
#' @examples
#' \dontrun{
#'   cf <- bb_config("/my/file/root") %>%
#'     bb_add(bb_example_sources())
#'   bb_fingerprint(cf)
#' }
#'
#' @export
bb_fingerprint <- function(config,hash="sha1",verbose=TRUE) {
    assert_that(is(config,"bb_config"))
    assert_that(is.string(hash))
    assert_that(is.flag(verbose))
    hash <- match.arg(tolower(hash),c("none","md5","sha1"))
    assert_that(is.flag(verbose))
    if (nrow(bb_data_sources(config))<1) {
        warning("config has no data sources: nothing for bb_fingerprint to do")
        return(invisible(NULL))
    }
    bb_validate(config)
    settings <- save_current_settings()
    fp <- tbl_df(do.call(rbind,lapply(seq_len(nrow(bb_data_sources(config))),function(di) do_fingerprint(bb_subset(config,di),hash,verbose,settings))))
    ## fp <- config %>% rowwise() %>% do(do_fingerprint,verbose=verbose,settings=settings) ??
    restore_settings(settings)
    fp
}


do_fingerprint <- function(this_dataset,hash,verbose,settings) {
    on.exit({ restore_settings(settings) })
    if (nrow(bb_data_sources(this_dataset))!=1) stop("expecting single-row data set")
    ## copy bb settings into this_dataset
    this_dataset <- bb_settings_to_cols(this_dataset)
    ## check that the root directory exists
    if (!dir_exists(this_dataset$local_file_root)) {
        ## no, it does not exist
        stop("local_file_root: ",this_dataset$local_file_root," does not exist")
    }
    if (verbose) {
        cat(sprintf("Data source: %s",this_dataset$name))
        if (!all(is.na(this_dataset$source_url[[1]]))) cat(sprintf(", source URL %s",this_dataset$source_url[[1]]))
        cat("\n")
    }
    setwd(this_dataset$local_file_root)

    this_path_no_trailing_sep <- sub("[\\/]$","",directory_from_url(this_dataset$source_url))
    if (verbose) cat(sprintf(" building file list ... "))
    myfiles <- list.files(path=this_path_no_trailing_sep,recursive=TRUE,full.names=TRUE) ## full.names TRUE so that names are relative to current working directory
    file_list <- file.info(myfiles)
    file_list <- file_list %>% mutate_(filename=~myfiles,data_source_id=~this_dataset$id) %>% select_(~filename,~data_source_id,~size,~mtime) %>% rename_(last_modified=~mtime)
    if (hash!="none") {
        if (verbose) cat(sprintf(" calculating file hashes ... "))
        file_list$hash <- vapply(myfiles,file_hash,FUN.VALUE="",hash)
    }
    if (verbose) cat(sprintf("done.\n"))
    file_list
}
