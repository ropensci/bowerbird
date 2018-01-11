#' Fingerprint the files associated with a data source
#'
#' @param config bb_config: configuration as returned by \code{\link{bb_config}}
#' @param hash string: algorithm to use to calculate file hashes: "md5", "sha1", or "none". Note that file hashing can be slow for large file collections
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
bb_fingerprint <- function(config,hash="sha1") {
    assert_that(is(config,"bb_config"))
    assert_that(is.string(hash))
    hash <- match.arg(tolower(hash),c("none","md5","sha1"))
    if (nrow(bb_data_sources(config))<1) {
        warning("config has no data sources: nothing for bb_fingerprint to do")
        return(invisible(NULL))
    }
    bb_validate(config)
    settings <- save_current_settings()
    fp <- as_tibble(do.call(rbind,lapply(seq_len(nrow(bb_data_sources(config))),function(di) do_fingerprint(bb_subset(config,di),hash,settings))))
    restore_settings(settings)
    fp
}


do_fingerprint <- function(this_dataset,hash,settings) {
    on.exit({ restore_settings(settings) })
    if (nrow(bb_data_sources(this_dataset))!=1) stop("expecting single-row data set")
    ## copy bb settings into this_dataset
    this_dataset <- bb_settings_to_cols(this_dataset)
    ## check that the root directory exists
    if (!dir_exists(this_dataset$local_file_root)) {
        ## no, it does not exist
        stop("local_file_root: ",this_dataset$local_file_root," does not exist")
    }
    setwd(this_dataset$local_file_root)

    this_path_no_trailing_sep <- sub("[\\/]$","",directory_from_url(this_dataset$source_url))
    myfiles <- list.files(path=this_path_no_trailing_sep,recursive=TRUE,full.names=TRUE) ## full.names TRUE so that names are relative to current working directory
    file_list <- file.info(myfiles)
    ##file_list <- file_list %>% mutate_(filename=~myfiles,data_source_id=~this_dataset$id) %>% select_(~filename,~data_source_id,~size,~mtime) %>% rename_(last_modified=~mtime)
    file_list$filename <- myfiles
    file_list$data_source_id <- this_dataset$id
    file_list$last_modified <- file_list$mtime
    file_list <- file_list[,c("filename","data_source_id","size","last_modified")]

    if (hash!="none") {
        file_list$hash <- vapply(myfiles,file_hash,FUN.VALUE="",hash)
    }
    file_list
}
