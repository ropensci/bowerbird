#' Generate a bowerbird data source object for a Zenodo data set
#'
#' @param id : the ID of the data set
#' @param use_latest logical: if \code{TRUE}, use the most recent version of the data set (if there is one). The most recent version might have a different data set ID to the one provided here
#'
#' @return A tibble containing the data source definition, as would be returned by \code{\link{bb_source}}
#'
#' @seealso \code{\link{bb_source}}
#'
#' @examples
#' \dontrun{
#'   ## generate the source object for the dataset
#'   ##   'Ichtyological data of Station de biologie des Laurentides 2019'
#'   src <- bb_zenodo_source(3533328)
#'
#'   ## download it to a temporary directory
#'   data_dir <- tempfile()
#'   dir.create(data_dir)
#'   res <- bb_get(src, local_file_root = data_dir, verbose = TRUE)
#'   res$files
#' }
#'
#' @export
bb_zenodo_source <- function(id, use_latest = FALSE) {
    jx <- jsonlite::fromJSON(paste0("https://zenodo.org/api/records/", id))
    ## $metadata$relations$version$is_last will be TRUE for latest version?
    if (isTRUE(use_latest) && length(jx$links$latest) == 1 && nzchar(jx$links$latest) && !is.na(jx$links$latest)) {
        if (grepl("zenodo.org/api/records/([[:digit:]]+)$", jx$links$latest)) {
            ## old format? latest ID is given directly
            latest_id <- stringr::str_match(jx$links$latest, "zenodo.org/api/records/([[:digit:]]+)$")
            if (nrow(latest_id) == 1 && !is.na(latest_id[1, 2])) return(bb_zenodo_source(id = latest_id[1, 2], use_latest = FALSE))
        } else if (grepl("zenodo.org/api/records/([[:digit:]]+)/", jx$links$latest)) {
            ## format has changed? (Feb 2024) - jx$links$latest gives the URL to the latest URL but does not have the latest ID in it
            jx <- jsonlite::fromJSON(jx$links$latest)
        } else {
            warning("could not find latest record, falling back to `use_latest = FALSE`")
        }
    }
    ne_or <- function(z, or) tryCatch(if (!is.null(z) && nzchar(z)) z else or, error = function(e) or)
    ## collection size
    csize <- tryCatch(as.numeric(format(sum(jx$files$filesize, na.rm = TRUE)/1024^3, digits = 1)), error = function(e) NULL)
    doi <- ne_or(jx$doi, ne_or(jx$metadata$doi, NULL))
    doc_url <- if (length(doi) > 0) paste0("https://doi.org/", doi) else paste0("https://zenodo.org/record/", id)
    postproc <- list()
    if (any(grepl("\\.zip$", jx$files$links$download, ignore.case = TRUE))) postproc <- c(postproc, list("unzip"))
    if (any(grepl("\\.gz$", jx$files$links$download, ignore.case = TRUE))) postproc <- c(postproc, list("gunzip"))
    ## maybe other post-processors
    ## list all urls. Does this cover datasets with multiple buckets? (Are there such things?)
    surls <- if ("download" %in% names(jx$files$links)) {
                 jx$files$links$download
             } else if ("self" %in% names(jx$files$links)) {
                 jx$files$links$self
             } else {
                 stop("could not find download URLs for zenodo source")
             }
    bb_source(name = ne_or(jx$title, ne_or(jx$metadata$title, "Dataset title")),
              id = ne_or(doi, id),
              description = ne_or(jx$metadata$description, "Dataset description"),
              ##keywords = ne_or(jx$metadata$keywords, NA_character_),
              doc_url = doc_url,
              citation = paste0("See ", doc_url, " for the correct citation"), ## seems odd that this isn't part of the record
              license = ne_or(unlist(unname(jx$metadata$license)), paste0("See ", doc_url, " for license information")),
              source_url = surls,
              method = list("bb_handler_rget", level = 1L, accept_download = ".*"), ## we know that we want to download everything here, so just accept everything for download
              comment = "Source definition created by bb_zenodo_source",
              postprocess = postproc,
              collection_size = csize)
}
