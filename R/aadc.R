#' Generate a bowerbird data source object for an Australian Antarctic Data Centre data set
#'
#' @param metadata_id string: the metadata ID of the data set. Browse the AADC's collection at https://data.aad.gov.au/metadata/records/ to find the relevant \code{metadata_id}
#' @param eds_id integer: specify one or more \code{eds_id}s if the metadata record has multiple data assets attached to it and you don't want all of them
#' @param id_is_metadata_id logical: if TRUE, use the \code{metadata_id} as the data source ID, otherwise use its DOI
#' @param ... : passed to \code{\link{bb_source}}
#'
#' @return A tibble containing the data source definition, as would be returned by \code{\link{bb_source}}
#'
#' @seealso \code{\link{bb_source}}
#'
#' @examples
#' \dontrun{
#'   ## generate the source def for the "AADC-00009" dataset
#'   ##  (Antarctic Fur Seal Populations on Heard Island, Summer 1987-1988)
#'   src <- bb_aadc_source("AADC-00009")
#'
#'   ## download it to a temporary directory
#'   data_dir <- tempfile()
#'   dir.create(data_dir)
#'   res <- bb_get(src, local_file_root = data_dir, verbose = TRUE)
#'   res$files
#' }
#'
#' @export
bb_aadc_source <- function(metadata_id, eds_id, id_is_metadata_id = FALSE, ...) {
    assert_that(is.string(metadata_id))
    if (grepl("^http", metadata_id)) metadata_id <- basename(metadata_id)
    md <- get_aadc_md(metadata_id)
    uuid <- tryCatch({
        temp <- get_json(paste0("https://data.aad.gov.au/eds/api/metadata/", metadata_id, "?format=json"))
        if (nrow(temp) > 1 && !missing(eds_id)) temp <- temp[temp$eds_id %in% eds_id, ]
        temp$uuid
    }, error = function(e) NULL)
    if (length(uuid) < 1) stop("could not find record UUID")
    murl <- paste0("https://data.aad.gov.au/metadata/records/", metadata_id)
    doi <- tryCatch(sub("^doi:", "", md$data$data_set_citation$dataset_doi), error = function(e) NULL)
    ## get collection size from the S3 API
    postproc <- list()
    csize <- tryCatch({
        s3x <- do.call(rbind, lapply(uuid, function(u) {
            this <- get_json(paste0("https://data.aad.gov.au/eds/api/dataset/", u, "/objects?recursive=true"))
            if (is.data.frame(this)) this else NULL
        })) ## data.frame of files
        if (is.data.frame(s3x)) {
            if (any(grepl("\\.zip$", s3x$name, ignore.case = TRUE))) postproc <- c(postproc, list("unzip"))
            if (any(grepl("\\.gz$", s3x$name, ignore.case = TRUE))) postproc <- c(postproc, list("gunzip"))
            sz <- sum(s3x$size, na.rm = TRUE)/1024^3 ## in GB
            if (!is.na(sz)) ceiling(sz*10)/10 else NA_real_
        } else {
            NA_real_
        }
    }, error = function(e) NA_real_)
    bb_source(name = md$data$entry_title,
              id = if (isTRUE(id_is_metadata_id) || length(doi) != 1) metadata_id else doi,
              description = md$data$summary$abstract,
              doc_url = if (length(doi) > 0) paste0("https://doi.org/", doi) else murl,
              citation = md$data$citation,
              license = "CC-BY",
              method = list("bb_handler_aws_s3", bucket = uuid, base_url = "data.aad.gov.au/eds/api/dataset", region = "", prefix = metadata_id, use_https = FALSE, bucket_browser_url = paste0("https://data.aad.gov.au/eds/api/dataset/", uuid, "/objects?recursive=true")),
              ## bucket_browser_url is a workaround for AADC servers not supporting the usual aws.s3::get_bucket method
              comment = "Source definition created by bb_aadc_source",
              postprocess = postproc,
              collection_size = csize, ...)
}

get_aadc_md <- function(metadata_id) {
    get_json(paste0("https://data.aad.gov.au/metadata/api/records/", metadata_id, "?format=json"))
}

get_json <- function(url) {
    out <- curl::curl_fetch_memory(url, handle = curl::new_handle(ssl_verifypeer = 0L))
    jsonlite::fromJSON(rawToChar(out$content))
}
