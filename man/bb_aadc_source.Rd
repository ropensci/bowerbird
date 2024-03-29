% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aadc.R
\name{bb_aadc_source}
\alias{bb_aadc_source}
\title{Generate a bowerbird data source object for an Australian Antarctic Data Centre data set}
\usage{
bb_aadc_source(metadata_id, eds_id, id_is_metadata_id = FALSE, ...)
}
\arguments{
\item{metadata_id}{string: the metadata ID of the data set. Browse the AADC's collection at https://data.aad.gov.au/metadata/records/ to find the relevant \code{metadata_id}}

\item{eds_id}{integer: specify one or more \code{eds_id}s if the metadata record has multiple data assets attached to it and you don't want all of them}

\item{id_is_metadata_id}{logical: if TRUE, use the \code{metadata_id} as the data source ID, otherwise use its DOI}

\item{...}{: passed to \code{\link{bb_source}}}
}
\value{
A tibble containing the data source definition, as would be returned by \code{\link{bb_source}}
}
\description{
Generate a bowerbird data source object for an Australian Antarctic Data Centre data set
}
\examples{
\dontrun{
  ## generate the source def for the "AADC-00009" dataset
  ##  (Antarctic Fur Seal Populations on Heard Island, Summer 1987-1988)
  src <- bb_aadc_source("AADC-00009")

  ## download it to a temporary directory
  data_dir <- tempfile()
  dir.create(data_dir)
  res <- bb_get(src, local_file_root = data_dir, verbose = TRUE)
  res$files
}

}
\seealso{
\code{\link{bb_source}}
}
