% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{bb_data_source_dir}
\alias{bb_data_source_dir}
\title{Return the local directory of each data source in a configuration}
\usage{
bb_data_source_dir(config)
}
\arguments{
\item{config}{bb_config: configuration as returned by \code{\link{bb_config}}}
}
\value{
character vector of directories
}
\description{
Return the local directory of each data source in a configuration. Files from each data source are stored locally in the associated directory. Note that if a data source has multiple \code{source_url} values, this function might return multiple directory names (depending on whether those \code{source_url}s map to the same directory or not).
}
\examples{
cf <- bb_config("/my/file/root") \%>\%
  bb_add(bb_example_sources())
bb_data_source_dir(cf)

}
