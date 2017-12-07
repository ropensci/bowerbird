#' \pkg{bowerbird}
#'
#' Often it's desirable to have local copies of third-party data sets. Fetching data on the fly from remote sources can be a great strategy, but for speed or other reasons it may be better to have local copies. This is particularly common in environmental and other sciences that deal with large data sets (e.g. satellite or global climate model products). Bowerbird is an R package for maintaining a local collection of data sets from a range of data providers.
#'
#' @name bowerbird
#' @aliases bowerbird-package
#' @docType package
#' @references \url{http://data.aad.gov.au}
#' @importFrom assertthat assert_that is.flag is.string
#' @importFrom dplyr arrange_ bind_rows group_by_ mutate_ rename_ select_ slice tbl_df tibble ungroup %>%
#' @importFrom methods is
#' @importFrom openssl sha1 md5
#' @importFrom stringr regex str_detect str_match str_split str_trim
#' @importFrom R.utils bunzip2 gunzip
#' @importFrom rmarkdown render
#' @importFrom stats na.omit
#' @importFrom sys exec_wait exec_internal
#' @importFrom utils download.file menu read.table str unzip
NULL
