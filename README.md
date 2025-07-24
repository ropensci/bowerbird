
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci/bowerbird/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/bowerbird/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ropensci/bowerbird/graph/badge.svg)](https://app.codecov.io/gh/ropensci/bowerbird)
[![](https://badges.ropensci.org/139_status.svg)](https://github.com/ropensci/onboarding/issues/139)
<!-- badges: end -->

# Bowerbird

<img align="right" src="https://rawgit.com/ropensci/bowerbird/master/inst/extdata/bowerbird.svg" />

Often it’s desirable to have local copies of third-party data sets.
Fetching data on the fly from remote sources can be a great strategy,
but for speed or other reasons it may be better to have local copies.
This is particularly common in environmental and other sciences that
deal with large data sets (e.g. satellite or global climate model
products). Bowerbird is an R package for maintaining a local collection
of data sets from a range of data providers.

A comprehensive introduction to bowerbird can be found at
<https://docs.ropensci.org/bowerbird/articles/bowerbird.html>, along
with full package documentation.

This [blog post (2018-11-13)](https://ropensci.org/blog/2018/11/13/antarctic/)
illustrates the process of setting up bowerbird from scratch and using
it to obtain and work with several disparate data sets that overlap in
space and time.

## Installing

``` r
## use the SCAR r-universe package repository
options(repos = c(SCAR = "https://scar.r-universe.dev", CRAN = "https://cloud.r-project.org"))

## install
install.packages("bowerbird")

## or install from github
##install.packages("remotes") ## if needed
remotes::install_github("ropensci/bowerbird", build_vignettes = TRUE)
```

## Usage overview

### Configuration

Build up a configuration by first defining global options such as the
destination (a directory on your local file system, or an object store).
Typically you would choose this destination data directory to be a
persistent location, suitable for a data library. For demonstration
purposes here we’ll just use a temporary directory::

``` r
library(bowerbird)
my_directory <- tempdir()
cf <- bb_config(local_file_root = my_directory)
```

Bowerbird must then be told which data sources to synchronize. Let’s use
data from the Australian 2016 federal election, which is provided as one
of the example data sources:

``` r
my_source <- bb_example_sources("Australian Election 2016 House of Representatives data")

## add this data source to the configuration
cf <- bb_add(cf, my_source)
```

Once the configuration has been defined and the data source added to it,
we can run the sync process. We set `verbose = TRUE` here so that we see
additional progress output:

``` r
status <- bb_sync(cf, verbose = TRUE)
```

    ##  
    ## Tue Nov 30 16:32:35 2021 
    ## Synchronizing dataset: Australian Election 2016 House of Representatives data 
    ## Source URL http://results.aec.gov.au/20499/Website/HouseDownloadsMenu-20499-Csv.htm 
    ## -------------------------------------------------------------------------------------------- 
    ##  
    ##  this dataset path is: /tmp/data/results.aec.gov.au/20499/Website 
    ##  visiting http://results.aec.gov.au/20499/Website/HouseDownloadsMenu-20499-Csv.htm ... done. 
    ##  downloading file 1 of 47: http://results.aec.gov.au/20499/Website/Downloads/HouseCandidatesDownload-20499.csv ...  done. 
    ##  downloading file 2 of 47: http://results.aec.gov.au/20499/Website/Downloads/HouseMembersElectedDownload-20499.csv ...  done. 
    ##  downloading file 3 of 47: http://results.aec.gov.au/20499/Website/Downloads/HouseNominationsByStateDownload-20499.csv ...  done. 
    ##  
    ##  [... output truncated] 
    ##  
    ## Tue Nov 30 16:32:49 2021 dataset synchronization complete: Australian Election 2016 House of Representatives data

Congratulations\! You now have your own local copy of your chosen data
set. This particular example is fairly small (about 10MB), so it should
not take too long to download. Details of the files in this data set are
given in the `status$files` object:

``` r
status$files
## [[1]]
## # A tibble: 47 × 3
##    url                                file                               note   
##    <chr>                              <chr>                              <chr>  
##  1 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
##  2 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
##  3 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
##  4 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
##  5 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
##  6 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
##  7 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
##  8 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
##  9 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
## 10 http://results.aec.gov.au/20499/W… /tmp/data/results.aec.gov.au/2049… downlo…
## # … with 37 more rows
```

At a later time you can re-run this synchronization process. If the
remote files have not changed, and assuming that your configuration has
the `clobber` parameter set to 0 (“do not overwrite existing files”) or
1 (“overwrite only if the remote file is newer than the local copy”)
then the sync process will run more quickly because it will not need to
re-download any data files.

## Data source definitions

The [blueant](https://github.com/AustralianAntarcticDivision/blueant)
package provides a suite of bowerbird data source definitions themed
around Southern Ocean and Antarctic data, and includes a range of
oceanographic, meteorological, topographic, and other environmental data
sets.

## Other packages

Many other data-retrieval R packages exist. bowerbird is perhaps most
similar to the
[rdataretriever](https://cran.r-project.org/package=rdataretriever).
This package provides an R interface to the (Python-based) [Data
Retriever](http://www.data-retriever.org/), which in turn provides (at
time of writing) access to 85 ecological data sets. A quick comparison:

### rdataretriever

  - requires `retriever` to be installed, either as a Python package or
    via a platform-specific installer (see
    <http://www.data-retriever.org/>)

  - makes efforts to clean and standardize the data that it downloads,
    and get them into a consistent format on the user’s system

  - designed to make it easy for users to get on with the business of
    using those data sets

  - carries the tradeoff that adding new data sets (and maintaining the
    existing ones) takes a bit of effort, and it can be cumbersome to
    deal with data sets that contain many files, particularly if new
    files get added on a regular basis (e.g. satellite environmental
    data).

### bowerbird

  - pure R, no other system dependencies

  - designed to make it easy for users to keep a local, up-to-date
    collection of files from remote providers. It can do recursive
    downloads, and so is particularly suitable for collections that are
    structured as a large number of individual files in yearly or other
    subdirectories (typical of e.g. satellite or climate model data)

  - simply mirrors remote data to your local system, without attempting
    to reformat the data files or do anything else clever with them
    (other than uncompress, if needed). It just grabs them and saves
    them in whatever format the provider uses

  - the upside is that it is intended to be easy to write bowerbird
    definitions for new data sources. In many cases, it is only
    necessary to specify some metadata and the top-level URL, and
    bowerbird can recursively download linked resources from there

  - bowerbird itself contains only a few example data sets, but data
    definitions are available from other packages
    (e.g. [blueant](https://github.com/AustralianAntarcticDivision/blueant),
    \~55 marine/Southern Ocean data sets).

The rdataretriever and bowerbird packages are both part of the rOpenSci
project.

[![ropensci\_footer](https://ropensci.org/public_images/scar_footer.png)](https://ropensci.org)
