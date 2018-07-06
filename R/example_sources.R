#' Example bowerbird data sources
#'
#' These example sources are useful as data sources in their own right, but are primarily provided as demonstrations of how to define data sources. See also \code{vignette("bowerbird")} for further examples and discussion.
#'
#' Example data sources:
#' \itemize{
#'   \item "NOAA OI SST V2" - a straightforward data source that requires a simple one-level recursive download
#'   \item "Australian Election 2016 House of Representatives data" - an example of a recursive download that uses additional criteria to restrict what is downloaded
#'   \item "CMEMS global gridded SSH reprocessed (1993-ongoing)" - a data source that requires a username and password
#'   \item "Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a" - an example data source that uses the \code{bb_handler_oceandata} method
#'   \item "Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 2" - an example data source that uses the \code{bb_handler_earthdata} method
#'   \item "Bathymetry of Lake Superior" - another example that passes extra flags to the \code{bb_handler_wget} call in order to restrict what is downloaded
#' }
#'
#' @param sources character: names or identifiers of one or more sources to return. See Details for the list of example sources and a brief explanation of each
#' @references See the \code{doc_url} and \code{citation} field in each row of the returned tibble for references associated with these particular data sources
#'
#' @return a tibble with columns as specified by \code{\link{bb_source}}
#'
#' @seealso \code{\link{bb_config}}, \code{\link{bb_handler_wget}}, \code{\link{bb_handler_oceandata}}, \code{\link{bb_handler_earthdata}}, \code{\link{bb_source_us_buildings}}
#'
#' @examples
#' \dontrun{
#' ## define a configuration and add the 2016 election data source to it
#' cf <- bb_config("/my/file/root") %>%
#'   bb_add(bb_example_sources("aus-election-house-2016"))
#'
#' ## synchronize (download) the data
#' bb_sync(cf)
#' }
#' @export
bb_example_sources <- function(sources) {
    if (!missing(sources)) assert_that(is.character(sources))
    out <- list()
    if (!missing(sources) && any(c("NOAA OI SST V2 wget", "oisst.v2 wget") %in% sources)) {
        out <- c(out, list(bb_source(
                          name = "NOAA OI SST V2",
                          id = "oisst.v2",
                          description = "Weekly and monthly mean and long-term monthly mean SST data, 1-degree resolution, 1981 to present. Ice concentration data are also included, which are the ice concentration values input to the SST analysis",
                          doc_url = "http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html",
                          citation = "NOAA_OI_SST_V2 data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their web site at http://www.esrl.noaa.gov/psd/",
                          source_url = c("ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/*"),
                          license = "Please cite",
                          method = list("bb_handler_wget",recursive=TRUE,level=1),
                          postprocess = NULL,
                          access_function = "raster::raster",
                          collection_size = 0.9,
                          data_group = "Sea surface temperature")))
    }
    if (missing(sources) || any(c("NOAA OI SST V2", "oisst.v2") %in% sources)) {
        out <- c(out, list(bb_source(
                          name = "NOAA OI SST V2",
                          id = "oisst.v2",
                          description = "Weekly and monthly mean and long-term monthly mean SST data, 1-degree resolution, 1981 to present. Ice concentration data are also included, which are the ice concentration values input to the SST analysis",
                          doc_url = "http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html",
                          citation = "NOAA_OI_SST_V2 data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their web site at http://www.esrl.noaa.gov/psd/",
                          source_url = "ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/",
                          license = "Please cite",
                          method = list("bb_handler_rget", level = 1),
                          postprocess = NULL,
                          access_function = "raster::raster",
                          collection_size = 0.9,
                          data_group = "Sea surface temperature")))
    }
    if (missing(sources) || any(c("Australian Election 2016 House of Representatives data", "aus-election-house-2016") %in% sources)) {
        out <- c(out, list(bb_source(
                          name = "Australian Election 2016 House of Representatives data",
                          id = "aus-election-house-2016",
                          description = "House of Representatives results from the 2016 Australian election.",
                          doc_url = "http://results.aec.gov.au/",
                          citation = "Copyright Commonwealth of Australia 2017. As far as practicable, material for which the copyright is owned by a third party will be clearly labelled. The AEC has made all reasonable efforts to ensure that this material has been reproduced on this website with the full consent of the copyright owners.",
                          source_url = "http://results.aec.gov.au/20499/Website/HouseDownloadsMenu-20499-Csv.htm",
                          license = "CC-BY",
                          method = list("bb_handler_rget", level = 1, accept_download = "csv$"),
                          collection_size=0.01,
                          data_group="Electoral")))
    }
    if (!missing(sources) && any(c("Australian Election 2016 House of Representatives data wget", "aus-election-house-2016 wget") %in% sources)) {
        out <- c(out, list(bb_source(
                          name = "Australian Election 2016 House of Representatives data",
                          id = "aus-election-house-2016",
                          description = "House of Representatives results from the 2016 Australian election.",
                          doc_url = "http://results.aec.gov.au/",
                          citation = "Copyright Commonwealth of Australia 2017. As far as practicable, material for which the copyright is owned by a third party will be clearly labelled. The AEC has made all reasonable efforts to ensure that this material has been reproduced on this website with the full consent of the copyright owners.",
                          source_url = c("http://results.aec.gov.au/20499/Website/HouseDownloadsMenu-20499-Csv.htm"),
                          license = "CC-BY",
                          method = list("bb_handler_wget",recursive=TRUE,level=1,accept="csv",reject_regex="Website/UserControls"),
                          collection_size = 0.01,
                          data_group = "Electoral")))
    }
    if (missing(sources) || any(c("CMEMS global gridded SSH reprocessed (1993-ongoing)", "SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047") %in% sources)) {
        out <- c(out, list(bb_source(
                          name="CMEMS global gridded SSH reprocessed (1993-ongoing)",
                          id="SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047",
                          description="For the Global Ocean - Multimission altimeter satellite gridded sea surface heights and derived variables computed with respect to a twenty-year mean. Previously distributed by Aviso+, no change in the scientific content. All the missions are homogenized with respect to a reference mission which is currently OSTM/Jason-2.\nVARIABLES\n- sea_surface_height_above_sea_level (SSH)\n- surface_geostrophic_eastward_sea_water_velocity_assuming_sea_level_for_geoid (UVG)\n- surface_geostrophic_northward_sea_water_velocity_assuming_sea_level_for_geoid (UVG)\n- sea_surface_height_above_geoid (SSH)\n- surface_geostrophic_eastward_sea_water_velocity (UVG)\n- surface_geostrophic_northward_sea_water_velocity (UVG)",
                          doc_url="http://cmems-resources.cls.fr/?option=com_csw&view=details&tab=info&product_id=SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047",
                          citation="In case of any publication, the Licensee will ensure credit the Copernicus Marine Service in the following manner: \"This study has been conducted using E.U. Copernicus Marine Service Information\"",
                          source_url=c("ftp://ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047/dataset-duacs-rep-global-merged-allsat-phy-l4-v3/"),
                          license="See http://marine.copernicus.eu/services-portfolio/service-commitments-and-licence/",
                          method=list("bb_handler_wget",recursive=TRUE,level=3),
                          postprocess=list("bb_gunzip"),
                          authentication_note="Copernicus Marine login required, see http://marine.copernicus.eu/services-portfolio/register-now/",
                          user="",
                          password="",
                          access_function="raster::raster",
                          collection_size=310,
                          data_group="Altimetry",warn_empty_auth=FALSE)))
    }
    if (missing(sources) || any(c("Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a", "SeaWiFS_L3m_MO_CHL_chlor_a_9km") %in% sources)) {
        out <- c(out, list(bb_source(
                          name = "Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a",
                          id = "SeaWiFS_L3m_MO_CHL_chlor_a_9km",
                          description = "Monthly remote-sensing chlorophyll-a from the SeaWiFS satellite at 9km spatial resolution",
                          doc_url = "https://oceancolor.gsfc.nasa.gov/",
                          citation = "See https://oceancolor.gsfc.nasa.gov/citations",
                          license = "Please cite",
                          method = list("bb_handler_oceandata", search = "S*L3m_MO_CHL_chlor_a_9km.nc"),
                          postprocess = NULL,
                          collection_size = 7.2,
                          data_group = "Ocean colour")))
    }
    if (missing(sources) || any(c("Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 2", "10.5067/EYICLBOAAJOU") %in% sources)) {
        out <- c(out, list(bb_source(
                          name="Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 2",
                          id="10.5067/EYICLBOAAJOU",
                          description="NSIDC provides this data set to aid in the investigations of the variability and trends of sea ice cover. Ice cover in these data are indicated by sea ice concentration: the percentage of the ocean surface covered by ice. The ice-covered area indicates how much ice is present; it is the total area of a pixel multiplied by the ice concentration in that pixel. Ice persistence is the percentage of months over the data set time period that ice existed at a location. The ice-extent indicates whether ice is present; here, ice is considered to exist in a pixel if the sea ice concentration exceeds 15 percent. This data set provides users with data about total ice-covered areas, sea ice extent, ice persistence, and monthly climatologies of sea ice concentrations.",
                          doc_url="https://nsidc.org/data/NSIDC-0192/versions/2",
                          citation="Stroeve, J. and W. Meier. 2017. Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 2. [Indicate subset used]. Boulder, Colorado USA. NASA National Snow and Ice Data Center Distributed Active Archive Center. doi: http://dx.doi.org/10.5067/EYICLBOAAJOU. [Date Accessed].",
                          source_url=c("https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0192_seaice_trends_climo_v2/"),
                          license="Please cite, see http://nsidc.org/about/use_copyright.html",
                          authentication_note="Requires Earthdata login, see https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+With+Earthdata+Login . Note that you will also need to authorize the application 'nsidc-daacdata' (see 'My Applications' at https://urs.earthdata.nasa.gov/profile)",
                          method=list("bb_handler_earthdata",recursive=TRUE,level=4,relative=TRUE),
                          user="",
                          password="",
                          postprocess=NULL,
                          collection_size=0.02,
                          data_group="Sea ice",warn_empty_auth=FALSE)))
    }
    if (missing(sources) || any(c("Bathymetry of Lake Superior", "greatlakes-superior-bathymetry") %in% sources)) {
        out <- c(out, list(bb_source(
                          name="Bathymetry of Lake Superior",
                          id="greatlakes-superior-bathymetry",
                          description="A draft version of the Lake Superior Bathymetry was compiled as a component of a NOAA project to rescue Great Lakes lake floor geological and geophysical data, and make it more accessible to the public. No time frame has been set for completing bathymetric contours of Lake Superior, though a 3 arc-second (~90 meter cell size) grid is available.",
                          doc_url="https://www.ngdc.noaa.gov/mgg/greatlakes/superior.html",
                          source_url=c("https://www.ngdc.noaa.gov/mgg/greatlakes/superior/data/"),
                          citation="Publisher: DOC/NOAA/NESDIS/NCEI > National Centers for Environmental Information, NESDIS, NOAA, U.S. Department of Commerce",
                          license="https://www.ngdc.noaa.gov/ngdcinfo/privacy.html#copyright",
                          method=list("bb_handler_wget",recursive=TRUE,level=2,accept_regex="/netcdf/",reject="index.html*"),
                          comment="Only the netcdf format data are retrieved here - adjust the accept_regex parameter in the method argument to get other formats",
                          postprocess=list("bb_gunzip"),
                          collection_size=0.03,
                          data_group="Topography")))
    }
    do.call(rbind, out)
}


#' Example bowerbird data source: Microsoft US Buildings
#'
#' This function constructs a data source definition for the Microsoft US Buildings data set. This data set contains 124,885,597 computer generated building footprints in all 50 US states. NOTE: currently, the downloaded zip files will not be unzipped automatically. Work in progress.
#'
#' @references \url{https://github.com/Microsoft/USBuildingFootprints}
#'
#' @param states character: (optional) one or more US state names for which to download data. If missing, data from all states will be downloaded. See the reference page for valid state names
#'
#' @return a tibble with columns as specified by \code{\link{bb_source}}
#'
#' @seealso \code{\link{bb_example_sources}}, \code{\link{bb_config}}, \code{\link{bb_handler_wget}}
#'
#' @examples
#' \dontrun{
#' ## define a configuration and add this buildings data source to it
#' ##  only including data for the District of Columbia and Hawaii
#' cf <- bb_config(tempdir()) %>%
#'   bb_add(bb_source_us_buildings(states = c("District of Columbia", "Hawaii")))
#'
#' ## synchronize (download) the data
#' bb_sync(cf)
#' }
#'
#' @export
bb_source_us_buildings <- function(states) {
    if (!missing(states)) {
        assert_that(is.character(states))
        if (any(!nzchar(states) | is.na(states))) stop("states must be a character vector of non-empty, non-NA strings")
        states <- gsub("[[:space:]]+", "", states) ## collapse whitespaces
        my_regex <- paste0("(", paste(states, collapse = "|"), ")")
    } else {
        my_regex <- ".+"
    }
    bb_source(name = "US Building Footprints",
              id = "Microsoft/USBuildingFootprints",
              description = "This dataset contains 124,885,597 computer generated building footprints in all 50 US states (figures correct as of 2-Jul-2018). These data are freely available to download and use. Building footprints have been digitized from Bing aerial imagery, using a modified ResNet34 neural network for semantic segmentation followed by polygonization of building pixel blobs into polygons.",
              doc_url = "https://github.com/Microsoft/USBuildingFootprints",
              source_url = "https://github.com/Microsoft/USBuildingFootprints",
              license = "Data in this repository are licensed by Microsoft under the Open Data Commons Open Database License (ODbL)",
              citation = "Please cite",
              method = list("bb_handler_rget", level = 1, accept_download = paste0("usbuildings/", my_regex, "\\.zip$")),
              comment="The collection size of 35GB applies to the complete data set (all states).",
              postprocess=list("bb_unzip"),
              collection_size=35,
              data_group="Built environment")
}
