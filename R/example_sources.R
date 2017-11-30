#' Example bowerbird data sources
#'
#' Five example data sources:
#' \itemize{
#'   \item "NOAA OI SST V2"
#'   \item "CMEMS global gridded SSH reprocessed (1993-ongoing)" - a data source that requires a username and password
#'   \item "Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a" - an example data source that uses the \code{bb_handler_oceandata} method
#'   \item "Nimbus Ice Edge Points from Nimbus Visible Imagery" - an example data source that uses the \code{bb_handler_earthdata} method
#'   \item "Bathymetry of Lake Superior" - an example that passes extra flags to the \code{bb_handler_wget} call in order to restrict what is downloaded
#' }
#' @references See the \code{reference} and \code{citation} field in each row of the returned tibble for references associated with these particular data sources
#'
#' @return tibble
#'
#' @seealso \code{\link{bb_config}} \code{\link{bb_handler_wget}} \code{\link{bb_handler_oceandata}} \code{\link{bb_handler_earthdata}}
#'
#' @examples
#' \dontrun{
#' cf <- bb_config("/my/file/root") %>%
#'   bb_add(bb_example_sources()[5,])
#' bb_sync(cf)
#' }
#' @export
bb_example_sources <- function() {
    bind_rows(
        bb_source2(
            name="NOAA OI SST V2",
            id="oisst.v2",
            description="Weekly and monthly mean and long-term monthly mean SST data, 1-degree resolution, 1981 to present. Ice concentration data are also included, which are the ice concentration values input to the SST analysis",
            reference= "http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html",
            citation="NOAA_OI_SST_V2 data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their web site at http://www.esrl.noaa.gov/psd/",
            source_url=c("ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/*"),
            license="Please cite",
            method=quote(bb_handler_wget2),
            method_flags=list(recursive=TRUE,level=1,no_parent=TRUE),
            postprocess=NULL,
            access_function="readsst",
            collection_size=0.9,
            data_group="Sea surface temperature"),
        bb_source(
            name="CMEMS global gridded SSH reprocessed (1993-ongoing)",
            id="SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047",
            description="For the Global Ocean - Multimission altimeter satellite gridded sea surface heights and derived variables computed with respect to a twenty-year mean. Previously distributed by Aviso+, no change in the scientific content. All the missions are homogenized with respect to a reference mission which is currently OSTM/Jason-2.\nVARIABLES\n- sea_surface_height_above_sea_level (SSH)\n- surface_geostrophic_eastward_sea_water_velocity_assuming_sea_level_for_geoid (UVG)\n- surface_geostrophic_northward_sea_water_velocity_assuming_sea_level_for_geoid (UVG)\n- sea_surface_height_above_geoid (SSH)\n- surface_geostrophic_eastward_sea_water_velocity (UVG)\n- surface_geostrophic_northward_sea_water_velocity (UVG)",
            reference="http://cmems-resources.cls.fr/?option=com_csw&view=details&tab=info&product_id=SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047",
            citation="In case of any publication, the Licensee will ensure credit the Copernicus Marine Service in the following manner: \"This study has been conducted using E.U. Copernicus Marine Service Information\"",
            source_url=c("ftp://ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047/dataset-duacs-rep-global-merged-allsat-phy-l4-v3/"),
            license="See http://marine.copernicus.eu/services-portfolio/service-commitments-and-licence/",
            method=quote(bb_handler_wget),
            method_flags=c("--recursive","--level=3","--follow-ftp","--no-parent"),
            postprocess=quote(bb_gunzip),
            authentication_note="Copernicus Marine login required, see http://marine.copernicus.eu/services-portfolio/register-now/",
            user="",
            password="",
            access_function="readssh",
            collection_size=310,
            data_group="Altimetry",warn_empty_auth=FALSE),
        bb_source(
            name="Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a",
            id="SeaWiFS_L3m_MO_CHL_chlor_a_9km",
            description="Monthly remote-sensing chlorophyll-a from the SeaWiFS satellite at 9km spatial resolution",
            reference= "https://oceancolor.gsfc.nasa.gov/",
            citation="See https://oceancolor.gsfc.nasa.gov/citations",
            license="Please cite",
            method=quote(bb_handler_oceandata),
            method_flags=c("search=S*L3m_MO_CHL_chlor_a_9km.nc"),
            postprocess=NULL,
            collection_size=7.2,
            data_group="Ocean colour"),
        bb_source(
            name="Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 2",
            id="10.5067/EYICLBOAAJOU",
            description="NSIDC provides this data set to aid in the investigations of the variability and trends of sea ice cover. Ice cover in these data are indicated by sea ice concentration: the percentage of the ocean surface covered by ice. The ice-covered area indicates how much ice is present; it is the total area of a pixel multiplied by the ice concentration in that pixel. Ice persistence is the percentage of months over the data set time period that ice existed at a location. The ice-extent indicates whether ice is present; here, ice is considered to exist in a pixel if the sea ice concentration exceeds 15 percent. This data set provides users with data about total ice-covered areas, sea ice extent, ice persistence, and monthly climatologies of sea ice concentrations.",
            reference="https://nsidc.org/data/NSIDC-0192/versions/2",
            citation="Stroeve, J. and W. Meier. 2017. Sea Ice Trends and Climatologies from SMMR and SSM/I-SSMIS, Version 2. [Indicate subset used]. Boulder, Colorado USA. NASA National Snow and Ice Data Center Distributed Active Archive Center. doi: http://dx.doi.org/10.5067/EYICLBOAAJOU. [Date Accessed].",
            source_url=c("https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0192_seaice_trends_climo_v2/"),
            license="Please cite, see http://nsidc.org/about/use_copyright.html",
            authentication_note="Requires Earthdata login, see https://urs.earthdata.nasa.gov/. Note that you will also need to authorize the application 'nsidc-daacdata' (see 'My Applications' at https://urs.earthdata.nasa.gov/profile)",
            method=quote(bb_handler_earthdata),
            method_flags=c("--recursive","--level=4","--no-parent","--relative"),
            user="",
            password="",
            postprocess=NULL,
            collection_size=0.02,
            data_group="Sea ice",warn_empty_auth=FALSE),
        bb_source(
            name="Bathymetry of Lake Superior",
            id="greatlakes-superior-bathymetry",
            description="A draft version of the Lake Superior Bathymetry was compiled as a component of a NOAA project to rescue Great Lakes lake floor geological and geophysical data, and make it more accessible to the public. No time frame has been set for completing bathymetric contours of Lake Superior, though a 3 arc-second (~90 meter cell size) grid is available.",
            reference="https://www.ngdc.noaa.gov/mgg/greatlakes/superior.html",
            source_url=c("https://www.ngdc.noaa.gov/mgg/greatlakes/superior/data/"),
            citation="Publisher: DOC/NOAA/NESDIS/NCEI > National Centers for Environmental Information, NESDIS, NOAA, U.S. Department of Commerce",
            license="https://www.ngdc.noaa.gov/ngdcinfo/privacy.html#copyright",
            method=quote(bb_handler_wget),
            method_flags=c("--recursive","--level=2","--accept-regex=/netcdf/","--reject=index.html*"),
            comment="Only the netcdf format data are retrieved here - adjust the accept parameter in the method_flags to get other formats",
            postprocess=quote(bb_gunzip),
            collection_size=0.03,
            data_group="Topography"),
        bb_source2(
            name="Australian Election 2016 House of Representatives data",
            id="aus-election-house-2016",
            description="House of Representatives results from the 2016 Australian election.",
            reference="http://results.aec.gov.au/",
            citation="Copyright Commonwealth of Australia 2017. As far as practicable, material for which the copyright is owned by a third party will be clearly labelled. The AEC has made all reasonable efforts to ensure that this material has been reproduced on this website with the full consent of the copyright owners.",
            source_url=c("http://results.aec.gov.au/20499/Website/HouseDownloadsMenu-20499-Csv.htm"),
            license="CC-BY",
            method=quote(bb_handler_wget2),
            method_flags=list(recursive=TRUE,level=1,accept="csv",no_if_modified_since=TRUE),
            collection_size=0.01,
            data_group="Electoral")
    )
}
