#' Example bowerbird data sources
#'
#' Five example data sources:
#' \itemize{
#'   \item "NOAA OI SST V2"
#'   \item "CMEMS global gridded SSH reprocessed (1993-ongoing)" - a data source that requires a username and password
#'   \item "Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a" - an example data source that uses the \code{oceandata_get} method
#'   \item "Nimbus Ice Edge Points from Nimbus Visible Imagery" - an example data source that uses the \code{earthdata_get} method
#'   \item "Bathymetry of Lake Superior" - an example that passes extra flags to the \code{bb_wget} call in order to restrict what is downloaded
#' }
#' @references See the \code{reference} and \code{citation} field in each row of the returned tibble for references associated with these particular data sources
#'
#' @return data.frame
#'
#' @seealso \code{\link{bb_config}} \code{\link{bb_wget}} \code{\link{oceandata_get}} \code{\link{earthdata_get}}
#'
#' @examples
#' \dontrun{
#' cf <- bb_config("/my/file/root") %>%
#'   add(bb_example_sources()[5,])
#' bb_sync(cf)
#' }
#' @export
bb_example_sources <- function() {
    bind_rows(
        bb_source(
            name="NOAA OI SST V2",
            id="oisst.v2",
            description="Weekly and monthly mean and long-term monthly mean SST data, 1-degree resolution, 1981 to present. Ice concentration data are also included, which are the ice concentration values input to the SST analysis",
            reference= "http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html",
            citation="NOAA_OI_SST_V2 data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their web site at http://www.esrl.noaa.gov/psd/",
            source_url="ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/*",
            license="Please cite",
            method=quote(bb_wget),
            method_flags="--recursive --level=1 --no-parent",
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
            method=quote(bb_wget),
            method_flags="--recursive --level=3 --follow-ftp --no-parent",
            postprocess=quote(pp_gunzip),
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
            reference= "http://oceancolor.gsfc.nasa.gov/",
            citation="See https://oceancolor.gsfc.nasa.gov/citations",
            license="Please cite",
            method=quote(oceandata_get),
            method_flags="search=S*L3m_MO_CHL_chlor_a_9km.nc",
            postprocess=NULL,
            collection_size=7.2,
            data_group="Ocean colour"),
        bb_source(
            name="Nimbus Ice Edge Points from Nimbus Visible Imagery",
            id="10.5067/NIMBUS/NmIcEdg2",
            description="This data set (NmIcEdg2) estimates the location of the North and South Pole sea ice edges at various times during the mid to late 1960s, based on recovered Nimbus 1 (1964), Nimbus 2 (1966), and Nimbus 3 (1969) visible imagery.",
            reference="http://nsidc.org/data/nmicedg2/",
            citation="Gallaher, D. and G. Campbell. 2014. Nimbus Ice Edge Points from Nimbus Visible Imagery L2, CSV. [indicate subset used]. Boulder, Colorado USA: NASA National Snow and Ice Data Center Distributed Active Archive Center. http://dx.doi.org/10.5067/NIMBUS/NmIcEdg2",
            source_url="https://n5eil01u.ecs.nsidc.org/NIMBUS/NmIcEdg2.001/",
            license="Please cite, see http://nsidc.org/about/use_copyright.html",
            authentication_note="Requires Earthdata login, see https://urs.earthdata.nasa.gov/",
            method=quote(earthdata_get),
            method_flags="--recursive --level=2 --no-parent --accept-regex=\"/NmIcEdg2.001/\"",
            user="",
            password="",
            postprocess=NULL,
            collection_size=0.1,
            data_group="Sea ice",warn_empty_auth=FALSE),
        bb_source(
            name="Bathymetry of Lake Superior",
            id="greatlakes-superior-bathymetry",
            description="A draft version of the Lake Superior Bathymetry was compiled as a component of a NOAA project to rescue Great Lakes lake floor geological and geophysical data, and make it more accessible to the public. No time frame has been set for completing bathymetric contours of Lake Superior, though a 3 arc-second (~90 meter cell size) grid is available.",
            reference="https://www.ngdc.noaa.gov/mgg/greatlakes/superior.html",
            source_url="https://www.ngdc.noaa.gov/mgg/greatlakes/superior/data/",
            citation="Publisher: DOC/NOAA/NESDIS/NCEI > National Centers for Environmental Information, NESDIS, NOAA, U.S. Department of Commerce",
            license="https://www.ngdc.noaa.gov/ngdcinfo/privacy.html#copyright",
            method=quote(bb_wget),
            method_flags="--recursive --level=2 --accept-regex=\"/netcdf/\" --reject=\"index.html*\"",
            comment="Only the netcdf format data are retrieved here - adjust the accept parameter in the method_flags to get other formats",
            postprocess=quote(pp_gunzip),
            collection_size=0.03,
            data_group="Topography")
    )
}