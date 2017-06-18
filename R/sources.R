#' Define an external data source
#'
#' @param name string: the name of the data source
#' @param description string: a description of the data source
#' @param reference string: URL to the metadata record or home page of the data source
#' @param source_urls character vector: one or more source URLs
#' @param citation string:
#' @param license string:
#' @param comment string:
#' @param method string:
#' @param method_flags string:
#' @param postprocess character vector:
#' @param access_function string:
#' @param data_group string:
#'
#' @return tibble
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#'
#' my_source <- bb_source(
#'    name="GSHHG coastline data",
#'    description="A Global Self-consistent, Hierarchical, High-resolution Geography Database",
#'    reference= "http://www.soest.hawaii.edu/pwessel/gshhg",
#'    citation="Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
#'      High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
#'    source_urls="ftp://ftp.soest.hawaii.edu/gshhg/*",
#'    license="",
#'    comment="",
#'    method="wget",
#'    method_flags="--recursive --level=1 --accept=\"*bin*.zip,README.TXT\"",
#'    postprocess="unzip")
#'
#' cf <- bb_config()
#' cf <- add(cf,my_source)
#'
#' @export
bb_source <- function(name,description="",reference,source_urls,citation,license,comment="",method="wget",method_flags="",postprocess="",access_function="",data_group="") {
    ## todo: allow method, postprocess to be passed as functions?
    if (missing(name))
        stop("A data source requires a name")
    if (missing(license) || missing(citation)) 
        stop("Please provide license and citation information for the data source, so that users properly acknowledge it")
    if (missing(reference))
        stop("Please provide a reference (a URL to the data source's metadata record or home page")
    if (missing(source_urls)) {
        if (method=="wget") stop("method 'wget' requires at least one source URL")
        ##warning("no source_urls provided")
        source_urls <- as.character(NA)
    }
    if (is.character(source_urls)) source_urls <- list(source_urls)
    if (is.character(postprocess)) postprocess <- list(postprocess)
    tibble(
        name=if (assert_that(is.string(name))) name,
        description=if (assert_that(is.string(description))) description,
        reference=if (assert_that(is.string(reference))) reference,
        source_urls=if (assert_that(is.list(source_urls))) source_urls,
        citation=if (assert_that(is.string(citation))) citation,
        license=if (assert_that(is.string(license))) license,
        comment=if (assert_that(is.string(comment))) comment,
        method=method,
        method_flags=if (assert_that(is.string(method_flags))) method_flags,
        postprocess=if (assert_that(is.list(postprocess))) postprocess,
        access_function=if (assert_that(is.string(access_function))) access_function,
        data_group=if (assert_that(is.string(data_group))) data_group)
}



#' Bowerbird configurations for various data sources
#'
#' @param name character vector: only return data sources with name matching these values
#' @param data_group character vector: only return data sources belonging to these data groups
#'
#' @references See \code{reference} and \code{citation} field in each row of the returned tibble
#'
#' @return tibble
#'
#' @seealso \code{\link{bb_config}}
#'
#' @examples
#' bb_sources()
#'
#' @export
bb_sources <- function(name,data_group) {
    out <- bb_source(
        name="NSIDC SMMR-SSM/I Nasateam sea ice concentration",
        description="Passive-microwave estimates of sea ice concentration at 25km spatial resolution. Daily and monthly resolution, available from 1-Oct-1978 to present.",
        reference="http://nsidc.org/data/nsidc-0051.html",
        source_urls="ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/",
        citation="Cavalieri, D. J., C. L. Parkinson, P. Gloersen, and H. Zwally. 1996, updated yearly. Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS Passive Microwave Data. [indicate subset used]. Boulder, Colorado USA: NASA National Snow and Ice Data Center Distributed Active Archive Center. http://dx.doi.org/10.5067/8GQ8LZQVL0VL",
        license="Please cite, see http://nsidc.org/about/use_copyright.html",
        comment="",
        method="wget",
        method_flags="--exclude-directories=pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/browse,pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north --recursive --level=inf",
        postprocess="",
        access_function="readice",
        data_group="Sea ice") %>%
        bind_rows(
            bb_source(
                name="NSIDC SMMR-SSM/I Nasateam near-real-time sea ice concentration",
                description="Passive-microwave estimates of sea ice concentration at 25km, daily, near-real-time resolution.",
                reference="http://nsidc.org/data/nsidc-0081.html",
                citation="Maslanik, J. and J. Stroeve. 1999, updated daily. Near-Real-Time DMSP SSMIS Daily Polar Gridded Sea Ice Concentrations. [indicate subset used]. Boulder, Colorado USA: NASA National Snow and Ice Data Center Distributed Active Archive Center. http://dx.doi.org/10.5067/U8C09DWVX9LM",
                source_urls="ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/",
                license="Please cite, see http://nsidc.org/about/use_copyright.html",
                comment="",
                method="wget",
                method_flags="--exclude-directories=pub/DATASETS/nsidc0081_nrt_nasateam_seaice/browse,pub/DATASETS/nsidc0081_nrt_nasateam_seaice/north --recursive --level=inf",
                postprocess="",
                access_function="readice",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="NSIDC passive microwave supporting files",
                description="Grids and other support files for NSIDC passive-microwave sea ice data.",
                reference="http://nsidc.org/data/nsidc-0051.html",
                citation="See the citation details of the particular sea ice dataset used",
                source_urls="ftp://sidads.colorado.edu/pub/DATASETS/seaice/polar-stereo/",
                license="Please cite, see http://nsidc.org/about/use_copyright.html",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf",
                postprocess="",
                access_function="readice",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="NSIDC SMMR-SSM/I Nasateam sea ice time since melt",
                description="Time since sea ice melted calculated from NSIDC SMMR-SSM/I Nasateam sea ice concentration data",
                reference="",
                citation="",
                source_urls="http://webdav.data.aad.gov.au/data/environmental/smmr_ssmi_nasateam/time_since_melt/",
                license="CC-BY",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf --no-parent --reject=\"index.html\"",
                postprocess="",
                access_function="",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source( 
                name="Nimbus Ice Edge Points from Nimbus Visible Imagery",
                description="This data set (NmIcEdg2) estimates the location of the North and South Pole sea ice edges at various times during the mid to late 1960s, based on recovered Nimbus 1 (1964), Nimbus 2 (1966), and Nimbus 3 (1969) visible imagery.",
                reference="http://nsidc.org/data/nmicedg2/",
                citation="Gallaher, D. and G. Campbell. 2014. Nimbus Ice Edge Points from Nimbus Visible Imagery L2, CSV. [indicate subset used]. Boulder, Colorado USA: NASA National Snow and Ice Data Center Distributed Active Archive Center. http://dx.doi.org/10.5067/NIMBUS/NmIcEdg2",
                source_urls="ftp://n5eil01u.ecs.nsidc.org/SAN/NIMBUS/NmIcEdg2.001/",
                license="Please cite, see http://nsidc.org/about/use_copyright.html",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf --no-parent",
                postprocess="",
                access_function="",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR-E passive microwave sea ice concentration",
                description="Passive-microwave estimates of daily sea ice concentration at 6.25km spatial resolution, from 19-Jun-2002 to 2-Oct-2011.",
                reference="http://icdc.zmaw.de/1/daten/cryosphere/seaiceconcentration-asi-amsre.html",
                citation="Include the acknowledgement: \"ASI Algorithm AMSR-E sea ice concentration were obtained for [PERIOD] from the Integrated Climate Date Center (ICDC, http://icdc.zmaw,de/), University of Hamburg, Hamburg, Germany.\" Also please cite: Spreen, G., L. Kaleschke, and G. Heygster (2008), Sea ice remote sensing using AMSR-E 89 GHz channels, J. Geophys. Res. 113, C02S03, doi:10.1029/2005JC003384",
                source_urls="ftp://ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/*",
                license="",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf --follow-ftp",
                postprocess="gunzip",
                access_function="readice",
                data_group="Sea ice")        
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR-E passive microwave supporting files",
                description="Grids and other support files for Artist AMSR-E passive-microwave sea ice data.",
                reference="http://icdc.zmaw.de/1/daten/cryosphere/seaiceconcentration-asi-amsre.html",
                citation="See the citation details of the particular sea ice dataset used",
                source_urls=c("ftp://ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/Landmasks/","ftp://ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/LonLatGrids/"),
                license="",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf --follow-ftp",
                postprocess="",
                access_function="readice",
                data_group="Sea ice")        
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR2 near-real-time passive microwave sea ice concentration",
                description="Near-real-time passive-microwave estimates of daily sea ice concentration at 6.25km spatial resolution, from 24-July-2012 to present.",
                reference="http://www.iup.uni-bremen.de:8084/amsr2/",
                citation="Spreen, G., L. Kaleschke, and G. Heygster (2008), Sea ice remote sensing using AMSR-E 89 GHz channels, J. Geophys. Res. 113, C02S03, doi:10.1029/2005JC003384",
                source_urls="http://www.iup.uni-bremen.de:8084/amsr2data/asi_daygrid_swath/s6250/",
                license="",
                comment="--reject-regex=\"^/amsr2data/\" to prevent download from recursing into unwanted directories",
                method="wget",
                method_flags="--recursive --level=inf --accept=\"asi*.hdf\" --accept=\"asi*.png\" --accept=\"asi*.tif\" --reject-regex=\"^/amsr2data/\" --no-parent",
                postprocess="",
                access_function="",
                data_group="Sea ice")        
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR2 regional passive microwave sea ice concentration",
                description="Passive-microwave estimates of daily sea ice concentration at 3.125km spatial resolution in selected regions, from 24-July-2012 to present.",
                reference="http://www.iup.uni-bremen.de:8084/amsr2/regions-amsr2.php",
                citation="Spreen, G., L. Kaleschke, and G. Heygster (2008), Sea ice remote sensing using AMSR-E 89 GHz channels, J. Geophys. Res. 113, C02S03, doi:10.1029/2005JC003384",
                source_urls="http://www.iup.uni-bremen.de:8084/amsr2data/asi_daygrid_swath/s3125/",
                license="",
                comment="--reject-regex=\"^/amsr2data/\" to prevent download from recursing into unwanted directories",
                method="wget",
                method_flags="--recursive --level=inf --accept=\"asi*.hdf\" --accept=\"asi*.tif\" --accept=\"asi*.png\" --reject-regex=\"^/amsr2data/\" --no-parent",
                postprocess="",
                access_function="",
                data_group="Sea ice")        
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR2 passive microwave supporting files",
                description="Grids for Artist AMSR2 passive-microwave sea ice data.",
                reference="http://www.iup.uni-bremen.de:8084/amsr2/",
                citation="See the citation details of the particular sea ice dataset used",
                source_urls="http://www.iup.uni-bremen.de:8084/amsredata/asi_daygrid_swath/l1a/s6250/grid_coordinates/LongitudeLatitudeGrid-s6250-Antarctic.hdf",
                license="",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf --accept=hdf --no-parent",
                postprocess="",
                access_function="",
                data_group="Sea ice")        
        ) %>%
        bind_rows(
            bb_source(
                name="CERSAT SSM/I sea ice concentration",
                description="Passive microwave sea ice concentration data at 12.5km resolution, 3-Dec-1991 to present",
                reference= "http://cersat.ifremer.fr/data/tools-and-services/quicklooks/sea-ice/ssm-i-sea-ice-concentration-maps",
                citation="",
                source_urls="ftp://ftp.ifremer.fr//ifremer/cersat/products/gridded/psi-concentration/data/antarctic/daily/netcdf/*",
                license="",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf --no-parent",
                postprocess="uncompress",
                access_function="readice",
                data_group="Sea ice")        
        ) %>%
        bind_rows(
            bb_source(
                name="MODIS Composite Based Maps of East Antarctic Fast Ice Coverage",
                description="Maps of East Antarctic landfast sea-ice extent, generated from approx. 250,000 1 km visible/thermal infrared cloud-free MODIS composite imagery (augmented with AMSR-E 6.25-km sea-ice concentration composite imagery when required). Coverage from 2000-03-01 to 2008-12-31",
                reference= "http://data.aad.gov.au/aadc/metadata/metadata.cfm?entry_id=modis_20day_fast_ice",
                citation="Fraser, AD, RA Massom, KJ Michael, BK Galton-Fenzi, and JL Lieser (2012) East Antarctic landfast sea ice distribution and variability, 2000-08. Journal of Climate, 25(4):1137-1156",
                source_urls="https://data.aad.gov.au/eds/file/3656",
                license="CC-BY",
                comment="",
                method="aadc_eds",
                method_flags="",
                postprocess="unzip",
                access_function="",
                data_group="Sea ice")        
        ) %>%
        bind_rows(
            bb_source(
                name="Smith and Sandwell bathymetry",
                description="Global seafloor topography from satellite altimetry and ship depth soundings",
                reference= "http://topex.ucsd.edu/WWW_html/mar_topo.html",
                citation="Smith, W. H. F., and D. T. Sandwell, Global seafloor topography from satellite altimetry and ship depth soundings, Science, v. 277, p. 1957-1962, 26 Sept., 1997",
                source_urls="ftp://topex.ucsd.edu/pub/global_topo_1min/*",
                license="See ftp://topex.ucsd.edu/pub/global_topo_1min/COPYRIGHT.txt",
                comment="",
                method="wget",
                method_flags="--recursive --level=1 --no-parent",
                postprocess="",
                access_function="readbathy",
                data_group="Topography")        
        ) %>%
        bind_rows(
            bb_source(
                name="ETOPO1",
                description="ETOPO1 is a 1 arc-minute global relief model of Earth's surface that integrates land topography and ocean bathymetry.",
                reference="http://www.ngdc.noaa.gov/mgg/global/global.html",
                citation="Amante, C. and B.W. Eakins, 2009. ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24. National Geophysical Data Center, NOAA. doi:10.7289/V5C8276M [access date]",
                license="",
                source_urls="http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/data/ice_surface/grid_registered/netcdf/",
                method="wget",
                method_flags="--recursive --no-parent --accept=\"*gdal*,*.txt\"",
                comment="",
                postprocess="gunzip",
                access_function="readtopo",
                data_group="Topography")        
        ) %>%
        bind_rows(
            bb_source(
                name="ETOPO2",
                description="2-Minute Gridded Global Relief Data (ETOPO2v2c)",
                reference="http://www.ngdc.noaa.gov/mgg/global/etopo2.html",
                citation="",
                license="",
                source_urls=c("http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO2/ETOPO2v2-2006/ETOPO2v2c/netCDF/ETOPO2v2c_f4_netCDF.zip","http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO2/ETOPO2v2-2006/ETOPO2v2c/ETOPO2v2c_ReadMe.txt"),
                method="wget",
                method_flags="--recursive --no-parent",
                comment="",
                postprocess="unzip",
                access_function="readtopo",
                data_group="Topography")
        ) %>%
        bind_rows(
            bb_source(
                name="Bedmap2",
                description="Bedmap2 is a suite of gridded products describing surface elevation, ice-thickness and the sea floor and subglacial bed elevation of the Antarctic south of 60S.",
                reference="http://www.antarctica.ac.uk/bas_research/our_research/az/bedmap2/",
                citation="Fretwell et al. (2013) Bedmap2: improved ice bed, surface and thickness datasets for Antarctica. The Cryosphere 7:375-393. doi:10.5194/tc-7-375-2013",
                license="Please cite",
                source_urls="https://secure.antarctica.ac.uk/data/bedmap2/",
                method="wget",
                method_flags="--recursive --no-parent -e robots=off --accept=\"*bin.zip,*tiff.zip,*.txt,*.rtf\" --no-check-certificate",
                comment="--no-check-certificate flag to wget until certificate authority issue fixed",
                postprocess="unzip",
                access_function="",
                data_group="Topography")        
        ) %>%
        bind_rows(
            bb_source(
                name="Kerguelen Plateau Bathymetric Grid 2010",
                description="This data replaces the digital elevation model (DEM) for the Kerguelen Plateau region produced in 2005 (Sexton 2005). The revised grid has been gridded at a grid pixel resolution of 0.001-arc degree (about 100 m). The new grid utilised the latest data sourced from ship-based multibeam and singlebeam echosounder surveys, and satellite remotely-sensed data. Report Reference: Beaman, R.J. and O'Brien, P.E., 2011. Kerguelen Plateau bathymetric grid, November 2010. Geoscience Australia, Record, 2011/22, 18 pages.",
                reference="http://www.ga.gov.au/metadata-gateway/metadata/record/gcat_71552",
                citation="Beaman, R.J. & O'Brien, P., 2011. Kerguelen Plateau Bathymetric Grid, November 2010. Record  2011/022. Geoscience Australia, Canberra",
                source_urls="http://ftt.jcu.edu.au/deepreef/kergdem/gmt/kerg_dem_gmt.zip",
                license="CC-BY",
                comment="",
                method="wget",
                method_flags="--recursive --no-parent",
                postprocess="unzip",
                access_function="",
                data_group="Topography")        
        ) %>%
        bind_rows(
            bb_source(
                name="GVDEM2008",
                description="This dataset comprises Digital Elevation Models (DEMs) of varying resolutions for the George V and Terre Adelie continental margin, derived by incorporating all available singlebeam and multibeam point depth data.",
                reference="http://data.aad.gov.au/aadc/metadata/metadata.cfm?entry_id=GVdem_2008",
                citation="Beaman, Robin (2009, updated 2015) A bathymetric Digital Elevation Model (DEM) of the George V and Terre Adelie continental shelf and margin Australian Antarctic Data Centre - CAASM Metadata (https://data.aad.gov.au/aadc/metadata/metadata_redirect.cfm?md=/AMD/AU/GVdem_2008)",
                license="CC-BY",
                source_urls="https://data.aad.gov.au/eds/file/4494",
                method="aadc_eds",
                method_flags="",
                comment="",
                postprocess="unzip",
                access_function="",
                data_group="Topography")        
        ) %>%
        bind_rows(
            bb_source(
                name="Geoscience Australia XYZ multibeam bathymetric grids of the Macquarie Ridge",
                description="This is a compilation of all the processed multibeam bathymetry data that are publicly available in Geoscience Australia's data holding for the Macquarie Ridge.",
                reference="http://www.ga.gov.au/metadata-gateway/metadata/record/gcat_b9224f95-a416-07f8-e044-00144fdd4fa6/XYZ+multibeam+bathymetric+grids+of+the+Macquarie+Ridge",
                citation="Spinoccia, M., 2012. XYZ multibeam bathymetric grids of the Macquarie Ridge. Geoscience Australia, Canberra.",
                source_urls="http://www.ga.gov.au/corporate_data/73697/Macquarie_ESRI_Raster.zip",
                license="CC BY 4.0",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf --accept=zip --no-parent",
                postprocess="unzip",
                access_function="",
                data_group="Topography")  
        ) %>%
        bind_rows(
            bb_source(
                name="The International Bathymetric Chart of the Southern Ocean (IBCSO) - digital bathymetric model",
                description="",
                reference="http://www.ibcso.org/",
                citation="Arndt, J.E., H. W. Schenke, M. Jakobsson, F. Nitsche, G. Buys, B. Goleby, M. Rebesco, F. Bohoyo, J.K. Hong, J. Black, R. Greku, G. Udintsev, F. Barrios, W. Reynoso-Peralta, T. Morishita, R. Wigley, The International Bathymetric Chart of the Southern Ocean (IBCSO) Version 1.0 - A new bathymetric compilation covering circum-Antarctic waters, 2013, Geophysical Research Letters, Vol. 40, p. 3111-3117, doi: 10.1002/grl.50413",
                license="CC-BY",
                source_urls=c("http://hs.pangaea.de/Maps/bathy/IBCSO_v1/IBCSO_v1_bed_PS71_500m_grd.zip","http://hs.pangaea.de/Maps/bathy/IBCSO_v1/IBCSO_v1_is_PS71_500m_grd.zip","http://hs.pangaea.de/Maps/bathy/IBCSO_v1/IBCSO_v1_sid_PS71_500m_grd.zip","http://hs.pangaea.de/Maps/bathy/IBCSO_v1/IBCSO_v1_is_PS71_500m_tif.zip","http://www.ibcso.org/data/IBCSO_background_hq.zip"),
                method="wget",
                method_flags="--recursive --no-parent",
                postprocess="unzip",
                access_function="",
                data_group="Topography")  
        ) %>%
        bind_rows(
            bb_source(
                name="The International Bathymetric Chart of the Southern Ocean (IBCSO) - digital chart for printing",
                description="",
                reference="http://www.ibcso.org/",
                citation="Arndt, J.E., H. W. Schenke, M. Jakobsson, F. Nitsche, G. Buys, B. Goleby, M. Rebesco, F. Bohoyo, J.K. Hong, J. Black, R. Greku, G. Udintsev, F. Barrios, W. Reynoso-Peralta, T. Morishita, R. Wigley, The International Bathymetric Chart of the Southern Ocean (IBCSO) Version 1.0 - A new bathymetric compilation covering circum-Antarctic waters, 2013, Geophysical Research Letters, Vol. 40, p. 3111-3117, doi: 10.1002/grl.50413",
                license="CC-BY",
                source_urls="http://hs.pangaea.de/Maps/bathy/IBCSO_v1/IBCSO_v1_digital_chart_pdfA.pdf",
                method="wget",
                method_flags="--recursive --no-parent",
                postprocess="",
                access_function="",
                data_group="Topography"            )        
        ) %>%
        bind_rows(
            bb_source(
                name="RTOPO-1: A consistent dataset for Antarctic ice shelf topography and global ocean bathymetry",
                description="Sub-ice shelf circulation and freezing/melting rates in ocean general circulation models depend critically on an accurate and consistent representation of cavity geometry. The goal of this work is to compile independent regional fields into a global data set. We use the S-2004 global 1-minute bathymetry as the backbone and add an improved version of the BEDMAP topography for an area that roughly coincides with the Antarctic continental shelf. Locations of the merging line have been carefully adjusted in order to get the best out of each data set. High-resolution gridded data for upper and lower ice surface topography and cavity geometry of the Amery, Fimbul, Filchner-Ronne, Larsen C and George VI Ice Shelves, and for Pine Island Glacier have been carefully merged into the ambient ice and ocean topographies. Multibeam survey data for bathymetry in the former Larsen B cavity and the southeastern Bellingshausen Sea have been obtained from the data centers of Alfred Wegener Institute (AWI), British Antarctic Survey (BAS) and Lamont-Doherty Earth Observatory (LDEO), gridded, and again carefully merged into the existing bathymetry map.",
                reference="http://epic.awi.de/30738/",
                citation="Timmermann, Ralph; Le Brocq, Anne M; Deen, Tara J; Domack, Eugene W; Dutrieux, Pierre; Galton-Fenzi, Ben; Hellmer, Hartmut H; Humbert, Angelika; Jansen, Daniela; Jenkins, Adrian; Lambrecht, Astrid; Makinson, Keith; Niederjasper, Fred; Nitsche, Frank-Oliver; N\uf8st, Ole Anders; Smedsrud, Lars Henrik; Smith, Walter (2010): A consistent dataset of Antarctic ice sheet topography, cavity geometry, and global bathymetry. Earth System Science Data, 2(2), 261-273, doi:10.5194/essd-2-261-2010",
                license="CC-BY",
                source_urls=c("http://store.pangaea.de/Publications/TimmermannR_et_al_2010/RTopo105b_data.nc","http://store.pangaea.de/Publications/TimmermannR_et_al_2010/RTopo105b_aux.nc","http://store.pangaea.de/Publications/TimmermannR_et_al_2010/RTopo105b_50S.nc","http://store.pangaea.de/Publications/TimmermannR_et_al_2010/RTopo105_coast.asc","http://store.pangaea.de/Publications/TimmermannR_et_al_2010/RTopo105_gl.asc","http://store.pangaea.de/Publications/TimmermannR_et_al_2010/RTopo105_bathy.jpg","http://store.pangaea.de/Publications/TimmermannR_et_al_2010/RTopo105_draft.jpg","http://store.pangaea.de/Publications/TimmermannR_et_al_2010/RTopo105_height.jpg","http://store.pangaea.de/Publications/TimmermannR_et_al_2010/RTopo105_famask.jpg"),
                method="wget",
                method_flags="--recursive --no-parent",
                postprocess="",
                access_function="",
                data_group="Topography")        
        ) %>%
        bind_rows(
            bb_source(
                name="Radarsat Antarctic Mapping Project Digital Elevation Model Version 2",
                description="The high-resolution Radarsat Antarctic Mapping Project (RAMP) digital elevation model (DEM) combines topographic data from a variety of sources to provide consistent coverage of all of Antarctica. Version 2 improves upon the original version by incorporating new topographic data, error corrections, extended coverage, and other modifications.",
                reference="http://nsidc.org/data/nsidc-0082",
                citation="Liu, H., K. Jezek, B. Li, and Z. Zhao. 2001. Radarsat Antarctic Mapping Project Digital Elevation Model Version 2. [indicate subset used]. Boulder, Colorado USA: National Snow and Ice Data Center. http://dx.doi.org/10.5067/PXKC81A7WAXD",
                license="Please cite",
                source_urls=c("ftp://sidads.colorado.edu/pub/DATASETS/nsidc0082_radarsat_dem_v02/200M/BINARY/*","ftp://sidads.colorado.edu/pub/DATASETS/nsidc0082_radarsat_dem_v02/1KM/BINARY/*","ftp://sidads.colorado.edu/pub/DATASETS/nsidc0082_radarsat_dem_v02/00README_v2.txt"),
                method="wget",
                method_flags="--recursive --no-parent --reject=\"*.txt.gz,*.tar.gz\"",
                comment="Only the 200m and 1km binary files are retrieved here: adjust the source_urls or method_flags for others",
                postprocess="gunzip",
                access_function="",
                data_group="Topography")
        ) %>%
        bind_rows(
            bb_source(
                name="New Zealand Regional Bathymetry (2016)",
                description="The NZ 250m gridded bathymetric data set and imagery, Mitchell et al. 2012, released 2016.",
                reference="https://www.niwa.co.nz/our-science/oceans/bathymetry/further-information",
                citation="Mitchell, J.S., Mackay, K.A., Neil, H.L., Mackay, E.J., Pallentin, A., Notman P., 2012. Undersea New Zealand, 1:5,000,000. NIWA Chart, Miscellaneous Series No. 92",
                source_urls="ftp://ftp.niwa.co.nz/bathymetry/NZBathy_DTM_2016_binary_grid.zip",
                license="Please cite",
                comment="",
                method="wget",
                method_flags="--recursive --level=inf --no-parent",
                postprocess="",
                access_function="",
                data_group="Topography"            )        
        ) %>%
        bind_rows(
            bb_source(
                name="Cryosat-2 Digital Elevation Model",
                description="A New Digital Elevation Model of Antarctica derived from 6 years of continuous CryoSat-2 measurements",
                reference="http://homepages.see.leeds.ac.uk/~py10ts/cpom_cryosat2_antarctic_dem/",
                citation="Slater T, Shepherd A, McMillan M, Muir A, Gilbert L, Hogg A (2017) A New Digital Elevation Model of Antarctica derived from 6 years of continuous CryoSat-2 measurements: Technical Report. http://homepages.see.leeds.ac.uk/~py10ts/cpom_cryosat2_antarctic_dem/",
                license="Please cite",
                source_urls="http://homepages.see.leeds.ac.uk/~py10ts/cpom_cryosat2_antarctic_dem/",
                method="wget",
                method_flags="--recursive --no-parent --reject=\"index.html\"",
                postprocess="",
                access_function="",
                data_group="Topography"            )        
        ) %>%
        bind_rows(
            bb_source(
                name="Natural Earth 10m physical vector data",
                description="Natural Earth is a public domain map dataset available at 1:10m, 1:50m, and 1:110 million scales.",
                reference="http://www.naturalearthdata.com/downloads/10m-physical-vectors/",
                citation="No permission is needed to use Natural Earth. Crediting the authors is unnecessary. However, if you wish to cite the map data, simply use one of the following. Short text: Made with Natural Earth. Long text: Made with Natural Earth. Free vector and raster map data @ naturalearthdata.com.",
                license="PD-CC",
                source_urls="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/10m_physical.zip",
                method="wget",
                method_flags="--recursive --no-parent",
                postprocess="unzip",
                access_function="",
                data_group="Topography"            )        
        ) %>%
        bind_rows(
            bb_source(
                name="GSHHG coastline data",
                description="A Global Self-consistent, Hierarchical, High-resolution Geography Database",
                reference= "http://www.soest.hawaii.edu/pwessel/gshhg",
                citation="Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical, High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
                source_urls="ftp://ftp.soest.hawaii.edu/gshhg/*",
                license="",
                comment="",
                method="wget",
                method_flags="--recursive --level=1 --accept=\"*bin*.zip,README.TXT\"",
                postprocess="unzip",
                access_function="",
                data_group="Topography"            )
        ) ##%>%
        ##bind_rows(
        ##    bb_source(
        ##    )        
        ##) %>%
    
    if (!missing(name)) out <- out[out$name %in% name,]
    if (!missing(data_group)) out <- out[out$data_group %in% data_group,]
    out
}
