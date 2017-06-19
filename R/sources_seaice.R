sources_seaice <- function() {
    bb_source(
        name="NSIDC SMMR-SSM/I Nasateam sea ice concentration",
        description="Passive-microwave estimates of sea ice concentration at 25km spatial resolution. Daily and monthly resolution, available from 1-Oct-1978 to present.",
        reference="http://nsidc.org/data/nsidc-0051.html",
        source_url="ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/",
        citation="Cavalieri, D. J., C. L. Parkinson, P. Gloersen, and H. Zwally. 1996, updated yearly. Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS Passive Microwave Data. [indicate subset used]. Boulder, Colorado USA: NASA National Snow and Ice Data Center Distributed Active Archive Center. http://dx.doi.org/10.5067/8GQ8LZQVL0VL",
        license="Please cite, see http://nsidc.org/about/use_copyright.html",
        comment="",
        method="webget",
        method_flags="--exclude-directories=pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/browse,pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north --recursive --level=inf",
        postprocess=NULL,
        access_function="readice",
        data_group="Sea ice") %>%
        bind_rows(
            bb_source(
                name="NSIDC SMMR-SSM/I Nasateam near-real-time sea ice concentration",
                description="Passive-microwave estimates of sea ice concentration at 25km, daily, near-real-time resolution.",
                reference="http://nsidc.org/data/nsidc-0081.html",
                citation="Maslanik, J. and J. Stroeve. 1999, updated daily. Near-Real-Time DMSP SSMIS Daily Polar Gridded Sea Ice Concentrations. [indicate subset used]. Boulder, Colorado USA: NASA National Snow and Ice Data Center Distributed Active Archive Center. http://dx.doi.org/10.5067/U8C09DWVX9LM",
                source_url="ftp://sidads.colorado.edu/pub/DATASETS/nsidc0081_nrt_nasateam_seaice/",
                license="Please cite, see http://nsidc.org/about/use_copyright.html",
                comment="",
                method="webget",
                method_flags="--exclude-directories=pub/DATASETS/nsidc0081_nrt_nasateam_seaice/browse,pub/DATASETS/nsidc0081_nrt_nasateam_seaice/north --recursive --level=inf",
                postprocess=NULL,
                access_function="readice",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="NSIDC passive microwave supporting files",
                description="Grids and other support files for NSIDC passive-microwave sea ice data.",
                reference="http://nsidc.org/data/nsidc-0051.html",
                citation="See the citation details of the particular sea ice dataset used",
                source_url="ftp://sidads.colorado.edu/pub/DATASETS/seaice/polar-stereo/",
                license="Please cite, see http://nsidc.org/about/use_copyright.html",
                comment="",
                method="webget",
                method_flags="--recursive --level=inf",
                postprocess=NULL,
                access_function="readice",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="NSIDC SMMR-SSM/I Nasateam sea ice time since melt",
                description="Time since sea ice melted calculated from NSIDC SMMR-SSM/I Nasateam sea ice concentration data",
                reference="",
                citation="",
                source_url="http://webdav.data.aad.gov.au/data/environmental/smmr_ssmi_nasateam/time_since_melt/",
                license="CC-BY",
                comment="",
                method="webget",
                method_flags="--recursive --level=inf --no-parent --reject=\"index.html\"",
                postprocess=NULL,
                access_function="",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="Nimbus Ice Edge Points from Nimbus Visible Imagery",
                description="This data set (NmIcEdg2) estimates the location of the North and South Pole sea ice edges at various times during the mid to late 1960s, based on recovered Nimbus 1 (1964), Nimbus 2 (1966), and Nimbus 3 (1969) visible imagery.",
                reference="http://nsidc.org/data/nmicedg2/",
                citation="Gallaher, D. and G. Campbell. 2014. Nimbus Ice Edge Points from Nimbus Visible Imagery L2, CSV. [indicate subset used]. Boulder, Colorado USA: NASA National Snow and Ice Data Center Distributed Active Archive Center. http://dx.doi.org/10.5067/NIMBUS/NmIcEdg2",
                source_url="ftp://n5eil01u.ecs.nsidc.org/SAN/NIMBUS/NmIcEdg2.001/",
                license="Please cite, see http://nsidc.org/about/use_copyright.html",
                comment="",
                method="webget",
                method_flags="--recursive --level=inf --no-parent",
                postprocess=NULL,
                access_function="",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR-E passive microwave sea ice concentration",
                description="Passive-microwave estimates of daily sea ice concentration at 6.25km spatial resolution, from 19-Jun-2002 to 2-Oct-2011.",
                reference="http://icdc.zmaw.de/1/daten/cryosphere/seaiceconcentration-asi-amsre.html",
                citation="Include the acknowledgement: \"ASI Algorithm AMSR-E sea ice concentration were obtained for [PERIOD] from the Integrated Climate Date Center (ICDC, http://icdc.zmaw,de/), University of Hamburg, Hamburg, Germany.\" Also please cite: Spreen, G., L. Kaleschke, and G. Heygster (2008), Sea ice remote sensing using AMSR-E 89 GHz channels, J. Geophys. Res. 113, C02S03, doi:10.1029/2005JC003384",
                source_url="ftp://ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/no_landmask/hdf/s6250/*",
                license="",
                comment="",
                method="webget",
                method_flags="--recursive --level=inf --follow-ftp",
                postprocess=pp_gunzip,
                access_function="readice",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR-E passive microwave supporting files",
                description="Grids and other support files for Artist AMSR-E passive-microwave sea ice data.",
                reference="http://icdc.zmaw.de/1/daten/cryosphere/seaiceconcentration-asi-amsre.html",
                citation="See the citation details of the particular sea ice dataset used",
                source_url=c("ftp://ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/Landmasks/","ftp://ftp-projects.zmaw.de/seaice/AMSR-E_ASI_IceConc/LonLatGrids/"),
                license="",
                comment="",
                method="webget",
                method_flags="--recursive --level=inf --follow-ftp",
                postprocess=NULL,
                access_function="readice",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR2 near-real-time passive microwave sea ice concentration",
                description="Near-real-time passive-microwave estimates of daily sea ice concentration at 6.25km spatial resolution, from 24-July-2012 to present.",
                reference="http://www.iup.uni-bremen.de:8084/amsr2/",
                citation="Spreen, G., L. Kaleschke, and G. Heygster (2008), Sea ice remote sensing using AMSR-E 89 GHz channels, J. Geophys. Res. 113, C02S03, doi:10.1029/2005JC003384",
                source_url="http://www.iup.uni-bremen.de:8084/amsr2data/asi_daygrid_swath/s6250/",
                license="",
                comment="--reject-regex=\"^/amsr2data/\" to prevent download from recursing into unwanted directories",
                method="webget",
                method_flags="--recursive --level=inf --accept=\"asi*.hdf\" --accept=\"asi*.png\" --accept=\"asi*.tif\" --reject-regex=\"^/amsr2data/\" --no-parent",
                postprocess=NULL,
                access_function="",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR2 regional passive microwave sea ice concentration",
                description="Passive-microwave estimates of daily sea ice concentration at 3.125km spatial resolution in selected regions, from 24-July-2012 to present.",
                reference="http://www.iup.uni-bremen.de:8084/amsr2/regions-amsr2.php",
                citation="Spreen, G., L. Kaleschke, and G. Heygster (2008), Sea ice remote sensing using AMSR-E 89 GHz channels, J. Geophys. Res. 113, C02S03, doi:10.1029/2005JC003384",
                source_url="http://www.iup.uni-bremen.de:8084/amsr2data/asi_daygrid_swath/s3125/",
                license="",
                comment="--reject-regex=\"^/amsr2data/\" to prevent download from recursing into unwanted directories",
                method="webget",
                method_flags="--recursive --level=inf --accept=\"asi*.hdf\" --accept=\"asi*.tif\" --accept=\"asi*.png\" --reject-regex=\"^/amsr2data/\" --no-parent",
                postprocess=NULL,
                access_function="",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="Artist AMSR2 passive microwave supporting files",
                description="Grids for Artist AMSR2 passive-microwave sea ice data.",
                reference="http://www.iup.uni-bremen.de:8084/amsr2/",
                citation="See the citation details of the particular sea ice dataset used",
                source_url="http://www.iup.uni-bremen.de:8084/amsredata/asi_daygrid_swath/l1a/s6250/grid_coordinates/LongitudeLatitudeGrid-s6250-Antarctic.hdf",
                license="",
                comment="",
                method="webget",
                method_flags="--recursive --level=inf --accept=hdf --no-parent",
                postprocess=NULL,
                access_function="",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="CERSAT SSM/I sea ice concentration",
                description="Passive microwave sea ice concentration data at 12.5km resolution, 3-Dec-1991 to present",
                reference= "http://cersat.ifremer.fr/data/tools-and-services/quicklooks/sea-ice/ssm-i-sea-ice-concentration-maps",
                citation="",
                source_url="ftp://ftp.ifremer.fr//ifremer/cersat/products/gridded/psi-concentration/data/antarctic/daily/netcdf/*",
                license="",
                comment="",
                method="webget",
                method_flags="--recursive --level=inf --no-parent",
                postprocess=pp_uncompress,
                access_function="readice",
                data_group="Sea ice")
        ) %>%
        bind_rows(
            bb_source(
                name="MODIS Composite Based Maps of East Antarctic Fast Ice Coverage",
                description="Maps of East Antarctic landfast sea-ice extent, generated from approx. 250,000 1 km visible/thermal infrared cloud-free MODIS composite imagery (augmented with AMSR-E 6.25-km sea-ice concentration composite imagery when required). Coverage from 2000-03-01 to 2008-12-31",
                reference= "http://data.aad.gov.au/aadc/metadata/metadata.cfm?entry_id=modis_20day_fast_ice",
                citation="Fraser, AD, RA Massom, KJ Michael, BK Galton-Fenzi, and JL Lieser (2012) East Antarctic landfast sea ice distribution and variability, 2000-08. Journal of Climate, 25(4):1137-1156",
                source_url="https://data.aad.gov.au/eds/file/3656",
                license="CC-BY",
                comment="",
                method="aadc_eds",
                method_flags="",
                postprocess=pp_unzip,
                access_function="",
                data_group="Sea ice")
        )
}
