sources_sst <- function() {
    bb_source(
        name="NOAA OI 1/4 Degree Daily SST AVHRR",
        id="10.7289/V5SQ8XB5",
        description="Sea surface temperature at 0.25 degree daily resolution, from 1-Sep-1981 to present",
        reference= "http://www.ngdc.noaa.gov/docucomp/page?xml=NOAA/NESDIS/NCDC/Geoportal/iso/xml/C00844.xml&view=getDataView&header=none",
        citation="Richard W. Reynolds, Viva F. Banzon, and NOAA CDR Program (2008): NOAA Optimum Interpolation 1/4 Degree Daily Sea Surface Temperature (OISST) Analysis, Version 2. [indicate subset used]. NOAA National Climatic Data Center. doi:10.7289/V5SQ8XB5 [access date]",
        source_url="ftp://eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/*",
        license="Please cite",
        method=quote(bb_wget),
        method_flags="--recursive --level=inf --accept=\"avhrr-only*\" --reject=\"*preliminary*\" --follow-ftp --no-parent",
        postprocess=quote(pp_gunzip),
        access_function="readsst",
        collection_size=140,
        data_group="Sea surface temperature") %>%
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
                data_group="Sea surface temperature")
        ) %>%
        bind_rows(
            bb_source(
                name="NOAA Extended Reconstructed SST V3b",
                id="ersst",
                description="A global monthly SST analysis from 1854 to the present derived from ICOADS data with missing data filled in by statistical methods",
                reference="http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.ersst.html",
                citation="NOAA_ERSST_V3 data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their web site at http://www.esrl.noaa.gov/psd/",
                source_url="ftp://ftp.cdc.noaa.gov/Datasets/noaa.ersst/*",
                license="Please cite",
                method=quote(bb_wget),
                method_flags="--recursive --level=1 --no-parent",
                postprocess=NULL,
                collection_size=0.3,
                data_group="Sea surface temperature")
        ) %>%
        bind_rows(
            bb_source(
                name="Oceandata MODIS Terra Level-3 mapped monthly 9km SST",
                id="TERRA_L3m_MO_SST_sst_9km",
                description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=T*L3m_MO_SST_sst_9km.nc",
                postprocess=NULL,
                collection_size=7,
                data_group="Sea surface temperature")
        ) %>%
        bind_rows(
            bb_source(
                name="Oceandata MODIS Aqua Level-3 mapped monthly 9km SST",
                id="MODISA_L3m_MO_SST_sst_9km",
                description="Monthly remote-sensing SST from the MODIS Aqua satellite at 9km spatial resolution",
                reference="http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=A*L3m_MO_SST_sst_9km.nc",
                postprocess=NULL,
                collection_size=7,
                data_group="Sea surface temperature")
        ) %>%
        bind_rows(
            bb_source(
                name="GHRSST Level 4 MUR Global Foundation SST v4.1",
                id="GHRSST-MUR-SST_v4.1",
                description="A Group for High Resolution Sea Surface Temperature (GHRSST) Level 4 sea surface temperature analysis produced as a retrospective dataset (four day latency) at the JPL Physical Oceanography DAAC using wavelets as basis functions in an optimal interpolation approach on a global 0.011 degree grid. The version 4 Multiscale Ultrahigh Resolution (MUR) L4 analysis is based upon nighttime GHRSST L2P skin and subskin SST observations from several instruments including the NASA Advanced Microwave Scanning Radiometer-EOS (AMSRE), the Moderate Resolution Imaging Spectroradiometer (MODIS) on the NASA Aqua and Terra platforms, the US Navy microwave WindSat radiometer and in situ SST observations from the NOAA iQuam project. The ice concentration data are from the archives at the EUMETSAT Ocean and Sea Ice Satellite Application Facility (OSI SAF) High Latitude Processing Center and are also used for an improved SST parameterization for the high-latitudes. This data set is funded by the NASA MEaSUREs program (http://earthdata.nasa.gov/our-community/community-data-system-programs/measures-projects), and created by a team led by Dr. Toshio Chin from JPL.",
                reference="https://podaac.jpl.nasa.gov/Multi-scale_Ultra-high_Resolution_MUR-SST",
                citation="Cite as: US NASA; Jet Propulsion Laboratory; Physical Oceanography Distributed Active Archive Center (JPL PO.DAAC) (2002). GHRSST Level 4 MUR Global Foundation Sea Surface Temperature Analysis (v4.1) (GDS versions 1 and 2). National Oceanographic Data Center, NOAA. Dataset. [access date]",
                source_url="ftp://podaac-ftp.jpl.nasa.gov/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/",
                license="Please cite",
                comment="Note: this collection is large! You may wish to specify one or more source_url values with only particular years, e.g. ftp://podaac-ftp.jpl.nasa.gov/allData/ghrsst/data/GDS2/L4/GLOB/JPL/MUR/v4.1/2015/",
                method=quote(ghrsst_get),
                method_flags="--recursive --level=inf --no-parent",
                postprocess=quote(pp_bunzip2),
                collection_size=2000,
                data_group="Sea surface temperature")
        )
}
