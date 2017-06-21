sources_ocean_colour <- function() {
    bb_source(
        name="Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a",
        description="Monthly remote-sensing chlorophyll-a from the SeaWiFS satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See https://oceancolor.gsfc.nasa.gov/citations",
        license="Please cite",
        method=quote(oceandata_get),
        method_flags="search=S*L3m_MO_CHL_chlor_a_9km.nc",
        postprocess=NULL,
        data_group="Ocean colour") %>%
        bind_rows(
            bb_source(
                name="Oceandata MODIS Aqua Level-3 mapped daily 4km chl-a",
                description="Daily remote-sensing chlorophyll-a from the MODIS Aqua satellite at 4km spatial resolution",
                reference="http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=A*L3m_DAY_CHL_chlor_a_4km.nc",
                postprocess=NULL,
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata MODIS Aqua Level-3 mapped monthly 9km chl-a",
                description="Monthly remote-sensing chlorophyll-a from the MODIS Aqua satellite at 9km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=A*L3m_MO_CHL_chlor_a_9km.nc",
                postprocess=NULL,
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 mapped daily 4km chl-a",
                description="Daily remote-sensing chlorophyll-a from the VIIRS satellite at 4km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V2016*L3m_DAY_NPP_CHL_chlor_a_4km.nc",
                postprocess=NULL,
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 mapped monthly 9km chl-a",
                description="Monthly remote-sensing chlorophyll-a from the VIIRS satellite at 9km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V*L3m_MO_SNPP_CHL_chlor_a_9km.nc",
                postprocess=NULL,
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 mapped seasonal 9km chl-a",
                description="Seasonal remote-sensing chlorophyll-a from the VIIRS satellite at 9km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V*3m_SN*_SNPP_CHL_chlor_a_9km.nc",
                postprocess=NULL,
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 binned daily RRS",
                description="Daily remote-sensing reflectance from VIIRS. RRS is used to produce standard ocean colour products such as chlorophyll concentration",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V*L3b_DAY_SNPP_RRS.nc",
                postprocess=NULL,
                access_function="roc::readL3",
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata MODIS Aqua Level-3 binned daily RRS",
                description="Daily remote-sensing reflectance from MODIS Aqua. RRS is used to produce standard ocean colour products such as chlorophyll concentration",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=A*L3b_DAY_RRS.nc",
                postprocess=NULL,
                access_function="roc::readL3",
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata SeaWiFS Level-3 binned daily RRS",
                description="Daily remote-sensing reflectance from SeaWiFS. RRS is used to produce standard ocean colour products such as chlorophyll concentration",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=S*L3b_DAY_RRS.nc",
                postprocess=NULL,
                access_function="roc::readL3",
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 mapped 32-day 9km chl-a",
                description="Rolling 32-day composite remote-sensing chlorophyll-a from the VIIRS satellite at 9km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V*L3m_R32_SNPP_CHL_chlor_a_9km.nc",
                postprocess=NULL,
                data_group="Ocean colour"))
}

