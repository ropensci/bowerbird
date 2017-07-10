sources_ocean_colour <- function() {
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
        data_group="Ocean colour") %>%
        bind_rows(
            bb_source(
                name="Oceandata MODIS Aqua Level-3 mapped daily 4km chl-a",
                id="MODISA_L3m_DAY_CHL_chlor_a_4km",
                description="Daily remote-sensing chlorophyll-a from the MODIS Aqua satellite at 4km spatial resolution",
                reference="http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=A*L3m_DAY_CHL_chlor_a_4km.nc",
                postprocess=NULL,
                collection_size=40,                
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata MODIS Aqua Level-3 mapped monthly 9km chl-a",
                id="MODISA_L3m_MO_CHL_chlor_a_9km",
                description="Monthly remote-sensing chlorophyll-a from the MODIS Aqua satellite at 9km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=A*L3m_MO_CHL_chlor_a_9km.nc",
                postprocess=NULL,
                collection_size=8,                
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 mapped daily 4km chl-a",
                id="VIIRS_L3m_DAY_NPP_CHL_chlor_a_4km",
                description="Daily remote-sensing chlorophyll-a from the VIIRS satellite at 4km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V2016*L3m_DAY_NPP_CHL_chlor_a_4km.nc",
                postprocess=NULL,
                collection_size=1,                
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 mapped monthly 9km chl-a",
                id="VIIRS_L3m_MO_SNPP_CHL_chlor_a_9km",
                description="Monthly remote-sensing chlorophyll-a from the VIIRS satellite at 9km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V*L3m_MO_SNPP_CHL_chlor_a_9km.nc",
                postprocess=NULL,
                collection_size=1,                
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 mapped seasonal 9km chl-a",
                id="VIIRS_L3m_SNxx_SNPP_CHL_chlor_a_9km",
                description="Seasonal remote-sensing chlorophyll-a from the VIIRS satellite at 9km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V*L3m_SN*_SNPP_CHL_chlor_a_9km.nc",
                postprocess=NULL,
                collection_size=0.5,                
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 binned daily RRS",
                id="VIIRS_L3b_DAY_SNPP_RRS",
                description="Daily remote-sensing reflectance from VIIRS. RRS is used to produce standard ocean colour products such as chlorophyll concentration",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V*L3b_DAY_SNPP_RRS.nc",
                postprocess=NULL,
                access_function="roc::readL3",
                collection_size=180,                
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata MODIS Aqua Level-3 binned daily RRS",
                id="MODISA_L3b_DAY_RRS",
                description="Daily remote-sensing reflectance from MODIS Aqua. RRS is used to produce standard ocean colour products such as chlorophyll concentration",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=A*L3b_DAY_RRS.nc",
                postprocess=NULL,
                access_function="roc::readL3",
                collection_size=800,
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata SeaWiFS Level-3 binned daily RRS",
                id="SeaWiFS_L3b_DAY_RRS",
                description="Daily remote-sensing reflectance from SeaWiFS. RRS is used to produce standard ocean colour products such as chlorophyll concentration",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=S*L3b_DAY_RRS.nc",
                postprocess=NULL,
                access_function="roc::readL3",
                collection_size=130,
                data_group="Ocean colour")) %>%
        bind_rows(
            bb_source(
                name="Oceandata VIIRS Level-3 mapped 32-day 9km chl-a",
                id="VIIRS_L3m_R32_SNPP_CHL_chlor_a_9km",
                description="Rolling 32-day composite remote-sensing chlorophyll-a from the VIIRS satellite at 9km spatial resolution",
                reference= "http://oceancolor.gsfc.nasa.gov/",
                citation="See https://oceancolor.gsfc.nasa.gov/citations",
                license="Please cite",
                method=quote(oceandata_get),
                method_flags="search=V*L3m_R32_SNPP_CHL_chlor_a_9km.nc",
                postprocess=NULL,
                collection_size=4,
                data_group="Ocean colour"))
}
