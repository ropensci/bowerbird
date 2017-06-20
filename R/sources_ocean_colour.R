sources_ocean_colour <- function() {
    bb_source(
        name="Oceandata SeaWiFS Level-3 mapped monthly 9km chlorophyll-a",
        description="Monthly remote-sensing chlorophyll-a from the SeaWiFS satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See http://oceancolor.gsfc.nasa.gov/cms/citations",
        license="Please cite",
        method=oceandata_get,
        method_flags="search=S*L3m_MO_CHL_chlor_a_9km.nc",
        postprocess=NULL,
        data_group="Ocean colour")
}

