sources_satellite_imagery <- function() {
    bb_source(
        name="NASA MODIS Rapid Response Antarctic Mosaic",
        description="Daily Antarctic MODIS mosaic images at 4km resolution",
        reference="https://earthdata.nasa.gov/data/near-real-time-data/rapid-response",
        citation="See https://earthdata.nasa.gov/earth-observation-data/citation-policies",
        source_url="http://lance-modis.eosdis.nasa.gov/imagery/subsets/",
        license="Please cite",
        method=quote(rapid_response_get),
        postprocess=NULL,
        access_function="readrapid_response",
        data_group="Satellite Imagery")
}
    
