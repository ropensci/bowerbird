sources_meteorological <- function() {
    bb_source(
        name="Antarctic Mesoscale Prediction System grib files",
        description="The Antarctic Mesoscale Prediction System - AMPS - is an experimental, real-time numerical weather prediction capability that provides support for the United States Antarctic Program, Antarctic science, and international Antarctic efforts.",
        reference="http://www2.mmm.ucar.edu/rt/amps/",
        citation="See http://www2.mmm.ucar.edu/rt/amps/",
        source_url="http://www2.mmm.ucar.edu/rt/amps/wrf_grib/",
        license="Please cite",
        comment="d1,d2 files for hours 000-027 only. Note that this web site provides only the last few days of files.",
        method=quote(amps_get),
        method_flags="",
        postprocess=NULL,
        collection_size=NA, ## depends on how long the sync has been running, since only the last few days worth are exposed at any one time
        data_group="Meteorological")
}
