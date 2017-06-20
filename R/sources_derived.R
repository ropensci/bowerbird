sources_derived_layers <- function() {
    bb_source(
        name="CSIRO Atlas of Regional Seas 2009",
        description="CARS is a digital climatology, or atlas of seasonal ocean water properties.",
        reference="http://www.marine.csiro.au/~dunn/cars2009/",
        citation="Ridgway K.R., J.R. Dunn, and J.L. Wilkin, Ocean interpolation by four-dimensional least squares -Application to the waters around Australia, J. Atmos. Ocean. Tech., Vol 19, No 9, 1357-1375, 2002",
        source_url="http://www.marine.csiro.au/atlas/",
        license="Please cite",
        method=bb_wget,
        method_flags="--recursive --level=1 --accept-regex=\".*2009.*.nc.gz\"",
        postprocess=pp_gunzip,
        data_group="Derived layers")
}
