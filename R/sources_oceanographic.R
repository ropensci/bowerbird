sources_oceanographic <- function() {
    bb_source(
        name="CSIRO Atlas of Regional Seas 2009",
        description="CARS is a digital climatology, or atlas of seasonal ocean water properties.",
        reference="http://www.marine.csiro.au/~dunn/cars2009/",
        citation="Ridgway K.R., J.R. Dunn, and J.L. Wilkin, Ocean interpolation by four-dimensional least squares -Application to the waters around Australia, J. Atmos. Ocean. Tech., Vol 19, No 9, 1357-1375, 2002",
        source_url="http://www.marine.csiro.au/atlas/",
        license="Please cite",
        method=quote(bb_wget),
        method_flags="--recursive --level=1 --accept-regex=\".*2009.*.nc.gz\"",
        postprocess=quote(pp_gunzip),
        data_group="Oceanographic") %>%
        bind_rows(
            bb_source(
                name="World Ocean Atlas 2009",
                description="World Ocean Atlas 2009 (WOA09) is a set of objectively analyzed (1 degree grid) climatological fields of in situ temperature, salinity, dissolved oxygen, Apparent Oxygen Utilization (AOU), percent oxygen saturation, phosphate, silicate, and nitrate at standard depth levels for annual, seasonal, and monthly compositing periods for the World Ocean. It also includes associated statistical fields of observed oceanographic profile data interpolated to standard depth levels on both 1 degree and 5 degree grids",
                reference="http://www.nodc.noaa.gov/OC5/WOA09/pr_woa09.html",
                citation="Citation for WOA09 Temperature: Locarnini, R. A., A. V. Mishonov, J. I. Antonov, T. P. Boyer, and H. E. Garcia, 2010. World Ocean Atlas 2009, Volume 1: Temperature. S. Levitus, Ed. NOAA Atlas NESDIS 68, U.S. Government Printing Office, Washington, D.C., 184 pp.\nCitation for WOA09 Salinity: Antonov, J. I., D. Seidov, T. P. Boyer, R. A. Locarnini, A. V. Mishonov, and H. E. Garcia, 2010. World Ocean Atlas 2009, Volume 2: Salinity. S. Levitus, Ed. NOAA Atlas NESDIS 69, U.S. Government Printing Office, Washington, D.C., 184 pp.\nCitation for WOA09 Oxygen: Garcia, H. E., R. A. Locarnini, T. P. Boyer, and J. I. Antonov, 2010. World Ocean Atlas 2009, Volume 3: Dissolved Oxygen, Apparent Oxygen Utilization, and Oxygen Saturation. S. Levitus, Ed. NOAA Atlas NESDIS 70, U.S. Government Printing Office, Washington, D.C., 344 pp.\nCitation for WOA09 Nutrients: Garcia, H. E., R. A. Locarnini, T. P. Boyer, and J. I. Antonov, 2010. World Ocean Atlas 2009, Volume 4: Nutrients (phosphate, nitrate, silicate). S. Levitus, Ed. NOAA Atlas NESDIS 71, U.S. Government Printing Office, Washington, D.C., 398 pp.",
                license="Please cite",
                source_url="https://data.nodc.noaa.gov/woa/WOA09/NetCDFdata/",
                method=quote(bb_wget),
                method_flags="--recursive --no-parent -e robots=off",
                postprocess=NULL,
                data_group="Oceanographic")) %>%
        bind_rows(
            bb_source(
                name="World Ocean Atlas 2013 V2",
                description="World Ocean Atlas 2013 version 2 (WOA13 V2) is a set of objectively analyzed (1 degree grid) climatological fields of in situ temperature, salinity, dissolved oxygen, Apparent Oxygen Utilization (AOU), percent oxygen saturation, phosphate, silicate, and nitrate at standard depth levels for annual, seasonal, and monthly compositing periods for the World Ocean. It also includes associated statistical fields of observed oceanographic profile data interpolated to standard depth levels on 5 degree, 1 degree, and 0.25 degree grids",
                reference="https://www.nodc.noaa.gov/OC5/woa13/",
                citation="Citation for WOA13 Temperature:\nLocarnini, R. A., A. V. Mishonov, J. I. Antonov, T. P. Boyer, H. E. Garcia, O. K. Baranova, M. M. Zweng, C. R. Paver, J. R. Reagan, D. R. Johnson, M. Hamilton, and D. Seidov, 2013. World Ocean Atlas 2013, Volume 1: Temperature. S. Levitus, Ed., A. Mishonov Technical Ed.; NOAA Atlas NESDIS 73, 40 pp.\nCitation for WOA13 Salinity:\nZweng, M.M, J.R. Reagan, J.I. Antonov, R.A. Locarnini, A.V. Mishonov, T.P. Boyer, H.E. Garcia, O.K. Baranova, D.R. Johnson, D.Seidov, M.M. Biddle, 2013. World Ocean Atlas 2013, Volume 2: Salinity. S. Levitus, Ed., A. Mishonov Technical Ed.; NOAA Atlas NESDIS 74, 39 pp.\nCitation for WOA13 Oxygen:\nGarcia, H. E., R. A. Locarnini, T. P. Boyer, J. I. Antonov, O.K. Baranova, M.M. Zweng, J.R. Reagan, D.R. Johnson, 2014. World Ocean Atlas 2013, Volume 3: Dissolved Oxygen, Apparent Oxygen Utilization, and Oxygen Saturation. S. Levitus, Ed., A. Mishonov Technical Ed.; NOAA Atlas NESDIS 75, 27 pp.\nCitation for WOA13 Nutrients:\nGarcia, H. E., R. A. Locarnini, T. P. Boyer, J. I. Antonov, O.K. Baranova, M.M. Zweng, J.R. Reagan, D.R. Johnson, 2014. World Ocean Atlas 2013, Volume 4: Dissolved Inorganic Nutrients (phosphate, nitrate, silicate). S. Levitus, Ed., A. Mishonov Technical Ed.; NOAA Atlas NESDIS 76, 25 pp.",
                license="Please cite",
                source_url="https://data.nodc.noaa.gov/woa/WOA13/DATAv2/",
                method=quote(bb_wget),
                method_flags="--recursive --no-parent -e robots=off --reject=\"/ascii/*,/csv/*,/shape/*\"",
                postprocess=NULL,
                data_group="Oceanographic"))
}
