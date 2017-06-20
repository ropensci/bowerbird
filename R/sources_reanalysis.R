sources_reanalysis <- function() {
    bb_source(
        name="NCEP-DOE Reanalysis 2 6-hourly data",
        description="NCEP-DOE Reanalysis 2 is an improved version of the NCEP Reanalysis I model that fixed errors and updated paramterizations of of physical processes. The 6-hourly data is the original output time resolution.",
        reference="http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis2.html",
        citation="NCEP_Reanalysis 2 data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their web site at http://www.esrl.noaa.gov/psd/",
        source_url="ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2/",
        license="Please cite",
        method=bb_wget,
        method_flags="--recursive --no-parent",
        postprocess=NULL,
        data_group="Reanalysis") %>%
        bind_rows(
            bb_source(
                name="NCEP-DOE Reanalysis 2 daily averages",
                description="NCEP-DOE Reanalysis 2 is an improved version of the NCEP Reanalysis I model that fixed errors and updated paramterizations of of physical processes. Daily averages are calculated from the 6-hourly model output.",
                reference="http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis2.html",
                citation="NCEP_Reanalysis 2 data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their web site at http://www.esrl.noaa.gov/psd/",
                source_url="ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2.dailyavgs/",
                license="Please cite",
                method=bb_wget,
                method_flags="--recursive --no-parent",
                postprocess=NULL,
                data_group="Reanalysis")) %>%
        bind_rows(
            bb_source(
                name="NCEP-DOE Reanalysis 2 monthly averages",
                description="NCEP-DOE Reanalysis 2 is an improved version of the NCEP Reanalysis I model that fixed errors and updated paramterizations of of physical processes. Monthly averages are calculated from the 6-hourly model output.",
                reference="http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis2.html",
                citation="NCEP_Reanalysis 2 data provided by the NOAA/OAR/ESRL PSD, Boulder, Colorado, USA, from their web site at http://www.esrl.noaa.gov/psd/",
                source_url="ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis2.derived/",
                license="Please cite",
                method=bb_wget,
                method_flags="--recursive --no-parent",
                postprocess=NULL,
                data_group="Reanalysis"))
}

