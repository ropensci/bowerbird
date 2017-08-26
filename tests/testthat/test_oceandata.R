context("oceandata handler")

test_that("oceandata_get works",{
    skip_on_appveyor() ## failing on AppVeyor for unknown reasons
    ods <- bb_source(
        id="bilbobaggins",
        name="Oceandata test",
        description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
        reference= "http://oceancolor.gsfc.nasa.gov/",
        citation="See http://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        comment="",
        method=oceandata_get,
        method_flags=c("search=T20000322000060.L3m_MO_SST_sst_9km.nc"),
        postprocess=NULL,
        access_function="",
        data_group="Sea surface temperature")
    temp_root <- tempdir()
    ocf <- add(bb_config(local_file_root="irrelevant_here"),ods)
    ## will be calling oceandata_get directly, not via bb_sync, so do some extra steps
    cwd <- getwd()
    setwd(temp_root)
    oceandata_get(ocf)
    fnm <- "oceandata.sci.gsfc.nasa.gov/MODIST/Mapped/Monthly/9km/SST/T20000322000060.L3m_MO_SST_sst_9km.nc" ## relative file name
    expect_true(file.exists(fnm))
    expect_true(file.exists(file.path(temp_root,fnm)))
    fi <- file.info(fnm)
    expect_gt(fi$size,6e6)
    setwd(cwd)
})

test_that("url mapper works", {
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021852007192.L3m_WC_SST_9.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/8D_Climatology/9km/SST/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030012003008.L3m_8D_KD490_Kd_490_9km.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/8Day/9km/Kd/2003/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090012009365.L3m_YR_FLH_ipar_9km.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Annual/9km/ipar/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021852010120.L3m_CU_CDOM_cdom_index_9km.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Cumulative/9km/cdom/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2007005.L3m_DAY_FLH_nflh_9km.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Daily/9km/nflh/2007/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20023052002334.L3m_MO_FLH_nflh_9km.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Monthly/9km/nflh/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021822006212.L3m_MC_SST4_9.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Monthly_Climatology/9km/SST4/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20110012011032.L3m_R32_RRS_aot_869_9km.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Rolling_32_Day/9km/aot/2011/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20040812004172.L3m_SNSP_PIC_pic_9km.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Seasonal/9km/pic/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030802011171.L3m_SCSP_RRS_angstrom_9km.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Seasonal_Climatology/9km/angstrom/")

    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_RRS.main.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2009/032/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_RRS.x06.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2009/032/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20140012014008.L3b_8D_PAR.main.bz2",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2014/001/")
    expect_identical(oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/")
    expect_identical(oceandata_url_mapper("https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc",path_only=TRUE),"oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/")
})
