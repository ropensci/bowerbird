context("oceandata handler")

test_that("bb_handler_oceandata works",{
    skip("skipping oceandata test downloads, requires Earth data login")
    ods <- bb_source(
        id="bilbobaggins",
        name="Oceandata test",
        description="Monthly remote-sensing PAR from the MODIS Terra satellite at 9km spatial resolution",
        doc_url= "https://oceancolor.gsfc.nasa.gov/",
        citation="See https://oceancolor.gsfc.nasa.gov/cms/citations",
        source_url="",
        license="Please cite",
        user = "YOUR_EARTHDATA_USERNAME", password = "YOUR_EARTHDATA_PASSWORD",
        comment="",
        method=list("bb_handler_oceandata",search="TERRA_MODIS.20000301_20000331.L3m.MO.PAR.par.9km.nc"),
        postprocess=NULL,
        access_function="",
        data_group="Sea surface temperature")
    temp_root <- tempdir()
    ocf <- bb_add(bb_config(local_file_root=temp_root),ods)
    expect_true(grepl("oceandata.sci.gsfc.nasa.gov/MODIST/Mapped$",bb_data_source_dir(ocf)))
    bb_sync(ocf, confirm_downloads_larger_than = NULL)
    fnm <- "oceandata.sci.gsfc.nasa.gov/MODIST/Mapped/Monthly/9km/par/TERRA_MODIS.20000301_20000331.L3m.MO.PAR.par.9km.nc" ## relative file name
    expect_true(file.exists(file.path(temp_root,fnm)))
    fi <- file.info(file.path(temp_root,fnm))
    expect_gt(fi$size,6e6)
})

test_that("url mapper works", {
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021852007192.L3m_WC_SST_9.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/8D_Climatology/9km/SST/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030012003008.L3m_8D_KD490_Kd_490_9km.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/8Day/9km/Kd/2003/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090012009365.L3m_YR_FLH_ipar_9km.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Annual/9km/ipar/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021852010120.L3m_CU_CDOM_cdom_index_9km.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Cumulative/9km/cdom/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2007005.L3m_DAY_FLH_nflh_9km.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Daily/9km/nflh/2007/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20023052002334.L3m_MO_FLH_nflh_9km.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Monthly/9km/nflh/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20021822006212.L3m_MC_SST4_9.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Monthly_Climatology/9km/SST4/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20110012011032.L3m_R32_RRS_aot_869_9km.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Rolling_32_Day/9km/aot/2011/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20040812004172.L3m_SNSP_PIC_pic_9km.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Seasonal/9km/pic/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20030802011171.L3m_SCSP_RRS_angstrom_9km.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/Mapped/Seasonal_Climatology/9km/angstrom/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_RRS.main.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2009/032/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_RRS.x06.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2009/032/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20140012014008.L3b_8D_PAR.main.bz2", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2014/001/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_PAR_par_9km.nc", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/")
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_PAR_par_9km.nc", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/")
    ## new naming conventions
    expect_identical(bowerbird:::oceandata_url_mapper("https://oceandata.sci.gsfc.nasa.gov/ob/getfile/AQUA_MODIS.20230109.L3b.DAY.RRS.nc", path_only = TRUE), "oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN/2023/01/09/")

})

test_that("other oceandata handler tests", {
    ods <- bb_source(
        name="Oceandata MODIS Aqua Level-3 binned daily RRS",
        id="MODISA_L3b_DAY_RRS",
        description="Daily remote-sensing reflectance from MODIS Aqua. RRS is used to produce standard ocean colour products such as chlorophyll concentration",
        doc_url="http://oceancolor.gsfc.nasa.gov/",
        citation="See https://oceancolor.gsfc.nasa.gov/citations",
        license="Please cite",
        method=list("bb_handler_oceandata",search="AQUA_MODIS*L3b.DAY.RRS.nc", sensor = "aqua", dtype = "L3b"),
        user = "", password = "", warn_empty_auth = FALSE)
    temp_root <- tempdir()
    ocf <- bb_add(bb_config(local_file_root = temp_root), ods)
    expect_true(grepl("oceandata.sci.gsfc.nasa.gov/MODISA/L3BIN$", bb_data_source_dir(ocf)))

    ods <- bb_source(name="Oceandata VIIRS Level-3 binned daily RRS",
                     id="VIIRS_L3b_DAY_SNPP_RRS",
                     description="Daily remote-sensing reflectance from VIIRS. RRS is used to produce standard ocean colour products such as chlorophyll concentration",
                     doc_url="http://oceancolor.gsfc.nasa.gov/",
                     citation="See https://oceancolor.gsfc.nasa.gov/citations",
                     license="Please cite",
                     method=list("bb_handler_oceandata", search = "SNPP_VIIRS*L3b.DAY.RRS.nc", sensor = "viirs", dtype = "L3b"),
                     user = "", password = "", warn_empty_auth = FALSE)
    ocf <- bb_add(bb_config(local_file_root = temp_root), ods)
    expect_true(grepl("oceandata.sci.gsfc.nasa.gov/VIIRS/L3BIN$", bb_data_source_dir(ocf)))

    ods <- bb_source(name="Oceandata MODIS Aqua Level-3 mapped daily 4km chl-a",
                     id="MODISA_L3m_DAY_CHL_chlor_a_4km",
                     description="Daily remote-sensing chlorophyll-a from the MODIS Aqua satellite at 4km spatial resolution",
                     doc_url="http://oceancolor.gsfc.nasa.gov/",
                     citation="See https://oceancolor.gsfc.nasa.gov/citations",
                     license="Please cite",
                     method=list("bb_handler_oceandata",search="AQUA_MODIS*L3m.DAY.CHL.chlor_a.4km.nc", sensor = "aqua", dtype = "L3m"),
                     user = "", password = "", warn_empty_auth = FALSE)
    ocf <- bb_add(bb_config(local_file_root = temp_root), ods)
    expect_true(grepl("oceandata.sci.gsfc.nasa.gov/MODISA/Mapped$", bb_data_source_dir(ocf)))

    ods <- bb_source(name="Oceandata SeaWiFS Level-3 binned daily RRS",
                     id="SeaWiFS_L3b_DAY_RRS",
                     description="Daily remote-sensing reflectance from SeaWiFS. RRS is used to produce standard ocean colour products such as chlorophyll concentration",
                     doc_url="http://oceancolor.gsfc.nasa.gov/",
                     citation="See https://oceancolor.gsfc.nasa.gov/citations",
                     license="Please cite",
                     method=list("bb_handler_oceandata",search="SEASTAR_SEAWIFS_GAC*L3b.DAY.RRS.nc", sensor = "seawifs", dtype = "L3b"),
                     user = "", password = "", warn_empty_auth = FALSE)
    ocf <- bb_add(bb_config(local_file_root = temp_root), ods)
    expect_true(grepl("oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN$", bb_data_source_dir(ocf)))
})

test_that("bb_handler_oceandata works when no files match",{
    skip_on_appveyor() ## failing on AppVeyor for unknown reasons
    ods <- bb_source(id="bilbobaggins",
                     name="Oceandata test",
                     description="Monthly remote-sensing sea surface temperature from the MODIS Terra satellite at 9km spatial resolution",
                     doc_url= "https://oceancolor.gsfc.nasa.gov/",
                     citation="See https://oceancolor.gsfc.nasa.gov/cms/citations",
                     source_url="",
                     license="Please cite",
                     user = "YOUR_EARTHDATA_USERNAME", password = "YOUR_EARTHDATA_PASSWORD",
                     comment="",
                     method=list("bb_handler_oceandata",search="TERRA_MODISblahblahblah20000322000060.L3m_MO_SST_sst_9km.nc"),
                     postprocess=NULL,
                     access_function="",
                     data_group="Sea surface temperature")
    temp_root <- tempdir()
    ocf <- bb_add(bb_config(local_file_root=temp_root),ods)
    expect_error(bb_sync(ocf, confirm_downloads_larger_than = NULL, catch_errors = FALSE), "No files matched")
    expect_warning(chk <- bb_sync(ocf, confirm_downloads_larger_than = NULL), "No files matched")
    expect_false(chk$status)
})
