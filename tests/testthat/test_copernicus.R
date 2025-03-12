context("copernicus handler")

test_that("bb_handler_copernicus works", {
    src <- bb_source(
        name = "Global Ocean, extreme and mean significant wave height trends from satellite observations - seasonal trends",
        id = "OMI_EXTREME_SEASTATE_GLOBAL_swh_mean_and_P95_obs",
        description = "Significant wave height (SWH), expressed in metres, is the average height of the highest one-third of waves. This OMI provides time series of seasonal mean and extreme SWH values in three oceanic regions as well as their trends from 2002 to 2020, computed from the reprocessed global L4 SWH product (WAVE_GLO_PHY_SWH_L4_MY_014_007). The extreme SWH is defined as the 95th percentile of the daily maximum of SWH over the chosen period and region. The 95th percentile represents the value below which 95% of the data points fall, indicating higher wave heights than usual. The mean and the 95th percentile of SWH are calculated for two seasons of the year to take into account the seasonal variability of waves (January, February, and March, and July, August, and September) and are in m while the trends are in cm/yr.",
        doc_url = "https://data.marine.copernicus.eu/product/OMI_EXTREME_SEASTATE_GLOBAL_swh_mean_and_P95_obs/description",
        citation = "In case of any publication, the Licensee will ensure credit the Copernicus Marine Service and cite the DOIs links guaranteeing the traceability of the scientific studies and experiments, in the following manner: \"This study has been conducted using E.U. Copernicus Marine Service Information; https://doi.org/10.48670/mds-00352\"",
        license = "See http://marine.copernicus.eu/services-portfolio/service-commitments-and-licence/",
        method = list("bb_handler_copernicus", product = "OMI_EXTREME_SEASTATE_GLOBAL_swh_mean_and_P95_obs"),
        user = "",
        password = "",
        warn_empty_auth = FALSE)

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    expect_true(grepl("data.marine.copernicus.eu/OMI_EXTREME_SEASTATE_GLOBAL_swh_mean_and_P95_obs$", bb_data_source_dir(cf)))
    res <- bb_sync(cf, confirm_downloads_larger_than = NULL)
    expect_equal(nrow(res$files[[1]]), 1)
    expect_true(all(file.exists(res$files[[1]]$file)))
})
