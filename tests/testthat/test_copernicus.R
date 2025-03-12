context("copernicus handler")

test_that("bb_handler_copernicus works", {
    skip("skipping Copernicus test temporarily, not sure if the test dataset is broken or Copernicus changes have broken our code more generally")
    src <- bb_source(
        name = "Antarctic Sea Ice Extent from Reanalysis",
        id = "ANTARCTIC_OMI_SI_extent",
        description = "Estimates of Antarctic sea ice extent are obtained from the surface of oceans grid cells that have at least 15% sea ice concentration. These values are cumulated in the entire Southern Hemisphere (excluding ice lakes) and from 1993 up to real time aiming to: i) obtain the Antarctic sea ice extent as expressed in millions of km squared (106 km2) to monitor both the large-scale variability and mean state and change. ii) to monitor the change in sea ice extent as expressed in millions of km squared per decade (106 km2/decade), or in sea ice extent loss/gain since the beginning of the time series as expressed in percent per decade (%/decade; reference period being the first date of the key figure b) dot-dashed trend line, Vaughan et al., 2013))",
        doc_url = "https://data.marine.copernicus.eu/product/ANTARCTIC_OMI_SI_extent/description",
        citation = "In case of any publication, the Licensee will ensure credit the Copernicus Marine Service and cite the DOIs links guaranteeing the traceability of the scientific studies and experiments, in the following manner: \"This study has been conducted using E.U. Copernicus Marine Service Information; https://doi.org/10.48670/moi-00186\"",
        license = "See http://marine.copernicus.eu/services-portfolio/service-commitments-and-licence/",
        method = list("bb_handler_copernicus", product = "ANTARCTIC_OMI_SI_extent"),
        user = "",
        password = "",
        data_group = "Sea surface temperature", warn_empty_auth = FALSE)

    temp_root <- tempdir()
    cf <- bb_add(bb_config(local_file_root = temp_root), src)
    expect_true(grepl("data.marine.copernicus.eu/ANTARCTIC_OMI_SI_extent$", bb_data_source_dir(cf)))
    res <- bb_sync(cf, confirm_downloads_larger_than = NULL)
    expect_equal(nrow(res$files[[1]]), 1)
    expect_true(all(file.exists(res$files[[1]]$file)))
})
