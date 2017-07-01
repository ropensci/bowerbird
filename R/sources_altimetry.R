sources_altimetry <- function() {
    bb_source(
        name="CMEMS global gridded SSH reprocessed (1993-ongoing)",
        description="CMEMS PRODUCT IDENTIFIER: SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047.\nFor the Global Ocean - Multimission altimeter satellite gridded sea surface heights and derived variables computed with respect to a twenty-year mean. Previously distributed by Aviso+, no change in the scientific content. All the missions are homogenized with respect to a reference mission which is currently OSTM/Jason-2.\nVARIABLES\n- sea_surface_height_above_sea_level (SSH)\n- surface_geostrophic_eastward_sea_water_velocity_assuming_sea_level_for_geoid (UVG)\n- surface_geostrophic_northward_sea_water_velocity_assuming_sea_level_for_geoid (UVG)\n- sea_surface_height_above_geoid (SSH)\n- surface_geostrophic_eastward_sea_water_velocity (UVG)\n- surface_geostrophic_northward_sea_water_velocity (UVG)",
        reference="http://cmems-resources.cls.fr/?option=com_csw&view=details&tab=info&product_id=SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047",
        citation="In case of any publication, the Licensee will ensure credit the Copernicus Marine Service in the following manner: \"This study has been conducted using E.U. Copernicus Marine Service Information\"",
        source_url=c("ftp://ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047/dataset-duacs-rep-global-merged-allsat-phy-l4-v3/"),
        license="See http://marine.copernicus.eu/services-portfolio/service-commitments-and-licence/",
        method=quote(bb_wget),
        method_flags="--recursive --level=inf --follow-ftp --no-parent",
        postprocess=quote(pp_gunzip),
        authentication_note="Copernicus Marine login required, see http://marine.copernicus.eu/services-portfolio/register-now/",
        user="",
        password="",
        access_function="readssh",
        collection_size=310,        
        data_group="Altimetry",warn_empty_auth=FALSE) %>%
        bind_rows(
            bb_source(
                name="CMEMS global gridded SSH near-real-time",
                description="CMEMS PRODUCT IDENTIFIER: SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046.\nFor the Global Ocean - Multimission altimeter satellite gridded sea surface heights and derived variables computed with respect to a twenty-year mean. Previously distributed by Aviso+, no change in the scientific content. All the missions are homogenized with respect to a reference mission which is currently Jason-3. The acquisition of various altimeter data is a few days at most.\nVARIABLES\n- sea_surface_height_above_sea_level (SSH)\n- surface_geostrophic_eastward_sea_water_velocity_assuming_sea_level_for_geoid (UVG)\n- surface_geostrophic_northward_sea_water_velocity_assuming_sea_level_for_geoid (UVG)\n- sea_surface_height_above_geoid (SSH)\n- surface_geostrophic_eastward_sea_water_velocity (UVG)\n- surface_geostrophic_northward_sea_water_velocity (UVG)",
                reference="http://cmems-resources.cls.fr/?option=com_csw&view=details&tab=info&product_id=SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046",
                citation="In case of any publication, the Licensee will ensure credit the Copernicus Marine Service in the following manner: \"This study has been conducted using E.U. Copernicus Marine Service Information\"",
                source_url=c("ftp://ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046/dataset-duacs-nrt-global-merged-allsat-phy-l4-v3/"),
                license="See http://marine.copernicus.eu/services-portfolio/service-commitments-and-licence/",
                method=quote(bb_wget),
                method_flags="--recursive --level=inf --follow-ftp --no-parent",
                postprocess=quote(pp_gunzip),
                authentication_note="Copernicus Marine login required, see http://marine.copernicus.eu/services-portfolio/register-now/",
                user="",
                password="",
                access_function="readssh",
                collection_size=3,                
                data_group="Altimetry",warn_empty_auth=FALSE)) %>%
##        bind_rows(
##            bb_source(
##                name="Ssalto/Duacs gridded mean and climatological sea level anomalies",
##                description="Monthly mean and monthly climatologies of gridded sea surface height anomalies from satellite altimetry",
##                reference= "http://www.aviso.altimetry.fr/en/data/products/sea-surface-height-products/global/msla-mean-climatology.html",
##                citation="The altimeter products were produced by Ssalto/Duacs and distributed by Aviso, with support from Cnes (http://www.aviso.altimetry.fr/duacs/)",
##                source_url="ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/climatology/*",
##                license="See http://www.aviso.altimetry.fr/en/data/product-information/citation-and-aviso-products-licence.html",
##                method=quote(bb_wget),
##                method_flags="--recursive --level=inf --follow-ftp --no-parent",
##                postprocess=quote(pp_gunzip),
##                authentication_note="AVISO login required, see https://www.aviso.altimetry.fr/en/data/data-access/endatadata-accessregistration-form.html",
##                user="",
##                password="",
##                access_function="readssh",
##                data_group="Altimetry",warn_empty_auth=FALSE)
##        ) %>%
        bind_rows(
            bb_source(
                name="CNES-CLS09 Mean Dynamic Topography",
                description="CNES-CLS09 Mean Dynamic Topography (v1.1 release)",
                reference= "http://www.aviso.altimetry.fr/en/data/products/auxiliary-products/mdt.html",
                citation= "Rio, M-H, P. Schaeffer, G. Moreaux, J-M Lemoine, E. Bronner (2009) : A new Mean Dynamic Topography computed over the global ocean from GRACE data, altimetry and in-situ measurements . Poster communication at OceanObs09 symposium, 21-25 September 2009, Venice.",
                source_url="ftp://ftp.aviso.altimetry.fr/auxiliary/mdt/",
                license="See http://www.aviso.altimetry.fr/en/data/product-information/citation-and-aviso-products-licence.html",
                method=quote(bb_wget),
                method_flags="--recursive --level=inf --follow-ftp --no-parent",
                postprocess=NULL,
                authentication_note="AVISO login required, see https://www.aviso.altimetry.fr/en/data/data-access/endatadata-accessregistration-form.html",
                user="",
                password="",
                access_function="raster",
                collection_size=0.1,                
                data_group="Altimetry",warn_empty_auth=FALSE))
}
