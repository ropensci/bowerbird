sources_altimetry <- function() {
    bb_source(
        name="Ssalto/Duacs gridded absolute dynamic topography",
        description="Gridded sea surface heights above geoid from satellite altimetry (delayed and near-real-time products)",
        reference= "http://www.aviso.altimetry.fr/en/data/products/sea-surface-height-products/global/madt.html",
        citation="The altimeter products were produced by Ssalto/Duacs and distributed by Aviso, with support from Cnes (http://www.aviso.altimetry.fr/duacs/)",
        source_url=c("ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/madt/all-sat-merged/h/*","ftp://ftp.aviso.altimetry.fr/global/near-real-time/grids/madt/all-sat-merged/h/*"),
        license="See http://www.aviso.altimetry.fr/en/data/product-information/citation-and-aviso-products-licence.html",
        method=webget,
        method_flags="--recursive --level=inf --follow-ftp --no-parent",
        postprocess=pp_gunzip,
        user="valid_aviso_account_user_name_needed",
        password="valid_aviso_account_password_needed",
        access_function="readssh",
        data_group="Altimetry") %>%
        bind_rows(
            bb_source(
                name="Ssalto/Duacs gridded absolute geostrophic velocities",
                description="Absolute geostrophic velocities computed from gridded sea surface heights above geoid from satellite altimetry  (delayed and near-real-time products)",
                reference= "http://www.aviso.altimetry.fr/en/data/products/sea-surface-height-products/global/madt.html",
                citation="The altimeter products were produced by Ssalto/Duacs and distributed by Aviso, with support from Cnes (http://www.aviso.altimetry.fr/duacs/)",
                source_url=c("ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/madt/all-sat-merged/uv/*","ftp://ftp.aviso.altimetry.fr/global/near-real-time/grids/madt/all-sat-merged/uv/*"),
                license="See http://www.aviso.altimetry.fr/en/data/product-information/citation-and-aviso-products-licence.html",
                method=webget,
                method_flags="--recursive --level=inf --timestamping --follow-ftp --no-parent",
                postprocess=pp_gunzip,
                user="valid_aviso_account_user_name_needed",
                password="valid_aviso_account_password_needed",
                access_function="readcurr",
                data_group="Altimetry")
        ) %>%
        bind_rows(
            bb_source(
                name="Ssalto/Duacs gridded sea level anomalies",
                description="Gridded sea surface height anomalies from satellite altimetry  (delayed and near-real-time products)",
                reference= "http://www.aviso.altimetry.fr/en/data/products/sea-surface-height-products/global/msla.html",
                citation="The altimeter products were produced by Ssalto/Duacs and distributed by Aviso, with support from Cnes (http://www.aviso.altimetry.fr/duacs/)",
                source_url=c("ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/msla/all-sat-merged/h/*","ftp://ftp.aviso.altimetry.fr/global/near-real-time/grids/msla/all-sat-merged/h/*"),
                license="See http://www.aviso.altimetry.fr/en/data/product-information/citation-and-aviso-products-licence.html",
                method=webget,
                method_flags="--recursive --level=inf --follow-ftp --no-parent",
                postprocess=pp_gunzip,
                user="valid_aviso_account_user_name_needed",
                password="valid_aviso_account_password_needed",
                access_function="readssh",
                data_group="Altimetry")
        ) %>%
        bind_rows(
            bb_source(
                name="Ssalto/Duacs gridded mean and climatological sea level anomalies",
                description="Monthly mean and monthly climatologies of gridded sea surface height anomalies from satellite altimetry",
                reference= "http://www.aviso.altimetry.fr/en/data/products/sea-surface-height-products/global/msla-mean-climatology.html",
                citation="The altimeter products were produced by Ssalto/Duacs and distributed by Aviso, with support from Cnes (http://www.aviso.altimetry.fr/duacs/)",
                source_url="ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/climatology/*",
                license="See http://www.aviso.altimetry.fr/en/data/product-information/citation-and-aviso-products-licence.html",
                method=webget,
                method_flags="--recursive --level=inf --follow-ftp --no-parent",
                postprocess=pp_gunzip,
                user="valid_aviso_account_user_name_needed",
                password="valid_aviso_account_password_needed",
                access_function="readssh",
                data_group="Altimetry")
            )
}
