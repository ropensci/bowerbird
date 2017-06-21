sources_altimetry <- function() {
    bb_source(
        name="Ssalto/Duacs gridded absolute dynamic topography",
        description="Gridded sea surface heights above geoid from satellite altimetry (delayed and near-real-time products)",
        reference="http://www.aviso.altimetry.fr/en/data/products/sea-surface-height-products/global/madt.html",
        citation="The altimeter products were produced by Ssalto/Duacs and distributed by Aviso, with support from Cnes (http://www.aviso.altimetry.fr/duacs/)",
        source_url=c("ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/madt/all-sat-merged/h/*","ftp://ftp.aviso.altimetry.fr/global/near-real-time/grids/madt/all-sat-merged/h/*"),
        license="See http://www.aviso.altimetry.fr/en/data/product-information/citation-and-aviso-products-licence.html",
        method=quote(bb_wget),
        method_flags="--recursive --level=inf --follow-ftp --no-parent",
        postprocess=quote(pp_gunzip),
        authentication_note="AVISO login required, see https://www.aviso.altimetry.fr/en/data/data-access/endatadata-accessregistration-form.html",
        user="",
        password="",
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
                method=quote(bb_wget),
                method_flags="--recursive --level=inf --timestamping --follow-ftp --no-parent",
                postprocess=quote(pp_gunzip),
                authentication_note="AVISO login required, see https://www.aviso.altimetry.fr/en/data/data-access/endatadata-accessregistration-form.html",
                user="",
                password="",
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
                method=quote(bb_wget),
                method_flags="--recursive --level=inf --follow-ftp --no-parent",
                postprocess=quote(pp_gunzip),
                authentication_note="AVISO login required, see https://www.aviso.altimetry.fr/en/data/data-access/endatadata-accessregistration-form.html",
                user="",
                password="",
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
                method=quote(bb_wget),
                method_flags="--recursive --level=inf --follow-ftp --no-parent",
                postprocess=quote(pp_gunzip),
                authentication_note="AVISO login required, see https://www.aviso.altimetry.fr/en/data/data-access/endatadata-accessregistration-form.html",
                user="",
                password="",
                access_function="readssh",
                data_group="Altimetry")
        ) %>%
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
                data_group="Altimetry"))
}
