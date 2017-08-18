
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/AustralianAntarcticDivision/bowerbird.svg?branch=master)](https://travis-ci.org/AustralianAntarcticDivision/bowerbird) [![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/5idrimyx0uuv6liu?svg=true)](https://ci.appveyor.com/project/raymondben/bowerbird) [![codecov](https://codecov.io/gh/AustralianAntarcticDivision/bowerbird/branch/master/graph/badge.svg)](https://codecov.io/gh/AustralianAntarcticDivision/bowerbird)

Bowerbird
=========

Often it's desirable to have local copies of third-party data sets. Fetching data on the fly from remote sources can be a great strategy, but for speed or other reasons it may be necessary to have local copies. This is particularly common in ecology and the environmental sciences. Bowerbird is an R package for maintaining a local collection of data sets from a range of data providers.

Installing
----------

``` r
install.packages("devtools")
library(devtools)
install_github("AustralianAntarcticDivision/bowerbird",build_vignettes=TRUE)
```

Bowerbird uses the third-party utility `wget` to do the heavy lifting of recursively downloading files from data providers. `wget` is typically installed by default on Linux. On Windows you can use the `install_wget()` function to install it. Otherwise download `wget` yourself (e.g. from <https://eternallybored.org/misc/wget/current/wget.exe>) and make sure it is on your path.

Usage
-----

### Configuration

Bowerbird must be configured to tell it which data sets to synchronise and where to save them on the local file system.

Build up a configuration by first defining global options such as the destination on your local file system:

``` r
cf <- bb_config(local_file_root="~/your/data/directory")
```

Add data sources by choosing from those already defined in Bowerbird, or define your own (see below for guidance). Bowerbird comes with configurations for a range of data sources, mostly environmental (marine and Antarctic) in nature. A summary of the pre-configured sources is given at the end of this document.

``` r
cf <- cf %>% add(bb_sources("CERSAT SSM/I sea ice concentration"))
```

### Synchronisation

Once the configuration has been defined, run the sync process:

``` r
bb_sync(cf)
```

Congratulations! You now have your own local copy of your chosen data sets.

The pre-packaged environmental data sources can be read, manipulated, and plotted using a range of other R packages, including [RAADTools](https://github.com/AustralianAntarcticDivision/raadtools) and [raster](https://cran.r-project.org/package=raster).

Nuances
-------

### Choosing a data directory

It's up to you where you want your data collection kept, and to provide that location to bowerbird. A common use case for bowerbird is maintaining a central data collection for multiple users, in which case that location is likely to be some sort of networked file share. However, if you are keeping a collection for your own use, you might like to look at <https://github.com/r-lib/rappdirs> to help find a suitable directory location.

### Modifying data sources

#### Authentication

Some data providers require users to log in. These are indicated by the `authentication_note` column in the configuration table. For these sources, you will need to provide your user name and password, e.g.:

``` r
src <- bb_sources(name="CMEMS global gridded SSH reprocessed (1993-ongoing)")
src$user <- "yourusername"
src$password <- "yourpassword"
cf <- add(cf,src)

## or, using dplyr
cf <- cf %>% add(bb_sources(name="CMEMS global gridded SSH reprocessed (1993-ongoing)") %>%
                 mutate(user="yourusername",password="yourpassword"))
```

#### Reducing download sizes

Sometimes you might only want part of a pre-configured data source. If the data source uses the `bb_wget` method, you can restrict what is downloaded by modifying the data source's `method_flags`, particularly the `--accept`, `--reject`, `--accept-regex`, and `--reject-regex` options. Be sure to leave the original method flags in place, unless you know what you are doing.

For example, the CERSAT SSM/I sea ice concentration data are arranged in yearly directories, so it is fairly easy to restrict ourselves to, say, only the 2017 data:

``` r
cf <- cf %>% add(bb_sources("CERSAT SSM/I sea ice concentration") %>%
                 mutate(method_flags=paste(method_flags,"--accept-regex=\"/2017/\"")))
```

See the notes below for further guidances on the accept/reject flags.

Alternatively, for data sources that are divided into subdirectories, one could replace the whole-data-source `source_url` with one or more that point to specific yearly (or other) subdirectories. For example, the default `source_url` for the CERSAT sea ice data above is "<ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/psi-concentration/data/antarctic/daily/netcdf/*>" (with yearly subdirectories). So e.g. for 2016 and 2017 data we could do:

``` r
cf <- cf %>% add(bb_sources("CERSAT SSM/I sea ice concentration") %>%
    mutate(source_url=c("ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/psi-concentration/data/antarctic/daily/netcdf/2016/*",
                        "ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/psi-concentration/data/antarctic/daily/netcdf/2017/*")))
```

### Defining new data sources

If the pre-packages data sources don't cover your needs, you can define your own using the `bb_source` function:

``` r
my_source <- bb_source(
    name="Geoscience Australia multibeam bathymetric grids of the Macquarie Ridge",
    id="10.4225/25/53D9B12E0F96E",
    description="This is a compilation of all the processed multibeam bathymetry data that are publicly available in Geoscience Australia's data holding for the Macquarie Ridge.",
    reference="http://www.ga.gov.au/metadata-gateway/metadata/record/gcat_b9224f95-a416-07f8-e044-00144fdd4fa6/XYZ+multibeam+bathymetric+grids+of+the+Macquarie+Ridge",
    citation="Spinoccia, M., 2012. XYZ multibeam bathymetric grids of the Macquarie Ridge. Geoscience Australia, Canberra.",
    source_url="http://www.ga.gov.au/corporate_data/73697/Macquarie_ESRI_Raster.zip",
    license="CC-BY 4.0",
    method=quote(bb_wget),
    method_flags="--recursive --level=inf --accept=zip --no-parent",
    postprocess=quote(pp_unzip),
    collection_size=0.4,
    data_group="Topography")

cf <- bb_config(local_file_root="/your/data/directory") %>%
    add(my_source)
```

Some particularly important components of this definition are:

1.  The `id` uniquely identifies the data source. If the data source has a DOI, use that. Otherwise, if the original data provider has an identifier for this dataset, that is probably a good choice here (include the data version number if there is one). The `id` should be something that changes when the data set is updated. A DOI is ideal for this. The `name` entry should be a human-readable but still concise name for the data set.

2.  The `license` and `citation` are important so that users know what conditions govern the usage of the data, and the appropriate citation to use to acknowledge the data providers. The `reference` entry should refer to a metadata or documentation page that describes the data in detail.

3.  The `method`, `method_flags`, and `source_url` define how this data will be retrieved. Most sources will use the `bb_wget` method, which is a wrapper around the wget utility. If you are unfamiliar with wget, consult one of the many online tutorials. You can also see the in-built wget help by running `wget("--help")`.

Some subtleties to bear in mind:

1.  If the data source delivers compressed files, you will most likely want to decompress them after downloading. The postprocess options `pp_decompress`, `pp_unzip`, etc will do this for you. By default, these *do not* delete the compressed files after decompressing. The reason for this is so that on the next synchronisation run, the local (compressed) copy can be compared to the remote compressed copy, and the download can be skipped if nothing has changed. Deleting local compressed files will save space on your file system, but may result in every file being re-downloaded on every synchronisation run.

2.  You will almost certainly want to specify `--recursive` as part of the `method_flags`. The synchronisation process saves files relative to the `local_file_root` directory specified in the call to `bb_config`. If `--recursive` is specified, then wget creates a directory structure that follows the URL structure. For example, calling `wget --recursive http://www.somewhere.org/monkey/banana/dataset.zip` will save the local file `www.somewhere.org/monkey/banana/dataset.zip`. Thus, specifying `--recursive` will keep data files from different sources naturally separated into their own directories. Without this flag, you are likely to get all downloaded files saved into your `local_file_root`.

3.  If you want to include/exclude certain files from being downloaded, use the `--accept`, `--reject`, `--accept-regex`, and `--reject-regex` flags. Note that `--accept` and `--reject` apply to file names (not the full path), and can be comma-separated lists of file name suffixes or patterns. The `--accept-regex` and `--reject-regex` flags apply to the full path but can only be a single regular expression each.

4.  Remember that any `wget_global_flags` defined via `bb_config` will be applied to every data source in addition to their specific `method_flags`. Any data source using `bb_wget` but without `method_flags` (i.e. NA) will use the `wget_default_flags` if any were defined via `bb_config`. Sources needing no flags should specify an empty string for `method_flags`.

5.  Several wget flags are set by the `bb_wget` function itself. The `--user` and `--password` flags are populated with any values supplied to the `user` and `password` parameters of the source. Similarly, the `clobber` parameter supplied to `bb_config` controls the overwrite behaviour: if `clobber` is 0 then the `--no-clobber` flags is added to each wget call; if `clobber` is 1 then the `--timestamping` flag is added.

#### Defining new data source methods

Some data sources can't be retrieved only using simple `wget` calls, and so the `method` for such data sources will need to be something more elaborate than `bb_wget`. Notes will be added here about defining new methods functions, but in the meantime look at e.g. `aadc_eds_get`, `oceandata_get`, or `earthdata_get`.

### Parallelized sync

Running the sync in parallel is likely to speed the process up considerably (unless your bandwidth is the limiting factor).

Note that a given data source may have several `source_url` values, in which case that data source will be expanded to multiple rows in the configuration object with one `source_url` per row (but all with the same data source `name` value). It is probably prudent to avoid running these in within-data-source replicates in parallel, because they might overlap in terms of the parts of the remote site that they are mirroring. Thus, it's probably best to split the configuration up by data source `name` and run those subsets in parallel, perhaps with (untested code):

``` r
library(doFuture)
registerDoFuture()
plan(multiprocess)

foreach (i=unique(cf$name),.export=c("cf")) %dopar% {bb_sync(cf[cf$name==i,])}
```

### Data provenance and reproducible research

An aspect of reproducible research is knowing which data were used to perform an analysis, and potentially archiving those data to an appropriate repository. Bowerbird can assist with this: see `vignette("data_provenance")`.

Data source summary
-------------------

These are the data source definitions that are provided as part of the bowerbird package.

### Data group: Altimetry

#### CMEMS global gridded SSH reprocessed (1993-ongoing)

For the Global Ocean - Multimission altimeter satellite gridded sea surface heights and derived variables computed with respect to a twenty-year mean. Previously distributed by Aviso+, no change in the scientific content. All the missions are homogenized with respect to a reference mission which is currently OSTM/Jason-2. VARIABLES

-   sea\_surface\_height\_above\_sea\_level (SSH)

-   surface\_geostrophic\_eastward\_sea\_water\_velocity\_assuming\_sea\_level\_for\_geoid (UVG)

-   surface\_geostrophic\_northward\_sea\_water\_velocity\_assuming\_sea\_level\_for\_geoid (UVG)

-   sea\_surface\_height\_above\_geoid (SSH)

-   surface\_geostrophic\_eastward\_sea\_water\_velocity (UVG)

-   surface\_geostrophic\_northward\_sea\_water\_velocity (UVG)

Authentication note: Copernicus Marine login required, see <http://marine.copernicus.eu/services-portfolio/register-now/>

Approximate size: 310 GB

Reference: <http://cmems-resources.cls.fr/?option=com_csw&view=details&tab=info&product_id=SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047>

### Data group: Ocean colour

#### Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a

Monthly remote-sensing chlorophyll-a from the SeaWiFS satellite at 9km spatial resolution

Approximate size: 7.2 GB

Reference: <http://oceancolor.gsfc.nasa.gov/>

### Data group: Sea ice

#### Nimbus Ice Edge Points from Nimbus Visible Imagery

This data set (NmIcEdg2) estimates the location of the North and South Pole sea ice edges at various times during the mid to late 1960s, based on recovered Nimbus 1 (1964), Nimbus 2 (1966), and Nimbus 3 (1969) visible imagery.

Authentication note: Requires Earthdata login, see <https://urs.earthdata.nasa.gov/>

Approximate size: 0.1 GB

Reference: <http://nsidc.org/data/nmicedg2/>

### Data group: Sea surface temperature

#### NOAA OI SST V2

Weekly and monthly mean and long-term monthly mean SST data, 1-degree resolution, 1981 to present. Ice concentration data are also included, which are the ice concentration values input to the SST analysis

Approximate size: 0.9 GB

Reference: <http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html>

### Data group: Topography

#### Bathymetry of Lake Superior

A draft version of the Lake Superior Bathymetry was compiled as a component of a NOAA project to rescue Great Lakes lake floor geological and geophysical data, and make it more accessible to the public. No time frame has been set for completing bathymetric contours of Lake Superior, though a 3 arc-second (~90 meter cell size) grid is available.

Approximate size: 0.03 GB

Reference: <https://www.ngdc.noaa.gov/mgg/greatlakes/superior.html>
