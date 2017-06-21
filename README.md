# Bowerbird

An R package for maintaining a local collection of sparkly data sets.

See [RAADTools](https://github.com/AustralianAntarcticDivision/raadtools) for reading, plotting, and manipulating these data.

## Installing

```{r,eval=FALSE}
install.packages("devtools")
library(devtools)
install_github("AustralianAntarcticDivision/bowerbird")
```

Bowerbird requires the third-party utility `wget` to recursively download files from data providers. `wget` is typically installed by default on Linux.
On Windows you can use the `install_wget()` function to install it. Otherwise download `wget` yourself (e.g. from https://eternallybored.org/misc/wget/current/wget.exe) and make sure it is on your path.

## Usage

Bowerbird must be configured to tell it which data sets to synchronise and where to save them on the local file system.

Build up a configuration by first defining global options such as the destination on your local file system:

```{r,eval=FALSE}
cf <- bb_config(local_file_root="/your/data/directory")
```

Add data sources by choosing from those already defined in bowerbird:

```{r,eval=FALSE}
library(magrittr)
cf <- cf %>% add(bb_sources("CERSAT SSM/I sea ice concentration"))
```

Or define your own data source:

```{r,eval=FALSE}
my_source <- bb_source(
    name="GSHHG coastline data",
    description="A Global Self-consistent, Hierarchical, High-resolution Geography Database",
    reference= "http://www.soest.hawaii.edu/pwessel/gshhg",
    citation="Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical,
      High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996",
    source_url="ftp://ftp.soest.hawaii.edu/gshhg/*",
    license="LGPL",
    comment="",
    method=quote(bb_wget),
    method_flags="--recursive --level=1 --accept=\"*bin*.zip,README.TXT\"",
    postprocess=pp_unzip)

cf <- bb_config(local_file_root="/your/data/directory") %>%
    add(my_source)
```

Once the configuration has been defined, run the sync process:

```{r,eval=FALSE}
bb_sync(cf)
```
