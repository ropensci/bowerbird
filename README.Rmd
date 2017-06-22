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

### Configuration

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
    postprocess=quote(pp_unzip))

cf <- bb_config(local_file_root="/your/data/directory") %>%
    add(my_source)
```

### Synchronisation

Once the configuration has been defined, run the sync process:

```{r,eval=FALSE}
bb_sync(cf)
```

Running the sync in parallel is likely to speed the process up considerably (unless your bandwidth is the limiting factor).

Note that a given data source may have several `source_url` values, each of which will have their own row in the configuration table (with the same data source `name` value). It is probably a good idea to avoid running these in within-data-source replicates in parallel, because they may overlap in terms of the parts of the remote site that they are mirroring. Thus, it's probably best to split the configuration up by data source `name` and run those subsets in parallel (untested code):

```{r,eval=FALSE}
library(doFuture)
registerDoFuture()
plan(multiprocess)

foreach (i=unique(cf$name),.export=c("cf")) %dopar% {bb_sync(cf[cf$name==i,])}
```
