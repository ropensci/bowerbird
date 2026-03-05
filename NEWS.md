# bowerbird 0.18.6

- `filter` parameter added to `bb_aadc_source()` so that a metadata record with multiple data sets attached to it can be reduced to just the data set(s) of actual interest, if needed
- a data source that obtains its files from S3 storage can now have multiple buckets in a single source

## Breaking changes

- `bb_data_source_dir()` can now return a vector of multiple paths (rather than a single string) for certain data sources
