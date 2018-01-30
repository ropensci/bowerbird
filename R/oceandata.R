#' Handler for Oceandata data sets
#'
#' This is a handler function to be used with data sets from NASA's Oceandata system. This function is not intended to be called directly, but rather is specified as a \code{postprocess} option in \code{\link{bb_source}}.
#'
#' Oceandata uses standardized file naming conventions (see https://oceancolor.gsfc.nasa.gov/docs/format/), so once you know which products you want you can construct a suitable file name pattern to search for. For example, "S*L3m_MO_CHL_chlor_a_9km.nc" would match monthly level-3 mapped chlorophyll data from the SeaWiFS satellite at 9km resolution, in netcdf format. This pattern is passed as the \code{search} argument. Note that the \code{bb_handler_oceandata} does not take need `source_url` to be specified in the \code{bb_source} call.
#'
#' @references https://oceandata.sci.gsfc.nasa.gov/
#' @param search string: (required) the search string to pass to the oceancolor file searcher (https://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi)
#' @param dtype string: (optional) the data type (e.g. "L3m") to pass to the oceancolor file searcher (https://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi)
#' @param ... : extra parameters passed automatically by \code{bb_sync}
#'
#' @return TRUE on success
#'
#' @examples
#'
#' my_source <- bb_source(
#'   name="Oceandata SeaWiFS Level-3 mapped monthly 9km chl-a",
#'   id="SeaWiFS_L3m_MO_CHL_chlor_a_9km",
#'   description="Monthly remote-sensing chlorophyll-a from the SeaWiFS satellite at
#'     9km spatial resolution",
#'   doc_url="https://oceancolor.gsfc.nasa.gov/",
#'   citation="See https://oceancolor.gsfc.nasa.gov/citations",
#'   license="Please cite",
#'   method=list("bb_handler_oceandata",search="S*L3m_MO_CHL_chlor_a_9km.nc"),
#'   postprocess=NULL,
#'   collection_size=7.2,
#'   data_group="Ocean colour")
#'
#' @export
bb_handler_oceandata <- function(search,dtype,...) {
    assert_that(is.string(search),nzchar(search))
    if (!missing(dtype)) {
        if (!is.null(dtype)) assert_that(is.string(dtype),nzchar(dtype))
    } else {
        dtype <- NULL
    }
    do.call(bb_handler_oceandata_inner,list(...,search=search,dtype=dtype))
}


# @param config bb_config: a bowerbird configuration (as returned by \code{bb_config}) with a single data source
# @param verbose logical: if TRUE, provide additional progress output
# @param local_dir_only logical: if TRUE, just return the local directory into which files from this data source would be saved
bb_handler_oceandata_inner <- function(config,verbose=FALSE,local_dir_only=FALSE,search,dtype=NULL) {
    ## oceandata synchronization handler

    ## oceandata provides a file search interface, e.g.:
    ## wget -q --post-data="cksum=1&search=A2002*DAY_CHL_chlor*9km*" -O - https://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi
    ## or
    ## wget -q --post-data="dtype=L3b&cksum=1&search=A2014*DAY_CHL.*" -O - https://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi
    ## returns list of files and SHA1 checksum for each file
    ## each file can be retrieved from https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/filename

    ## expect that config$data_sources$method_flags will contain the search and dtype components of the post string
    ##  i.e. "search=...&dtype=..." in "dtype=L3m&addurl=1&results_as_file=1&search=A2002*DAY_CHL_chlor*9km*"
    ##  or just include the data type in the search pattern e.g. "search=A2002*L3m_DAY_CHL_chlor*9km*

    assert_that(is(config,"bb_config"))
    assert_that(nrow(bb_data_sources(config))==1)
    ##assert_that(is.list(bb_data_sources(config)$method_flags))
    ##assert_that(is.character(bb_data_sources(config)$method_flags[[1]]))
    assert_that(is.flag(verbose),!is.na(verbose))
    assert_that(is.flag(local_dir_only),!is.na(local_dir_only))
    assert_that(is.string(search),nzchar(search))
    if (!is.null(dtype)) assert_that(is.string(dtype),nzchar(dtype))

    ##method_flags <- bb_data_sources(config)$method_flags[[1]]
    ##assert_that(!is.null(method_flags$search),is.string(method_flags$search),nzchar(method_flags$search))
    this_att <- bb_settings(config)
    if (local_dir_only) {
        ## highest-level dir
        out <- "oceandata.sci.gsfc.nasa.gov"
        ## refine by platform
        this_search_spec <- search##sub("search=","",method_flags)
        this_platform <- oceandata_platform_map(substr(this_search_spec,1,1))
        if (nchar(this_platform)>0) out <- file.path(out,this_platform)
        if (grepl("L3m",this_search_spec)) {
            out <- file.path(out,"Mapped")
        } else if (grepl("L3",this_search_spec)) {
            out <- file.path(out,"L3BIN")
        }
        return(file.path(this_att$local_file_root,out))
    }
    tries <- 0
    while (tries<3) {
        ## sometimes this takes a couple of attempts!
        ## sys code, nb don't quote args, will break on unix
        qry <- paste0("--post-data=cksum=1&search=",search)
        if (!is.null(dtype))
            qry <- paste0(qry,"&dtype=",dtype)
        ##if (get_os()=="windows") qry <- paste0("\"",qry,"\"") ## not sure if need these on windows or not!!
        myfiles <- bb_wget("https://oceandata.sci.gsfc.nasa.gov/search/file_search.cgi",recursive=FALSE,extra_flags=c("-q",qry,"-O","-"),capture_stdout=TRUE,verbose=verbose)
        if (myfiles$status==0) break
        tries <- tries+1
    }
    if (myfiles$status!=0) stop("error with oceancolour data file search: could not retrieve file list (query: ",search,")")
    myfiles <- strsplit(rawToChar(myfiles$stdout),"\n")[[1]]
    ## catch "Sorry No Files Matched Your Query"
    if (any(grepl("no files matched your query",myfiles,ignore.case=TRUE))) stop("No files matched the supplied oceancolour data file search query (",search,")")
    myfiles <- myfiles[-c(1,2)] ## get rid of header line and blank line that follows it
    myfiles <- as_tibble(do.call(rbind,lapply(myfiles,function(z)strsplit(z,"[[:space:]]+")[[1]]))) ## split checksum and file name from each line
    colnames(myfiles) <- c("checksum","filename")
    myfiles <- myfiles[order(myfiles$filename),]
    ## for each file, download if needed and store in appropriate directory
    out <- TRUE
    for (idx in seq_len(nrow(myfiles))) {
        this_url <- paste0("https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/",myfiles$filename[idx]) ## full URL
        this_fullfile <- oceandata_url_mapper(this_url) ## where local copy will go
        if (is.null(this_fullfile)) {
            warning(sprintf("skipping oceandata URL (%s): cannot determine the local path to store the file",this_url))
            next
        }
        this_exists <- file.exists(this_fullfile)
        download_this <- !this_exists
        if (this_att$clobber==0) {
            ## don't clobber existing
        } else if (this_att$clobber==1) {
            ## replace existing if server copy newer than local copy
            ## use checksum rather than dates for this
            if (this_exists) {
                existing_checksum <- file_hash(this_fullfile,"sha1")
                download_this <- existing_checksum!=myfiles$checksum[idx]
            }
        } else {
            download_this <- TRUE
        }
        if (download_this) {
            dummy <- config
            ## note that if dry_run is TRUE, it will be passed through to bb_handler_wget here
            ##dummy$method_flags <- paste("--timeout=1800","--recursive","--directory-prefix",oceandata_url_mapper(this_url,path_only=TRUE),"--cut-dirs=2","--no-host-directories",sep=" ")
            temp <- bb_data_sources(dummy)
            temp$method <- list(list("bb_handler_wget",recursive=TRUE,extra_flags=c("--timeout=1800","--directory-prefix",oceandata_url_mapper(this_url,path_only=TRUE),"--cut-dirs=2","--no-host-directories")))
            ##temp$method_flags <- list(list(recursive=TRUE,extra_flags=c("--timeout=1800","--directory-prefix",oceandata_url_mapper(this_url,path_only=TRUE),"--cut-dirs=2","--no-host-directories")))
            temp$source_url <- this_url
            bb_data_sources(dummy) <- temp
            out <- out && do.call(bb_handler_wget,c(list(dummy,verbose=verbose),temp$method[[1]][-1]))
        } else {
            if (this_exists) {
                if (verbose) cat(sprintf("not downloading %s, local copy exists with identical checksum\n",myfiles$filename[idx]))
            }
        }
    }
    out
}


# Satellite platform names and abbreviations used in Oceancolor URLs and file names
# Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
# For example, \url{https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the Oceancolor visual browser or file search facility)
# maps to \url{https://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). Locally, this file will be stored in oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
# The \code{oceandata_platform_map} function maps the URL platform component ("V" in this example) to the corresponding directory name ("VIIRS")
# @param abbrev character: the platform abbreviation from the URL (e.g. "Q" for Aquarius, "M" for MODIS-Aqua)
# @param error_no_match logical: should an error be thrown if the abbrev is not matched?
# @references \url{https://oceandata.sci.gsfc.nasa.gov/}
# @return Either the platform name string corresponding to the abbreviation, if \code{abbrev} supplied, or a data.frame of all abbreviations and platform name strings if \code{abbrev} is missing
# @seealso \code{\link{oceandata_timeperiod_map}}, \code{\link{oceandata_parameter_map}}
# @export
oceandata_platform_map <- function(abbrev,error_no_match=FALSE) {
    rawtext <- "abbrev,platform
Q,Aquarius
C,CZCS
H,HICO
M,MERIS
A,MODISA
T,MODIST
O,OCTS
S,SeaWiFS
V,VIIRS"
    allp <- read.table(text=rawtext,stringsAsFactors=FALSE,sep=",",header=TRUE)
    if (missing(abbrev)) {
        allp
    } else {
        out <- allp$platform[allp$abbrev==abbrev]
        if (error_no_match & length(out)<1) {
            stop("oceandata URL platform token ",abbrev," not recognized")
        }
        out
    }
}

# Time periods and abbreviations used in Oceancolor URLs and file names
# Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
# For example, \url{https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the Oceancolor visual browser or file search facility)
# maps to \url{https://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). Locally, this file will be stored in oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
# The \code{oceandata_timeperiod_map} function maps the URL time period component ("DAY" in this example) to the corresponding directory name ("Daily")
# @references \url{https://oceandata.sci.gsfc.nasa.gov/}
# @param abbrev string: the time period abbreviation from the URL (e.g. "DAY" for daily, "SCSP" for seasonal spring climatology)
# @param error_no_match logical: should an error be thrown if the abbrev is not matched?
# @return Either the time period string corresponding to the abbreviation, if \code{abbrev} supplied, or a data.frame of all abbreviations and time period strings if \code{abbrev} is missing
# @seealso \code{\link{oceandata_platform_map}}, \code{\link{oceandata_parameter_map}}
# @export
oceandata_timeperiod_map <- function(abbrev,error_no_match=FALSE) {
    rawtext <- "abbrev,time_period
WC,8D_Climatology
8D,8Day
YR,Annual
CU,Cumulative
DAY,Daily
MO,Monthly
MC,Monthly_Climatology
R32,Rolling_32_Day
SNSP,Seasonal
SNSU,Seasonal
SNAU,Seasonal
SNWI,Seasonal
SCSP,Seasonal_Climatology
SCSU,Seasonal_Climatology
SCAU,Seasonal_Climatology
SCWI,Seasonal_Climatology"

    alltp <- read.table(text=rawtext,stringsAsFactors=FALSE,sep=",",header=TRUE)
    if (missing(abbrev)) {
        alltp
    } else {
        out <- alltp$time_period[alltp$abbrev==abbrev]
        if (error_no_match & length(out)<1) {
            stop("oceandata URL timeperiod token ",abbrev," not recognized")
        }
        out
    }
}


# rdname oceandata_parameter_map
#
# @param platform V for VIIRS, S for SeaWiFS, etc.
#
# @export
oceandata_parameters <- function(platform) {
    rawtext <- "platform,parameter,pattern
SATCO,Kd,KD490_Kd_490
SATCO,NSST,NSST
SATCO,Rrs,RRS_Rrs_[[:digit:]]+
SATCO,SST,SST
SATCO,SST,SST_sst
SATCO,SST4,SST4
SATCO,a,IOP_a_.*
SATCO,adg,IOP_adg_.*
SATCO,angstrom,RRS_angstrom
SATCO,aot,RRS_aot_[[:digit:]]+
SATCO,aph,IOP_aph_.*
SATCO,bb,IOP_bb_.*
SATCO,bbp,IOP_bbp_.*
SATCO,cdom,CDOM_cdom_index
SATCO,chl,CHL_chl_ocx
SATCO,chlor,CHL_chlor_a
SATCO,ipar,FLH_ipar
SATCO,nflh,FLH_nflh
SATCO,par,PAR_par
SATCO,pic,PIC_pic
SATCO,poc,POC_poc
S,NDVI,LAND_NDVI
V,KD490,S?NPP_KD490_Kd_490
V,chl,S?NPP_CHL_chl_ocx
V,chlor,S?NPP_CHL_chlor_a
V,IOP,S?NPP_IOP_.*
V,par,S?NPP_PAR_par
V,pic,S?NPP_PIC_pic
V,poc,S?NPP_POC_poc
V,RRS,S?NPP_RRS_.*"
    ## note some VIIRS parameters that appear in the browse file structure but with no associated files, and so have not been coded here:
    ## CHLOCI GSM QAA ZLEE
    ## platforms yet to do: "Q","H" (are different folder structure to the others)
    read.table(text=rawtext,stringsAsFactors=FALSE,sep=",",header=TRUE)
}


# Parameter names used in Oceancolor URLs and file names
# Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
# For example, \url{https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the Oceancolor visual browser or file search facility)
# maps to \url{https://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). Locally, this file will be stored in oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
# The \code{oceandata_parameter_map} function maps the URL parameter component ("NPP_PAR_par" in this example) to the corresponding directory name ("par").
# @references \url{https://oceandata.sci.gsfc.nasa.gov/}
# @param urlparm string: the parameter component of the URL (e.g. "KD490_Kd_490" for MODIS diffuse attenuation coefficient at 490 nm)
# @param platform character: the platform abbreviation (currently one of "Q" (Aquarius), "C" (CZCS), "H" (HICO), "M" (MERIS), "A" (MODISA), "T" (MODIST), "O" (OCTS), "S" (SeaWiFS), "V" (VIIRS)
# @param error_no_match logical: should an error be thrown if the urlparm is not matched?
# @return Either the directory string corresponding to the URL code, if \code{abbrev} supplied, or a data.frame of all URL regexps and corresponding directory name strings if \code{urlparm} is missing
# @export
oceandata_parameter_map <- function(platform,urlparm,error_no_match=FALSE) {
    if (missing(platform) || !(is.string(platform) && nchar(platform)==1)) stop("platform must be specified as a one-letter character")
    parm_map <- oceandata_parameters()
    parm_map <- parm_map[grepl(platform,parm_map$platform),]
    if (!missing(urlparm)) {
        if (nrow(parm_map)>0) {
            this_parm_folder <- vapply(parm_map$pattern,function(z)grepl(paste0("^",z,"$"),urlparm),FUN.VALUE=TRUE)
            out <- unlist(parm_map$parameter[this_parm_folder])
        } else {
            out <- as.character(NULL)
        }
        if (error_no_match & length(out)<1) {
            stop("oceandata URL parameter token ",urlparm," not recognized for platform ",platform)
        }
        out
    } else {
        parm_map
    }
}


# Map Oceancolor URL to file path
# Oceancolor data file URLs need to be mapped to a file system hierarchy that mirrors the one used on the Oceancolor web site.
# For example, \url{https://oceancolor.gsfc.nasa.gov/cgi/l3/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} or \url{https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (obtained from the Oceancolor visual browser or file search facility)
# maps to \url{https://oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc} (in the Oceancolor file browse interface). Locally, this file will be stored in oceandata.sci.gsfc.nasa.gov/VIIRS/Mapped/Daily/9km/par/2016/V2016044.L3m_DAY_NPP_PAR_par_9km.nc
# The \code{oceandata_url_mapper} function maps the URL parameter component ("NPP_PAR_par" in this example) to the corresponding directory name ("par").
# @references \url{https://oceandata.sci.gsfc.nasa.gov/}
# @param this_url string: the Oceancolor URL, e.g. https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km.bz2
# @param path_only logical: if TRUE, do not append the file name to the path
# @param sep string: the path separator to use
# @return Either the directory string corresponding to the URL code, if \code{abbrev} supplied, or a data.frame of all URL regexps and corresponding directory name strings if \code{urlparm} is missing
# @export
oceandata_url_mapper <- function(this_url,path_only=FALSE,sep=.Platform$file.sep) {
    ## take getfile URL and return (relative) path to put the file into
    ## this_url should look like: https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km.bz2
    ## Mapped files (L3m) should become oceandata.sci.gsfc.nasa.gov/platform/Mapped/timeperiod/spatial/parm/[yyyy/]basename
    ## [yyyy] only for 8Day,Daily,Rolling_32_Day
    ## Binned files (L3b) should become oceandata.sci.gsfc.nasa.gov/platform/L3BIN/yyyy/ddd/basename
    assert_that(is.string(this_url))
    assert_that(is.flag(path_only),!is.na(path_only))
    assert_that(is.string(sep))
    if (grepl("\\.L3m_",this_url)) {
        ## mapped file
        url_parts <- str_match(this_url,"/([ASTCV])([[:digit:]]+)\\.(L3m)_([[:upper:][:digit:]]+)_(.*?)_(9|4)(km)?\\.(bz2|nc)")
        ## e.g. [1,] "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2002359.L3m_DAY_CHL_chlor_a_9km"
        ## [,2] [,3]      [,4]  [,5]  [,6]          [,7]
        ## "A"  "2002359" "L3m" "DAY" "CHL_chlor_a" "9"
        url_parts <- as.data.frame(url_parts,stringsAsFactors=FALSE)
        colnames(url_parts) <- c("full_url","platform","date","type","timeperiod","parm","spatial","spatial_unit")
    } else if (grepl("\\.L3b_",this_url)) {

        url_parts <- str_match(this_url,"/([ASTCV])([[:digit:]]+)\\.(L3b)_([[:upper:][:digit:]]+)_(.*?)\\.(bz2|nc)")
        ## https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_KD490.main.bz2

        ## e.g. [1,] "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A20090322009059.L3b_MO_KD490.main.bz2" "A"  "20090322009059" "L3b" "MO" "KD490"
        ## https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2015016.L3b_DAY_RRS.nc
        url_parts <- as.data.frame(url_parts,stringsAsFactors=FALSE)
        colnames(url_parts) <- c("full_url","platform","date","type","timeperiod","parm")
    } else if (grepl("\\.L2", this_url)) {
      # "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile/A2017002003000.L2_LAC_OC.nc"
      url_parts <- str_match(this_url,"/([ASTCV])([[:digit:]]+)\\.(L2)_([[:upper:][:digit:]]+)_(.*?)\\.(bz2|nc)")
      url_parts <- as.data.frame(url_parts,stringsAsFactors=FALSE)
      colnames(url_parts) <- c("full_url","platform","date","type","coverage","parm", "extension")
    } else {
        stop("not a L2 or L3 binned or L3 mapped file")
    }
    this_year <- substr(url_parts$date,1,4)
    if (is.na(url_parts$type)) {
        warning("unrecognized URL pattern",this_url,", ignoring")
        out <- NULL
    } else {
        switch(url_parts$type,
               L3m={
                   this_parm_folder <- oceandata_parameter_map(url_parts$platform,url_parts$parm,error_no_match=TRUE)
                   out <- paste("oceandata.sci.gsfc.nasa.gov",oceandata_platform_map(url_parts$platform,error_no_match=TRUE),"Mapped",oceandata_timeperiod_map(url_parts$timeperiod,error_no_match=TRUE),paste0(url_parts$spatial,"km"),this_parm_folder,sep=sep)
                   if (url_parts$timeperiod %in% c("8D","DAY","R32")) {
                       out <- paste(out,this_year,sep=sep)
                   }
                   if (!path_only) {
                       out <- paste(out,basename(this_url),sep=sep)
                   } else {
                       out <- paste0(out,sep) ## trailing path separator
                   }
                 },
               L3b={ this_doy <- substr(url_parts$date,5,7)
                     out <- paste("oceandata.sci.gsfc.nasa.gov",oceandata_platform_map(url_parts$platform,error_no_match=TRUE),"L3BIN",this_year,this_doy,sep=sep)
                     if (!path_only) {
                         out <- paste(out,basename(this_url),sep=sep)
                     } else {
                         out <- paste0(out,sep) ## trailing path separator
                     }
                 },
               L2 = {
                 this_doy <- substr(url_parts$date,5,7)
                 out <- paste("oceandata.sci.gsfc.nasa.gov",oceandata_platform_map(url_parts$platform,error_no_match=TRUE),"L2",this_year,this_doy,sep=sep)
                 if (!path_only) {
                   out <- paste(out,basename(this_url),sep=sep)
                 } else {
                   out <- paste0(out,sep) ## trailing path separator
                 }
               },
               stop("unrecognized file type: ",url_parts$type,"\n",str(url_parts))
               )
    }
    out
}



