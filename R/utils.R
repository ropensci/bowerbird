##-------------------------------------------
## various helper functions
## not exported for user

save_current_settings <- function() {
    return(list(working_dir=getwd(), ## current working directory
                env_http_proxy=Sys.getenv("http_proxy"), ## proxy env vars
                env_https_proxy=Sys.getenv("https_proxy"),
                env_ftp_proxy=Sys.getenv("ftp_proxy")
                ))
}

restore_settings <- function(settings) {
    setwd(settings$working_dir)
    Sys.setenv(http_proxy=settings$env_http_proxy)
    Sys.setenv(https_proxy=settings$env_https_proxy)
    Sys.setenv(ftp_proxy=settings$env_ftp_proxy)
    invisible(NULL)
}

dir_exists <- function(z) file.exists(dirname(z)) && !(!file.info(z)$isdir || is.na(file.info(z)$isdir))
