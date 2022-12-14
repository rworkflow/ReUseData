#' getCloudData Download the pre-generated curated data sets from
#' ReUseData cloud bucket
#' @param datahub The `dataHub` object returned from `dataSearch()`
#'     with 1 data record available on ReUseData cloud bucket.
#'@param outdir The output directory for the data (and concomitant
#'     annotation files) to be downloaded. It is recommended to use a
#'     new folder under a shared folder for a new to-be-downloaded
#'     data.
#' @return Data and comcomitant annotation files will be downloaded to
#'     the user-specified folder that is locally searchable with
#'     `dataSearch()`.
#' @export 
#' @examples
#' outdir <- file.path(tempdir(), "gcpData")
#' dataUpdate(outdir, cloud=TRUE)
#' dh <- dataSearch(c("ensembl", "GRCh38"))
#' 
#' ## download data from google bucket
#' getCloudData(dh[1], outdir = outdir)
#'
#' ## Update local data caching
#' dataUpdate(outdir)  ## no "cloud=TRUE" here, only showing local data cache
#'
#' ## Now the data is available to use locally 
#' dataSearch(c("ensembl", "GRCh38"))
#'
getCloudData <- function(datahub, outdir = character()) {
    tp <- mcols(datahub)$rtype
    idx <- which(tp == "web")
    if (!length(idx)) {
        stop("The 'data' is not a cloud object. Please double check the data name.")
    }

    ## download data files
    url <- dataPaths(datahub[idx])
    if (!dir.exists(outdir))
        dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    for (i in seq_along(url)) {
        download.file(url[i], destfile = file.path(outdir, basename(url[i])))
    }
    
    ## download unique annotation files
    yml <- dataYml(datahub[idx])
    ymlpath <- unique(gsub(".yml", "", yml))
    for (i in seq_along(ymlpath)) {
        ofls <- paste0(ymlpath[i], c(".yml", ".cwl", ".md5", ".sh"))
        sapply(ofls, function(x) download.file(x, file.path(outdir, basename(x))))
    }
    message("Data is downloaded: \n", paste(file.path(outdir, basename(url)), collapse = "\n"))
    
    ## change the .yml "# output: " row with new file path
    nyml <- file.path(outdir, unique(basename(yml)))
    for (i in seq_along(nyml)) {
        cts <- readLines(nyml[i])
        idx <- grep("# output", cts)
        cts[idx] <- paste0("# output: ", outdir, "/", basename(gsub("# output: ", "", cts[idx])))
        ## cts[idx] <- paste0("# output: ", file.path(outdir, basename(url[i])))
        write(cts, nyml[i])
    }
}
