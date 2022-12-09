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
    url <- dataPaths(datahub[idx])
    if (!dir.exists(outdir))
        dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    download.file(url, destfile = file.path(outdir, basename(url)))
    yml <- dataYml(datahub[idx])
    pfix <- gsub(".yml", "", basename(yml))
    ofls <- file.path(dirname(yml), paste0(pfix, c(".yml", ".cwl", ".md5", ".sh")))
    sapply(ofls, function(x) download.file(x, file.path(outdir, basename(x))))
    message("The data file is downloaded: ", file.path(outdir, basename(url)))
    
    ## change the .yml "# output: " row with new file path
    nyml <- file.path(outdir, basename(yml))
    cts <- readLines(nyml)
    idx <- grep("# output", cts)
    cts[idx] <- paste0("# output: ", file.path(outdir, basename(url)))
    write(cts, nyml)
}
