#' dataUpdate
#'
#' Function to update the local data records by reading the yaml files
#' in the specified directory recursively.
#'
#' @param dir a character string for the directory where all data are
#'     saved. Data information will be collected recursively within
#'     this directory.
#' @param cachePath A character string specifying the name for the
#'     `BiocFileCache` object to store all the curated data
#'     resources. Once specified, must match the `cachePath` argument
#'     in `dataSearch`. Default is "ReUseData".
#' @param outMeta Logical. If TRUE, a "meta_data.csv" file will be
#'     generated in the `dir`, containing information about all
#'     available datasets in the directory: The file path to the yaml
#'     files, and yaml entries including parameter values for data
#'     recipe, file path to datasets, notes, version (from
#'     `getData()`), if available and data generating date.
#' @param keepTags If keep the prior assigned data tags. Default is
#'     TRUE.
#' @param cleanup If remove any invalid intermediate files. Default is
#'     FALSE. In cases one data recipe (with same parameter values)
#'     was evaluated multiple times, the same data file(s) will match
#'     to multiple intermediate files (e.g., .yml). `cleanup` will
#'     remove older intermediate files, and only keep the most recent
#'     ones that matches the data file. When there are any
#'     intermediate files that don't match to any data file, `cleanup`
#'     will also remove those.
#' @param cloud Whether to return the pregenerated data from Google
#'     Cloud bucket of ReUseData. Default is FALSE.
#' @param remote Whether to use the csv file (containing information
#'     about pregenerated data on Google Cloud) from GitHub, which is
#'     most up-to-date. Only works when `cloud = TRUE`. Default is
#'     FALSE.
#' @param checkData check if the data (listed as "# output: " in the
#'     yml file) exists. If not, do not include in the output csv
#'     file. This argument is added for internal testing purpose.
#' @details Users can directly retrieve information for all available
#'     datasets by using `meta_data(dir=)`, which generates a data
#'     frame in R with same information as described above and can be
#'     saved out. `dataUpdate` does extra check for all datasets
#'     (check the file path in "output" column), remove invalid ones,
#'     e.g., empty or non-existing file path, and create a data cache
#'     for all valid datasets.
#' @return a `dataHub` object containing the information about local
#'     data cache, e.g., data name, data path, etc.
#' @export
#' @examples
#' ## Generate data 
#' library(Rcwl)
#' outdir <- file.path(tempdir(), "SharedData")
#'
#' recipeLoad("echo_out", return = TRUE)
#' Rcwl::inputs(echo_out)
#' echo_out$input <- "Hello World!"
#' echo_out$outfile <- "outfile"
#' res <- getData(echo_out,
#'                outdir = outdir,
#'                notes = c("echo", "hello", "world", "txt"),
#'                showLog = TRUE)
#' 
#' \dontrun{
#' recipeLoad("ensembl_liftover", return = TRUE)
#' Rcwl::inputs(ensembl_liftover)
#' ensembl_liftover$species <- "human"
#' ensembl_liftover$from <- "GRCh37"
#' ensembl_liftover$to <- "GRCh38"
#' res <- getData(ensembl_liftover,
#'         outdir = outdir, 
#'         notes = c("ensembl", "liftover", "human", "GRCh37", "GRCh38"),
#'         showLog = TRUE)
#'}
#' ## Update data cache (with or without prebuilt data sets from ReUseData cloud bucket)
#' dataUpdate(dir = outdir)
#' dataUpdate(dir = outdir, cloud = TRUE)
#'
#' ## newly generated data are now cached and searchable
#' dataSearch(c("hello", "world"))
#' dataSearch(c("ensembl", "liftover"))  ## both locally generated data and google cloud data! 
#' 
dataUpdate <- function(dir, cachePath = "ReUseData", outMeta = FALSE,
                       keepTags = TRUE, cleanup = FALSE, cloud = FALSE, remote = FALSE, checkData = TRUE) {
    ## find/create the cache path, and create a BFC object.
    bfcpath <- Sys.getenv("cachePath")
    if(bfcpath != ""){
        cachePath <- file.path(bfcpath, "ReUseData")
    }else{
        if(!file.exists(cachePath) & !grepl("^/", cachePath)){
            cachePath <- R_user_dir(cachePath, which = "cache")
        }
    }
    bfc <- BiocFileCache(cachePath, ask = FALSE)


    if("tag" %in% colnames(mcols(dataHub(bfc)))){
        if(keepTags) {
            tag_old <- dataTags(dataHub(bfc))
        }
    }else{
        keepTags <- FALSE
    }
    
    bfcremove(bfc, bfcinfo(bfc)$rid)
    
    meta <- meta_data(dir = dir, cleanup = cleanup, checkData = checkData)

    ## append pregenerated cloud data
    if (cloud) {
        if (remote) {
            dltry <- tryCatch(
                download.file(
                    "https://raw.githubusercontent.com/rworkflow/ReUseDataRecipe/master/meta_gcp.csv",
                    file.path(tempdir(), "meta_gcp.csv")),
                error = identity)
            if (inherits(dltry, "error")) {
                stop(conditionMessage(dltry))
            }
            meta_gcp <- read.csv(file.path(tempdir(), "meta_gcp.csv"))
        } else {
            meta_gcp <- read.csv(system.file("extdata", "meta_gcp.csv", package = "ReUseData"))
        }
        meta <- rbind(meta, meta_gcp)
    }
    
    if (outMeta) {
        write.csv(meta, file = file.path(dir, "meta_data.csv"),
                  quote=FALSE, row.names=FALSE)
        message("\nMeta file for all available datasets generated: ",
                file.path(dir, "meta_data.csv"))
    }

    message("\nUpdating data record...")
    fpath <- meta$output
    
    ## add any non-cached recipes to local cache
    if(length(fpath) > 0){
        rnames <- basename(fpath)
        for(i in seq_along(fpath)){
            add1 <- bfcadd(bfc, rnames[i], fpath = fpath[i],
                           rtype = "auto", action = "asis", download=FALSE)
            message(basename(add1), " added")
        }
        bm <- data.frame(rid = bfcrid(bfc),
                         meta[, c("params", "notes", "date", "yml")], tag = "")
        bfcmeta(bfc, "dataMeta", overwrite = TRUE) <- bm
    }

    dh <- dataHub(bfc)
    if(keepTags){
        dataTags(dh[mcols(dh)$rtype == "local"]) <- tag_old
    }
    return(dh)
}

