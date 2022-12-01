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
#'     `getData()`), tag (from `dataTag()`) if available and data
#'     generating date.
#' @param keepTags If keep the prior assigned data tags. Default is TRUE. 
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
#' rcp <- recipeLoad("gencode_transcripts")
#' inputs(rcp)
#' rcp$species <- "human"
#' rcp$version <- "42"
#' outdir <- file.path(tempdir(), "SharedData")
#' res <- getData(rcp,
#'         outdir = outdir, 
#'         prefix = "gencode_annotation_human_42",
#'         notes = c("gencode", "human", "42"),
#'         showLog = TRUE)
#'
#' ## Update data cache 
#' dataUpdate(dir = outdir)
#'
#' ## newly generated data are now cached and searchable
#' dataSearch(c("gencode", "42"))
#' 
dataUpdate <- function(dir, cachePath = "ReUseData", outMeta = FALSE, keepTags = TRUE) {
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
    
    meta <- meta_data(dir = dir)
    if (outMeta) {
        write.csv(meta, file = file.path(dir, "meta_data.csv"))
        message("\nMeta file for all available datasets generated: ", file.path(dir, "meta_data.csv"))
    }

    ## if any data not exist (meta$output), then delete that record. 
    ind <- meta$output == "" | !file.exists(meta$output)
    if (any(ind)) {
        message("\nCleaning up invalid data records...")
        meta <- meta[!ind, ]
    }
    
    message("\nUpdating data record...")
    fpath <- meta$output
    
    ## add any non-cached recipes to local cache
    if(length(fpath) > 0){
        rnames <- basename(fpath)
        for(i in seq_along(fpath)){
            add1 <- bfcadd(bfc, rnames[i], fpath = fpath[i],
                           rtype = "local", action = "asis")
            message(basename(add1), " added")
        }
        bm <- data.frame(rid = bfcrid(bfc),
                         meta[, c("params", "notes", "version", "date", "tag")])
        bfcmeta(bfc, "dataMeta", overwrite = TRUE) <- bm
    }

    dh <- dataHub(bfc)
    if(keepTags){
        dataTags(dh) <- tag_old
    }
    return(dh)
}
