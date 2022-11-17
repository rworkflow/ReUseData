#' update the local data records by reading the yaml files in the
#' specified directory recursively.
#'
#' @param dir a character string for the directory where all data are
#'     saved. Data information will be collected recursively within
#'     this directory.
#' @param cachePath the cache path for recording all available
#'     datasets. Default is "ReUseData".
#' @param outMeta Logical. If TRUE, a "meta_data.csv" file will be
#'     generated in the `dir`, containing information about all
#'     available datasets in the directory: The file path to the yaml
#'     files, and yaml entries including parameter values for data
#'     recipe, file path to datasets, notes, version (from
#'     `getData()`), tag (from `dataTag()`) if available and data
#'     generating date.
#' @details Users can directly retrieve information for all available
#'     datasets by using `meta_data(dir=)`, which generates a data
#'     frame in R with same information as described above and can be
#'     saved out. `dataUpdate` does extra check for all datasets
#'     (check the file path in "output" column), remove invalid ones,
#'     e.g., empty or non-existing file path, and create a data cache
#'     for all valid datasets.
#' @return a `dataHub` object containing the information about local
#'     data cache, e.g., data name, data path, etc.
#' @examples
#' ## 
#' ## dataUpdate(dir = "~/workspace/SharedData")
#' @export
#' 
dataUpdate <- function(dir, cachePath = "ReUseData", outMeta = FALSE, keepTags = TRUE) {
    ## browser()    
    ## find/create the cache path, and create a BFC object.
    bfcpath <- Sys.getenv("cachePath")  ## FIXME: create the system env for "cachePath"
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
        ## dirs <- unique(dirname(meta$yml[ind]))
        ## ptns <- gsub(".yml", "", basename(meta$yml[ind]))
        ## fls <- list.files(dirs, pattern = paste0(ptns, collapse="|"),
        ##                   full.names=TRUE)    
        ## file.remove(fls)
        meta <- meta[!ind, ]
    }
    
    message("\nUpdating data record...")
    fpath <- meta$output
    
    ## add any non-cached recipes to local cache
    if(length(fpath) > 0){
        rnames <- basename(fpath)
        ## ex <- rnames %in% bfcinfo(bfc)$rname
        ## if(sum(!ex)>0){
        ##     idx <- which(!ex)
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
