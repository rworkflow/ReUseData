#' update the data records by reading the .yml files using BiocFileCache
#'
#' @param dir a character string for the path to save the data.
#' @param cachePath the cache path for recording all available
#'     datasets. Default is "ReUseData".
#' @return a `dataHub` object containing the information about local
#'     data cache, e.g., data name, data path, etc.
#' @examples
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

    if(keepTags) {
        tag_old <- tags(dataHub(bfc))
    }
    
    bfcremove(bfc, bfcinfo(bfc)$rid)
    
    meta <- meta_data(dir = dir)
    if (outMeta) {
        write.csv(meta, file = file.path(dir, "meta_data.csv"))
    }
        
    message("Updating data record...")
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
        tags(dh) <- tag_old
    }
    return(dh)
}
