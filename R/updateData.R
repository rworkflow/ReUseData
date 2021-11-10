#' update the data records by reading the .yml files using BiocFileCache
#'
#' @param dir data path
#' @param cachePath the cache path for recording all available datasets
#' @example
#' updatedata(dir = "~/workspace/SharedData")
#' 
updateData <- function(dir, cachePath = "ReUseData") {
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
    bfcremove(bfc, bfcinfo(bfc)$rid)
    
    meta <- meta_data(dir = dir, outdir = dir)

    message("Updating data record...")
    fpath <- meta$Output
    
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
                         meta[, c("params", "Notes", "Version", "Date")])
        bfcmeta(bfc, "dataMeta", overwrite = TRUE) <- bm
    }
    return(bfcinfo(bfc)[, c("rname", "fpath")]) ## FIXME: use a new class "dataHub"? to print the recipe names, etc. 
    ## return(cwlHub(bfc))
}
