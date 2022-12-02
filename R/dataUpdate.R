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
#' @param keepTags If keep the prior assigned data tags. Default is
#'     TRUE.
#' @param removeOld If remove the older intermediate files for same
#'     dataset. Default is TRUE. In cases one data recipe (with same
#'     parameter values) was evaluated multiple times, the same data
#'     file(s) will match to multiple intermeidate files (e.g.,
#'     .yml). `removeOld` will remove older intermediate files, and
#'     only keep the most recent ones that matches the data file.
#' @param cloud Whether to return the pre-generated data from Google
#'     Cloud bucket of ReUseData. Default is FALSE. 
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
#' recipeLoad("gencode_transcripts", return=TRUE)
#' inputs(gencode_transcripts)
#' gencode_transcripts$species <- "human"
#' gencode_transcripts$version <- "42"
#' outdir <- file.path(tempdir(), "SharedData")
#' res <- getData(gencode_transcripts,
#'         outdir = outdir, 
#'         notes = c("gencode", "human", "42"),
#'         showLog = TRUE)
#'
#' ## Update data cache 
#' dataUpdate(dir = outdir)
#' dataUpdate(dir = outdir, cloud = TRUE)
#'
#' ## newly generated data are now cached and searchable
#' dataSearch(c("gencode", "42"))
#' 
dataUpdate <- function(dir, cachePath = "ReUseData", outMeta = FALSE, keepTags = TRUE, removeOld = TRUE, cloud = FALSE) {
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
            tag_old <- dataTags(dataHub(bfc))  ## FIXME: will lose when updating. keepTags add back the old ones. 
        }
    }else{
        keepTags <- FALSE
    }
    
    bfcremove(bfc, bfcinfo(bfc)$rid)
    
    meta <- meta_data(dir = dir)

    if (nrow(meta)) {
        ## if any data not exist (meta$output), then delete that record. 
        ind <- meta$output == "" | !file.exists(meta$output)
        if (any(ind)) {
            message("\nCleaning up invalid data records...")
            meta <- meta[!ind, ]
        }
        
        ## if any duplicated (1 data matches multiple yml files), only keep the most recent ones.
        dup <- duplicated(meta$output)
        if (any(dup)) {
            yml_keep <- c()
            uniqd <- unique(meta$output)
            for (i in seq_along(uniqd)) {
                yml <- meta$yml[meta$output == uniqd[i]]
                ymld <- gsub("[[:alpha:]]|_", "", gsub(".yml$", "", basename(yml)))
                keep <- which(ymld == max(ymld))
                yml_keep <- c(yml_keep, yml[keep])
            }
            idx <- meta$yml %in% yml_keep
            ymls_rm <- meta$yml[!idx]
            ## remove older intermediate files
            if (removeOld) {
                dfrm <- data.frame(dir = dirname(ymls_rm), ptn = gsub(".yml", "", basename(ymls_rm)))
                apply(dfrm, 1, function(x) {file.remove(list.files(x[1], x[2], full.names = TRUE))})
                message("\nOlder intermediate files are removed! Turn removeOld = FALSE to disable!")
            }
            meta  <- meta[meta$yml %in% yml_keep, ]
        }
    }
    ## append pre-generated cloud data
    if (cloud) {
        download.file("https://raw.githubusercontent.com/rworkflow/ReUseDataRecipe/master/meta_gcp.csv",
                      file.path(tempdir(), "meta_gcp.csv"))
        meta_gcp <- read.csv(file.path(tempdir(), "meta_gcp.csv"))
        meta <- rbind(meta, meta_gcp)
    }
    
    if (outMeta) {
        write.csv(meta, file = file.path(dir, "meta_data.csv"), quote=FALSE, row.names=FALSE)
        message("\nMeta file for all available datasets generated: ", file.path(dir, "meta_data.csv"))
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
                         meta[, c("params", "notes", "date", "tag", "yml")])
        bfcmeta(bfc, "dataMeta", overwrite = TRUE) <- bm
    }

    dh <- dataHub(bfc)
    ## if(keepTags){
    ##     dataTags(dh) <- tag_old
    ## }
    return(dh)
}

## todo: keepTags!!!! 
