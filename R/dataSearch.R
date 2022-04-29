#' search recipes on GitHub repository
#'
#' @param keywords keywords
#' @param cachePath ReUseData by default
#' @return a `dataHub` object containing the information about local
#'     data cache, e.g., data name, data path, etc.
#' @examples
#' dataSearch(c("gencode", "human", "38"))
#' @export
#' 
dataSearch <- function(keywords, cachePath = "ReUseData") {

    ## browser()
    ## find/create the cache path, and create a BFC object.
    bfcpath <- Sys.getenv("cachePath")
    if(bfcpath != ""){
        cachePath <- file.path(bfcpath, cachePath)
    }else{
        if(!file.exists(cachePath) & !grepl("^/", cachePath)){
            cachePath <- R_user_dir(cachePath, which = "cache")
        }
    }
    bfc <- BiocFileCache(cachePath, ask = FALSE)

    res <- bfcquery(bfc, query = keywords,
                    field = c("rname", "fpath", "params", "notes", "version", "date"))
    dataHub(bfc[res$rid])
}
