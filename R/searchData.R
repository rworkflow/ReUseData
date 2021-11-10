#' search recipes on GitHub repository
#'
#' @param keywords keywords
#' @param cachePath ReUseData by default
#' @return return a DataHub object to be defined?
#' @examples
#' searchData("gencode")
#' @export
#' 
searchData <- function(keywords, cachePath = "ReUseData") {

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
                    field = c("rname", "fpath", "params", "Notes", "Version", "Date"))
    res[, c("rname", "rpath")]  ## FIXME: return same format as "updateRecipe". 
}
