#' search recipes on GitHub repository
#'
#' @param keywords keywords
#' @param cachePath ReUseData by default
#' @return return a DataHub object to be defined?
#' @examples
#' searchRecipe("gencode")
#' @export
#' 
searchRecipe <- function(keywords, cachePath = "ReUseDataRecipe") {

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

    res <- bfcquery(bfc, query = keywords)
    res[, c("rname", "rpath")]  ## FIXME: return same format as "updateRecipe". 
}

searchData <- function(keywords) {
    return(keywords)
}

## todo:
## 1. should we save data into R_user_dir? or recipes only?
## 2. should we search the meta file? where is the default location? 
