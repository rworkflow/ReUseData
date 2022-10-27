#' search existing data recipes.
#'
#' @param keywords character vector of keywords to be matched to the recipe names.
#' @param cachePath A character string for the recipe caching path. "ReUseDataRecipe" by default.
#' @return return a recipeHub object.
#' @examples
#' recipeSearch("gencode")
#' recipeSearch(c("STAR", "index"))
#' @export
#' 
recipeSearch <- function(keywords, cachePath = "ReUseDataRecipe") {

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

    res <- bfcquery(bfc, query = keywords, ignore.case = TRUE)
    recipeHub(bfc[res$rid])
}
