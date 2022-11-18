#' recipeSearch
#'
#' Search existing data recipes.
#' @param keywords character vector of keywords to be matched to the
#'     recipe names. If not specified, function returns the full
#'     recipe list.
#' @param cachePath A character string for the recipe cache. Must
#'     match the one specified in `recipeUpdate()`. Default is
#'     "ReUseDataRecipe".
#' @return A `recipeHub` object.
#' @examples
#' recipeSearch()
#' recipeSearch("gencode")
#' recipeSearch(c("STAR", "index"))
#' @export
#' 
recipeSearch <- function(keywords = character(), cachePath = "ReUseDataRecipe") {

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
    if(missing(keywords))
       keywords <- ""
    res <- bfcquery(bfc, query = keywords, ignore.case = TRUE)
    recipeHub(bfc[res$rid])
}
