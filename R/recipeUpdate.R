#' recipeUpdate
#'
#' Function to sync and get the most updated data recipes through the
#' pubic "rworkflows/ReUseDataRecipes" GitHub repository or user
#' specified private GitHub repo.
#' @param cachePath The cache path of the BiocFileCache object to
#'     store the ReUseData recipes. "ReUseDataRecipe" by default. 
#' @param force Whether to clean existing recipes cache. Default is
#'     FALSE.
#' @param repo The GitHub repository for data recipes. By default, it
#'     reads the "rworkflows/ReUseDataRecipes" GitHub
#'     repository. Users can also specify a GitHub repo for their
#'     private data recipes.
#' @importFrom tools R_user_dir
#' @import BiocFileCache
#' @import Rcwl
#' @import utils
#' @export
#' @examples
#' \dontrun{
#' rcps <- recipeUpdate()
#' rcps
#' }

recipeUpdate <- function(cachePath = "ReUseDataRecipe", force = FALSE, repo = "rworkflow/ReUseDataRecipe"){
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

    ## if "force=TRUE", remove local recipes, and reload/update all
    ## recipes from remote repo.
    if(force){
        message("Warning: existing caches will be removed")
        bfcremove(bfc, bfcinfo(bfc)$rid)
    }

    ## FIXME: CREATE A private github repo for private data recipes. 
    message("Update recipes...")
    dlpath <- file.path(cachePath, "recipes.zip")
    download.file(paste0("https://github.com/", repo, "/archive/refs/heads/master.zip"),
                  dlpath)
    unzip(dlpath, exdir = cachePath)
    fpath <- list.files(file.path(cachePath, paste0(basename(repo), "-master")), full.names=TRUE)
    ## add any non-cached recipes to local cache
    if(length(fpath) > 0){
        rnames <- sub(".R$", "", basename(fpath))
        ex <- rnames %in% bfcinfo(bfc)$rname
        if(sum(!ex)>0){
            idx <- which(!ex)
            for(i in idx){
                add1 <- bfcadd(bfc, rnames[i], fpath = fpath[i],
                               rtype = "local", action = "asis")
                message(basename(add1), " added")
            }            
        }
    }
    recipeHub(bfc)
}

