#' recipeUpdate
#'
#' Function to sync and get the most updated data recipes from the
#' "rworkflows/ReUseDataRecipes" GitHub repository. 
#' @param cachePath The cache path of the BiocFileCache object to
#'     store the ReUseData recipes.
#' @param force Whether to clean existing recipes cache.
#' @param branch The branch of github recipes repository. It can be
#'     "master" and "dev". "force = TRUE" is recommended when swithing
#'     branch.
#' @importFrom tools R_user_dir
#' @import BiocFileCache
#' @import utils
#' @export
#' @examples
#' \dontrun{
#' tools <- UpdateRecipe()
#' }

loadRecipe <- function(rcpname) {
    rcppath <- recipeSearch(rcpname)$rpath
    source(rcppath)
    return(eval(parse(text = rcpname)))  ## FIXME: return the object to R env
}

recipeUpdate <- function(cachePath = "ReUseDataRecipe", force = FALSE){
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
    
    message("Update recipes...")
    dlpath <- file.path(cachePath, "master.zip")
    download.file("https://github.com/rworkflow/ReUseDataRecipe/archive/refs/heads/master.zip",
                  dlpath)
    unzip(dlpath, exdir = cachePath)
    fpath <- list.files(file.path(cachePath, "ReUseDataRecipe-master"),
                        full.names = TRUE)

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
    return(bfcinfo(bfc)[, c("rname", "fpath")])
    ## FIXME: use a new class "dataHub"? to print the recipe names, etc. 
    ## return(cwlHub(bfc))
}

