#' recipeUpdate
#'
#' Function to sync and get the most updated and newly added data
#' recipes through the pubic "rworkflows/ReUseDataRecipes" GitHub
#' repository or user-specified private GitHub repo.
#' @param cachePath A character string specifying the name for the
#'     `BiocFileCache` object to store the ReUseData recipes. Once
#'     specified here, must use the same for `cachePath` argument in
#'     `recipeSearch`, and `recipeLoad`. Default is "ReUseDataRecipe".
#' @param force Whether to remove existing and regenerate recipes
#'     cache. Default is FALSE. Only use if any old recipes that have
#'     been previously cached locally are updated remotely (on GitHub
#'     `repos`).
#' @param remote Whether to download the data recipes directly from a
#'     GitHub repository. Default is FALSE.
#' @param repos The GitHub repository containing data recipes that are
#'     to be synced to local cache. Only works when
#'     `remote=TRUE`. Default is "rworkflows/ReUseDataRecipes" GitHub
#'     repository where public data recipes are saved, which might be
#'     more up-to-date than the recipes contained in`ReUseData`
#'     package. It can also be a private GitHub repository where users
#'     save their own data recipes.
#' @return a `recipeHub` object.
#' @importFrom tools R_user_dir
#' @import BiocFileCache
#' @import Rcwl
#' @import utils
#' @export
#' @examples
#' ## recipeUpdate()
#' ## recipeUpdate(force=TRUE)
#' ## recipeUpdate(force = TRUE, remote = TRUE)

recipeUpdate <- function(cachePath = "ReUseDataRecipe",
                         force = FALSE,
                         remote = FALSE,
                         repos = "rworkflow/ReUseDataRecipe"){
    
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

    ## if "force=TRUE", remove local cache and reload/update all
    ## recipes for a new cache. 
    if(force){
        message("Warning: existing caches will be removed and regenerated!")
        bfcremove(bfc, bfcinfo(bfc)$rid)
    }

    message("Update recipes...")
    if (remote) {
        dlpath <- file.path(cachePath, "recipes.zip")
        download.file(paste0("https://github.com/", repos,
                             "/archive/refs/heads/master.zip"),
                      dlpath)
        unzip(dlpath, exdir = cachePath)
        fpath <- list.files(
            file.path(cachePath, paste0(basename(repos), "-master")),
            full.names=TRUE)
        fpath <- fpath[basename(fpath) != "dataGen.R"]
    } else {
        rcpfls <- list.files(
            system.file("extdata/dataRecipes", package = "ReUseData"),
            full.names = TRUE)
        file.copy(rcpfls, cachePath)
        fpath <- list.files(cachePath, pattern = ".R", full.names = TRUE)
    }
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
    message()
    recipeHub(bfc)
}

