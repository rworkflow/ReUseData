## This script is modified from "RcwlPipelines::cwlLoad". 

.loadmsg <- function(return = FALSE, value) {
    message("Data recipe ",
            ifelse(return, paste0("'", value, "' "), ""),
            "loaded!\n",
            "Use inputs(", ifelse(return, value, ""),
            ") to check required input parameters before evaluation.",
            "\nCheck here: https://rcwl.org/dataRecipes/", value, ".html",
            "\nfor recipe instructions (e.g., eligible input values, data source, etc.)\n"
            )
}
.load_rcp <- function(rscript, env = .GlobalEnv, return = FALSE){
    .env <- new.env()
    source(rscript, .env)
    objs <- ls(.env)
    idx <- sapply(objs, function(x) is(get(x, envir = .env), "cwlProcess"))

    .loadmsg(return = return, value = objs[idx])

    if(!return){  ## return=FALSE, load the recipe into working
                  ## environment and assign a new name
        get(objs[idx], envir = .env)
    }else{  ## return=TRUE, load the recipe into working
            ## environment with original name
        assign(objs[idx],
               get(objs[idx], envir = .env),
               envir = env)
    }
}

#' recipeLoad
#' 
#' To load data recipe(s) into R environment.
#' @param rcp The (vector of) character string of recipe name or
#'     filepath (`recipeNames()` or `mcols()$fpath` column of the
#'     `recipeHub` object returned from `recipeSearch`).
#' @param cachePath A character string for the recipe cache. Must
#'     match the one specified in `recipeUpdate()`. Default is
#'     "ReUseDataRecipe".
#' @param env The R enviroment to export to. Default is `.GlobalEnv`.
#' @param return Whether to use the original recipe name. Default is
#'     FALSE, where user need to assign a variable name to the
#'     recipe. e.g., `rcp1 <- recipeLoad()`. If TRUE, it loads the
#'     recipe and uses its original name, and user doesn't need to
#'     assign a new name. e.g., `recipeLoad(return=TRUE)`. If multiple
#'     recipes are to be loaded, `return=TRUE` must be used.
#' @return A data recipe of `cwlProcess` S4 class, which is ready to
#'     be evaluated in _R_.
#' @import methods
#' @export
#' @examples
#' ########################
#' ## Load single recipe
#' ########################
#'
#' library(Rcwl)
#' recipeSearch("liftover")
#' rcp <- recipeLoad("ensembl_liftover")
#' Rcwl::inputs(rcp)
#' rm(rcp)
#'
#' recipeLoad("gencode_annotation", return=TRUE)
#' inputs(gencode_annotation)
#' rm(gencode_annotation)
#'
#' #########################
#' ## Load multiple recipes
#' #########################
#' 
#' rcphub <- recipeSearch("gencode")
#' recipeNames(rcphub)
#' recipeLoad(recipeNames(rcphub), return=TRUE)
#' inputs(gencode_transcripts)

recipeLoad <- function(rcp = c(),
                       cachePath = "ReUseDataRecipe",
                       env = .GlobalEnv,
                       return = FALSE) {
    bfcpath <- Sys.getenv("cachePath")
    if(bfcpath != ""){
        cachePath <- file.path(bfcpath, "ReUseData")
    } else if(!file.exists(cachePath) & !grepl("^/", cachePath)){
        cachePath <- R_user_dir(cachePath, which="cache")
    }
    bfc <- BiocFileCache(cachePath, ask = FALSE)
    if (missing(rcp))
        stop("Please provide a valid name or filepath for the data recipe.")
    idx <- match(rcp, bfcinfo(bfc)$rname)
    idx <- idx[!is.na(idx)]
    if (length(idx)) {
        fpath <- bfcrpath(bfc)[idx]
    } else {
        isfile <- file.exists(rcp)
        if (any(isfile)) {
            fpath <- rcp[isfile]
        } else {
            stop("Please provide a valid name or filepath for the data recipe.")
        }
    }
    if (!return) {
        if (length(fpath) > 1)
            stop("Use recipeLoad(return=TRUE) to load multiple recipes",
                 " and do not assign new names!")
        message("Note: you need to assign a name for the recipe!")
        .load_rcp(rscript = fpath, env = env, return = return)
    } else {
        for (i in seq(length(fpath))) {
            .load_rcp(rscript = fpath[i], env = env, return = return)        
        }
    }
}
