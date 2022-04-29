## This script is modified from "RcwlPipelines::cwlLoad", but the "
## FIXME: update ".load_rcp" when multiple "cwlProcess" objects matched. 
## FIXME: update ".load_rcp_URL" when supporting url as input for recipeLoad().
## FIXME: update ".load_rcp_git" when supporting git repo as input for recipeLoad().

.load_rcp <- function(rscript, env = .GlobalEnv, return = FALSE){
    .env <- new.env()
    source(rscript, .env)
    objs <- ls(.env)
    oidx <- sapply(objs,
                   function(x)is(get(x, envir = .env), "cwlProcess"))
    if(sum(oidx)==1){
        idx <- oidx
    }else if(sum(oidx)>1){
        oclass <- sapply(objs,
                         function(x)class(get(x, envir = .env)))
        idx <- oclass == "cwlWorkflow"  ## FIXME: need update, test for cases with multiple "cwlProcess" objects
        if(sum(idx)==0) idx <- tail(which(oidx), 1)
        oidx <- oidx & !idx

        for(i in seq(sum(oidx))){
            assign(objs[oidx][i],
                   get(objs[oidx][i], envir = .env),
                   envir = env)
            message(objs[oidx][i], " loaded")
        }
    }
    if(return){
        get(objs[idx], envir = .env)
    }else{
        assign(objs[idx],
               get(objs[idx], envir = .env),
               envir = env)
        message(objs[idx], " loaded")
    }
}

.load_rcp_URL <- function(url){
    cwlpath <- paste0(tempfile(), ".cwl")  
    download.file(url, cwlpath)
    read_rcp(cwlpath)
}

#' @importFrom git2r clone
.load_rcp_git <- function(repo, cwlfile, dir = tempdir(), ...){
    github_repo <- paste0("https://github.com/", repo)
    localdir <- file.path(dir, basename(github_repo))
    clone(github_repo, local_path = localdir, ...)

    cfile <- file.path(localdir, cwlfile)
    stopifnot(file.exists(cfile))
    readCWL(cfile)
}

#' recipeLoad
#' 
#' To load a data recipe into R environment.
#' @param rname The name or filepath of data recipe (`rname` or
#'     `fpath` column from the `bfc` object returned from
#'     `recipeSearch`). It can also be an url or a github repo.
## #' @param bfc The `BiocFileCache` object for the data recipes. The
## #'     default is NULL which automatically detect the "ReUseData" cache
## #'     directory.

#' @param cachePath The cache path of the BiocFileCache object to
#'     store the ReUseData recipes.
#' @param env The R enviroment to export to. The default is
#'     `.GlobalEnv`.
## #' @param rcpfile For github repo input, The relative path of a data
## #'     recipe file inside of the github repo.
## #' @param dir For github repo input, the directory to clone the repo.
## #' @param ... More options from git2r::clone.

#' @return A `cwlProcess` object. For advanced/composite data recipes,
#'     the basic data recipes involved will also be loaded.

## #' @details Note to developers that the dependent Rcwl scripts should
## #'     be included in the recipe with `@include` tag.

#' @import methods
#' @export
#' @examples
#' \dontrun{
#' rcp <- recipeSearch("gencode")
#' rcp$rname
#' rcp$rpath
#' gtf <- recipeLoad("gencode_gtf")
#' gtf <- recipeLoad(rcp$rpath[rcp$rname == "gencode_gtf"])  ## equivalent
#' gtf
#' }

## recipeLoad <- function(rname, bfc = NULL, env = .GlobalEnv,
##                        rcpfile = NULL, dir = tempdir(), ...) {
recipeLoad <- function(rname, cachePath = "ReUseDataRecipe", env = .GlobalEnv) {
    ## if(grepl("http", rname) & grepl("\\.cwl$", rname)){
    ##     .load_rcp_URL(rname)
    ## }else if(!file.exists(rname) && grepl("\\/", rname)){
    ##     .load_rcp_git(rname, rcpfile = rcpfile, dir = dir, ...)
    ## }else{
    bfcpath <- Sys.getenv("cachePath")
    if(bfcpath != ""){
        cachePath <- file.path(bfcpath, "ReUseData")
    } else if(!file.exists(cachePath) & !grepl("^/", cachePath)){
    ## }else if(is.null(bfc)){
        cachePath <- R_user_dir(cachePath, which="cache")
    }
    bfc <- BiocFileCache(cachePath, ask = FALSE)
    if (missing(rname))
        stop("Please provide a valid name or filepath for the data recipe.")
    idx <- match(rname, bfcinfo(bfc)$rname)
    if (!is.na(idx)) {
        fpath <- bfcrpath(bfc)[idx]
    } else {
        if (file.exists(rname)){
            fpath <- rname
        } else {
            stop("Please provide a valid name or filepath for the data recipe.")
        }
    }
    ## scripts <- readLines(fpath)
    ## iscripts <- grep("@include", scripts, value = TRUE)
    ## if(length(iscripts) > 0){
    ##     rscripts <- grep(".R$",
    ##                      unlist(strsplit(iscripts, split = " ")),
    ##                      value = TRUE)
    ##     if(length(rscripts) > 0){
    ##         sapply(rscripts, function(x){
    ##             rscript <- file.path(dirname(fpath), x)
    ##             if(any(grepl("cwlWorkflow", readLines(rscript)))){
    ##                 cwlInstall(rscript, bfc = bfc, env = env)
    ##             }else{
    ##                 .load_rcp(rscript, env)
    ##             }
    ##         })
    ##     }
    ## }
    .load_rcp(fpath, env, return = TRUE)        
    ## }
}
