################
## recipeHub
################

#' dataHub
#'
#' `dataHub` class, constructor, and methods. 
#' @rdname dataHub-class 
#' @exportClass dataHub
dataHub <- setClass("dataHub", contains = "cwlHub")

#' @rdname dataHub-class
#' @param BFC A BiocFileCache object created for data and recipes.
#' @return dataHub: a `dataHub` object.
#' @importClassesFrom RcwlPipelines cwlHub
#' @export
dataHub <- function(BFC){
    cwlh <- RcwlPipelines:::cwlHub(BFC)
    new("dataHub", cwlh)
}

## Methods
## 'mcols()' inherited from cwlHub, returns a DataFrame with all info including bfcmeta(bfc, "dataMeta"). 
## 'title()' inherited to return the 'rname' column in mcols(cwlHub).
## '[' inherited to return the subset of cwlHub with cwlHub["BFCid"]. 

#' @rdname dataHub-class
#' @param x A `dataHub` object
#' @importFrom S4Vectors mcols get_showHeadLines get_showTailLines
#' @importMethodsFrom RcwlPipelines mcols
#' @exportMethod show

setMethod("show", "dataHub", function(object){
    rid <- object@rid
    mc <- mcols(object)

    cat("dataHub with", length(rid), "records\n")
    cat("cache path: ", bfccache(object), "\n")
    ## mdate <- tail(sort(as.Date(mc$mtime)), 1)
    ## cat("# last modified date: ", as.character(mdate), "\n")
    cat("# dataSearch() to query a specific dataset\n")
    cat("# dataUpdate() to update the local data cache\n")
    cat("# additional mcols(): rid, rpath, params, Notes, Versoin, Date, ...\n")
    ## https://github.com/Bioconductor/AnnotationHub/blob/master/R/Hub-class.R#L602
    .some <-
        function(elt, nh, nt, fill="...", width=getOption("width") - 13L)
    {
        answer <- if (length(elt) < nh + nt + 1L)
                      elt
                  else
                      c(head(elt, nh), fill, tail(elt, nt))
        ifelse(nchar(answer) > width,
               sprintf("%s...", substring(answer, 1L, width-3L)),
               answer)
    }
    if (length(rid) > 0) {
        nhead <- get_showHeadLines()
        ntail <- get_showTailLines()
        rownames <- paste0("  ", .some(rid, nhead, ntail))
        out <- matrix(c(.some(rep("|", length(rid)), nhead, ntail, fill=""),
                        .some(mc$rname, nhead, ntail),
                        ## .some(mc$params, nhead, ntail),
                        .some(mc$fpath, nhead, ntail)),
                      ncol=3L,
                      ## dimnames=list(rownames, c("", "title", "params", "Path")))
                      dimnames=list(rownames, c("", "title", "Path")))
        cat("\n")
        print(out, quote=FALSE, right=FALSE)
    }
})

#' @rdname dataHub-class
#' @return dataParams: the "parameter" values for the `dataHub` object.
#' @export
title <- function(object){
    mcols(object)$rname
}

#' @rdname dataHub-class
#' @return dataParams: the "parameter" values for the `dataHub` object.
#' @export
dataParams <- function(object){
    mcols(object)$params
}

#' @rdname dataHub-class
#' @return dataNotes: the "notes" for the `dataHub` object.
#' @export
dataNotes <- function(object){
    mcols(object)$notes
}

#' @rdname dataHub-class
#' @return dataPath: the file path to the `dataHub` object.
#' @export
dataPath <- function(object){
    bfcinfo(object)$fpath
}


################
## recipeHub
################

#' recipeHub
#'
#' `recipeHub` class, constructor, and methods. 
#' @rdname recipeHub-class 
#' @exportClass recipeHub
#' 
recipeHub <- setClass("recipeHub", contains = "cwlHub")

#' @rdname recipeHub-class
#' @param BFC A BiocFileCache object created for recipe and recipes.
#' @return recipeHub: a `recipeHub` object.
#' @importClassesFrom RcwlPipelines cwlHub
#' @export
#' 
recipeHub <- function(BFC){
    cwlh <- RcwlPipelines::cwlHub(BFC)
    new("recipeHub", cwlh)
}

## Methods
## 'mcols()' inherited from cwlHub, returns a DataFrame.
## 'title()' inherited to return the 'rname' column in mcols(cwlHub).
## '[' inherited to return the subset of cwlHub with cwlHub["BFCid"]. 

#' @rdname recipeHub-class
#' @param x A `recipeHub` object
#' @importFrom S4Vectors mcols get_showHeadLines get_showTailLines
#' @importMethodsFrom RcwlPipelines mcols
#' @exportMethod show

setMethod("show", "recipeHub", function(object){
    rid <- object@rid
    mc <- mcols(object)

    cat("recipeHub with", length(rid), "records\n")
    cat("cache path: ", bfccache(object), "\n")
    cat("# recipeSearch() to query specific recipes using multipe keywords\n")
    cat("# recipeUpdate() to update the local recipe cache\n")
    ## https://github.com/Bioconductor/AnnotationHub/blob/master/R/Hub-class.R#L602
    .some <-
        function(elt, nh, nt, fill="...", width=getOption("width") - 13L)
    {
        answer <- if (length(elt) < nh + nt + 1L)
                      elt
                  else
                      c(head(elt, nh), fill, tail(elt, nt))
        ifelse(nchar(answer) > width,
               sprintf("%s...", substring(answer, 1L, width-3L)),
               answer)
    }
    if (length(rid) > 0) {
        nhead <- get_showHeadLines()
        ntail <- get_showTailLines()
        rownames <- paste0("  ", .some(rid, nhead, ntail))
        out <- matrix(c(.some(rep("|", length(rid)), nhead, ntail, fill=""),
                        .some(mc$rname, nhead, ntail)
                        ),
                      ncol=2L,
                      dimnames=list(rownames, c("", "title")))
        cat("\n")
        print(out, quote=FALSE, right=FALSE)
    }
})

#' subset dataHub
#' @rdname dataHub-class
#' @param x A `dataHub` object.
#' @param i The integer index of the `dataHub` object.
#' @export
setMethod("[", c("dataHub"), function(x, i) {
    rids <- x@rid[i]
    return(x[rids])
})
setGeneric("[")

#' combine dataHub
#' @rdname dataHub-class
#' @param x A `dataHub` object to be combined.
#' @param ... More `dataHub` objects to combine.
#' @export
setMethod("c", c("dataHub"), function(x, ...) {
    object <- list(x, ...)
    rids <- unlist(lapply(object, function(x)x@rid))
    x@rid <- unique(rids)
    return(x)
})
setGeneric("c")

#' dataHub to list
#' @rdname dataHub-class
#' @param x A `dataHub` object.
#' @param type The type of workflow input list, such as cwl.
#' @export
toList <- function(x, type = NULL, format = "list"){
    tl <- title(x)
    pth <- dataPath(x)
    if(!is.null(type) && type == "cwl"){
        dtype <- unlist(lapply(pth, function(x)file.info(x)$isdir))
        dtype <- ifelse(dtype, "Directory", "File")
        dl <- vector("list", length(pth))
        for(i in 1:length(pth)){
            dl[[i]] <- list(class = dtype[i],
                            path = pth[i])
        }
    }else {
        dl <- as.list(dataPath(x))
    }
    names(dl) <- title(x)
    if (format == "json") {
        dl <- jsonlite::toJSON(dl, pretty = TRUE, auto_unbox = TRUE)
    } else if (format == "yml") {
        dl <- yaml::as.yaml(dl)
    } 
    return(dl)
}
