#' dataHub
#'
#' `dataHub` class, constructor, and methods. 
#' @rdname dataHub-class 
#' @exportClass dataHub
#' 
dataHub <- setClass("dataHub", contains = "cwlHub", slots = c(path = "character", params="character"))

#' @rdname dataHub-class
#' @param BFC A BiocFileCache object created for data and recipes.
#' @return dataHub: a `dataHub` object.
#' @importClassesFrom RcwlPipelines cwlHub
#' @export
#' 
dataHub <- function(BFC){
    cwlh <- RcwlPipelines:::cwlHub(BFC)
    path <- bfcinfo(BFC)$fpath
    params <- bfcmeta(BFC, "dataMeta")$params  ## FIXME: don't really need to include "params" or "path"... 
    new("dataHub", cwlh, path = path, params = params)
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
                        .some(mc$params, nhead, ntail)),
                      ncol=3L,
                      dimnames=list(rownames, c("", "title", "Params")))
        cat("\n")
        print(out, quote=FALSE, right=FALSE)
    }
})

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
