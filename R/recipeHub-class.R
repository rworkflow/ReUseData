################
## recipeHub
################

#' recipeHub
#'
#' `recipeHub` class, constructor, and methods. 
#' @rdname recipeHub-class
#' @export
recipeHub <- setClass("recipeHub", contains = "cwlHub")

#' @rdname recipeHub-class
#' @param BFC A BiocFileCache object created for recipe and recipes.
#' @return recipeHub: a `recipeHub` object.
#' @importClassesFrom RcwlPipelines cwlHub
#' @export
recipeHub <- function(BFC){
    cwlh <- RcwlPipelines::cwlHub(BFC)
    new("recipeHub", cwlh)
}

## Methods
## 'mcols()' inherited from cwlHub, returns a DataFrame.
## 'title()' inherited to return the 'rname' column in mcols(cwlHub),
## here use `dataNames()`
## '[' inherited to return the subset of cwlHub with cwlHub["BFCid"]. 

#' @rdname recipeHub-class
#' @param object The `recipeHub` object
#' @importFrom S4Vectors mcols get_showHeadLines get_showTailLines
#' @importMethodsFrom RcwlPipelines mcols
#' @exportMethod show
#' @examples
#' rcps <- recipeSearch(c("gencode"))
#' rcp1 <- rcps[1]
#' recipeNames(rcp1)

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
                      dimnames=list(rownames, c("", "name")))
        cat("\n")
        print(out, quote=FALSE, right=FALSE)
    }
})

#' subset dataHub
#' @rdname recipeHub-class
#' @param x The `recipeHub` object
#' @param i The integer index of the `recipeHub` object
#' @return [: A `recipeHub` object that was subsetted.
#' @export
setMethod("[", c("recipeHub"), function(x, i) {
    rids <- x@rid[i]
    return(x[rids])
})
## setGeneric("[")

#' @rdname recipeHub-class
#' @return recipeNames: the recipe names for the `recipeHub` object.
#' @export
recipeNames <- function(object){
    mcols(object)$rname
}
