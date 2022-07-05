#' tag
#' 
#' Show tag column of a `dataHub` object.
#' @rdname tag
#' @param object A `dataHub` object.
#' @export
setGeneric("tag", function(object)standardGeneric("tag"))

#' @rdname tag
setMethod("tag", "dataHub", function(object) {
    mcols(object)$tag
})


## tag <- function(object){
##     mcols(object)$tag
## }

#' @rdname tag
#' @param object A `dataHub` object.
#' @param value The value(s) to assign to the `dataHub` object.
#' @param append Whether to append new tag or replace all tags.
#' @export
setGeneric("tag<-", function(object, append=TRUE, value)standardGeneric("tag<-"))

#' @rdname tag
setReplaceMethod("tag", "dataHub", function(object, append=FALSE, value){
    if(append){
        value <- paste0(tag(object), value)
    }
    dm <- bfcmeta(object, "dataMeta")
    idx <- match(object@rid, dm$rid)
    dm$tag[idx] <- value
    bfc <- BiocFileCache(object@cache, ask = FALSE)
    bfcmeta(bfc, "dataMeta", overwrite = TRUE) <- dm
    return(object)
})

## "tag<-" <- function(object, value, append = TRUE){
##     ## mcols(dat1)@listData$tag <- value
##     if(append){
##         value <- paste0(tag(object), value)
##     }
##     dm <- bfcmeta(object, "dataMeta")
##     idx <- match(object@rid, dm$rid)
##     dm$tag[idx] <- value
##     bfc <- BiocFileCache(object@cache, ask = FALSE)
##     bfcmeta(bfc, "dataMeta", overwrite = TRUE) <- dm
##     return(object)
## }

#' show tags
#'
#' Show a data.frame of file paths and tags.
#' @rdname tags
#' @param object A `dataHub` object.
#' @export
setGeneric("tags", function(object)standardGeneric("tags"))

#' @rdname tags
setMethod("tags", "dataHub", function(object) {
    mcols(object)[, c("fpath", "tag")]
})

#' add tags
#' 
#' @rdname tags
#' @param value A data.frame of fpath and tags to update the metadata.
#' @export
setGeneric("tags<-", function(object, value)standardGeneric("tags<-"))

#' @rdname tags
setReplaceMethod("tags", "dataHub", function(object, value){
    dm <- bfcmeta(object, "dataMeta")
    rids <- object@rid[match(value$fpath, dataPath(object))]
    dm$tag[match(rids, dm$rid)] <- value$tag
    bfc <- BiocFileCache(object@cache, ask = FALSE)
    bfcmeta(bfc, "dataMeta", overwrite = TRUE) <- dm
    return(object)
})
