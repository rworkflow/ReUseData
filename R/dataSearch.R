#' dataSearch

#' search data in local data caching system
#'
#' @param keywords character vector of keywords to be matched to the
#'     local datasets. It matches the "notes" when generating the data
#'     using `getData(notes = )`. Keywords can be a tag with the data
#'     in `#tag` format. If not specified, function returns the full
#'     data list.
#' @param cachePath A character string for the data cache. Must
#'     match the one specified in `dataUpdate()`. Default is
#'     "ReUseData".
#' @return a `dataHub` object containing the information about local
#'     data cache, e.g., data name, data path, etc.
#' @import BiocFileCache 
#' @export
#' @examples
#' \dontrun{
#' dataSearch()
#' dataSearch(c("gencode")) 
#' dataSearch("#gatk")
#' }

dataSearch <- function(keywords=character(), cachePath = "ReUseData") {
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

    if (missing(keywords))
        keywords <- ""
    res <- bfcquery(bfc, query = keywords,
                    field = c("rname", "fpath", "params", "notes", "date", "tag"),
                    ignore.case = TRUE)
    if(any(grepl("#", keywords))){
        res <- bfcquery(bfc, query = keywords, field = c("tag"), ignore.case = TRUE)
    }
    dataHub(bfc[res$rid])
}
