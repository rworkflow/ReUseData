#' search recipes on GitHub repository
#'
#' @param keywords character vector of keywords to be matched to the
#'     local datasets. It matches the "notes" when generating the data
#'     using `getData(notes = )`. Keywords can be a tag with the data
#'     in `#tag` format. If not specified, function returns the full
#'     data list.
#' @param cachePath ReUseData by default
#' @return a `dataHub` object containing the information about local
#'     data cache, e.g., data name, data path, etc.
#' @examples
#' ## dataSearch()
#' ## dataSearch(c("gencode", "human", "38"))
#' @@ dataSearch("#samtools")
#' @export
#' 
dataSearch <- function(keywords=character(), cachePath = "ReUseData") {

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

    if (missing(keywords))
        keywords <- ""
    res <- bfcquery(bfc, query = keywords,
                    field = c("rname", "fpath", "params", "notes", "version", "date", "tag"),
                    ignore.case = TRUE)
    if(any(grepl("#", keywords))){
        res <- bfcquery(bfc, query = keywords, field = c("tag"), ignore.case = TRUE)
    }
    dataHub(bfc[res$rid])
}
