#' annData
#'
#' Add annotation or meta information to existing data
#' @param path The data path to annotate.
#' @param notes User assigned notes/keywords to annotate the data and
#'     be used for keywords matching in `dataSearch(keywords = )`.
#' @param date The date of the data.
#' @param recursive Whether to annotate all data recursively.
#' @param skip Patter to skip files in the path.
#' @param md5 Whether to generate md5 values for all files.
#' @param force Whether to force regenerate meta.yml.
#' @param ... The other options from `list.files`
#' @export
annData <- function(path, notes, date=Sys.Date(),
                    recursive=TRUE, md5=FALSE,
                    skip="*.md|meta.yml", force=FALSE, ...){
    path <- normalizePath(path)
    files <- list.files(path, recursive=recursive,
                        full.names=TRUE, ...)
    files <- files[!grepl(skip, files)]
    if(length(files) == 0){
        files <- path
    }
    pms <- paste("path:", path)
    ots <- paste("# output:", files)
    nts <- paste("# notes:", notes)
    dts <- paste("# Date:", as.character(date))
    mfile <- file.path(path, "meta.yml")
    if(file.exists(mfile) & !force){
        message("meta.yml exists")
    }else{
        writeLines(c(pms, ots, nts, dts), mfile)
        message("meta.yml added")
    }

    if(md5){
        md5 <- md5sum(files)
        md5 <- cbind(md5, basename(names(md5)))
        write.table(md5, file.path(path, "meta.md5"),
                    col.names = FALSE, row.names = FALSE, quote = FALSE)
        message("meta.md5 added")
    }
    return(mfile)
}
