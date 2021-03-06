#' meta_data
#' 
#' Functions to generate the meta csv file for local cached dataset.
#' @param dir The path to the shared data folder.
#' @return a `data.frame` with yml file name, parameter values, data
#'     file paths, date, and user-specified notes, version when
#'     generating the data with `getData()`. 
#' @examples
#' ## meta_data("../SharedData")

meta_data <- function(dir = "") {
    ymls <- list.files(dir, pattern = ".yml", full.names = TRUE, recursive = TRUE)
    dnames <- sub(".yml$", "", basename(ymls))  ## file name.

    out <- c()
    for (i in seq_along(dnames)) {
        res <- readLines(ymls[i])

        ## read parameter values specific to data
        params <- res[!grepl("^#", res)]
        params <- paste(params, collapse = "; ")

        ## read standard output from YAML
        keys <- c("output", "notes", "version", "date", "tag")
        val <- data.frame(yml = ymls[i], params = params)
        for (key in keys) {
            keyfull <- paste0("# ", key, ": ")
            kval <- res[grep(keyfull, res)]
            kval <- sub(keyfull, "", kval)
            if (length(kval) == 0) kval = ""
            val <- data.frame(val, kval) ## multiple output files return multiple records.
        }
        out <- rbind(out, val)
    }
    colnames(out) <- c("yml", "params", keys)
    out
}

## generate a csv file for all available recipes on the GitHub repository.

## meta_recipe <- function(cachePath = "ReUseData") {
##     cachePath <- R_user_dir(cachePath, which = "cache")
##     bfc <- bfcFileCache(cachePath, ask = FALSE)
##     bfc
## }

