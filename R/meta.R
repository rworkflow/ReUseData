#' meta_data()
#'
#' Functions to generate the meta csv file for local cached dataset.
#' @param dir The path to the data folder.
#' @param outdir The path the the output csv file.
#' @examples
#' meta_data("../res")
#' @return a csv file with columns: yml file name, Output path, Notes, Version, Date, etc. 

## todo: "dir" use cached path? "outdir"? 
## FIXME: multiple output files. 

meta_data <- function(dir = "", outdir = dir) {
    ymls <- list.files(dir, pattern = ".yml", full.names = TRUE)
    dnames <- sub(".yml$", "", basename(ymls))  ## file name.

    out <- c()
    for (i in seq_along(dnames)) {
        res <- readLines(ymls[i])

        ## read parameter values specific to data
        params <- res[!grepl("^#", res)]
        params <- paste(params, collapse = "; ")

        ## read standard output from YAML
        keys <- c("Output", "Notes", "Version", "Date")
        val <- c()
        for (key in keys) {
            keyfull <- paste0("# ", key, ": ")
            kval <- res[grep(keyfull, res)]
            kval <- sub(keyfull, "", kval)
            if (length(kval) == 0) kval = ""
            val <- c(val, kval)
        }
        out <- rbind(out, c(params, val))
        colnames(out) <- c("params", keys)
    }

    ## write csv file
    outdir <- file.path(outdir, "meta_data.csv")
    write.csv(out, file = outdir)
    return(out)
    cat(paste0("The meta file for available data is generated at: ",
               "\n", normalizePath(outdir), "\n"))
}

## generate a csv file for all available recipes on the GitHub repository.

## meta_recipe <- function(cachePath = "ReUseData") {
##     cachePath <- R_user_dir(cachePath, which = "cache")
##     bfc <- bfcFileCache(cachePath, ask = FALSE)
##     bfc
## }

## search in the "meta_data.csv" file, and return the file path
## ("Output" column) for the matched data.

