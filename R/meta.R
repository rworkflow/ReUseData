#' meta_data
#' 
#' Functions to generate the meta csv file for local cached dataset.
#' @param dir The path to the shared data folder.
#' @param cleanup If remove any invalid intermediate files. Default is
#'     FALSE. In cases one data recipe (with same parameter values)
#'     was evaluated multiple times, the same data file(s) will match
#'     to multiple intermeidate files (e.g., .yml). `cleanup` will
#'     remove older intermediate files, and only keep the most recent
#'     ones that matches the data file. When there are any
#'     intermediate files that don't match to any data file, `cleanup`
#'     will also remove those.
#' @param checkData check if the data (listed as "# output: " in the
#'     yml file) exists. If not, do not include in the output csv
#'     file. This argument is added for internal testing purpose.
#' @return a `data.frame` with yml file name, parameter values, data
#'     file paths, date, and user-specified notes when generating the
#'     data with `getData()`.
#' @export
#' @importFrom stats setNames
#' @examples
#' outdir <- file.path(tempdir(), "SharedData")
#' meta_data(outdir)

meta_data <- function(dir = "", cleanup = FALSE, checkData = TRUE) {
    ymls <- normalizePath(list.files(dir, pattern = ".yml", full.names = TRUE, recursive = TRUE))
    dnames <- sub(".yml$", "", basename(ymls))  ## file name.
    keys <- c("output", "notes", "date", "tag")

    if (!length(dnames)) {
        meta <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("yml", "params", keys))
    } else {
        meta <- c()
        for (i in seq_along(dnames)) {
            res <- readLines(ymls[i])
            
            ## read parameter values specific to data
            params <- res[!grepl("^#", res)]
            params <- paste(params, collapse = "; ")
            
            ## read standard output from YAML
            val <- data.frame(yml = ymls[i], params = params)
            for (key in keys) {
                keyfull <- paste0("# ", key, ": ")
                kval <- res[grep(keyfull, res)]
                kval <- sub(keyfull, "", kval)
                if (length(kval) == 0) kval = ""
                val <- data.frame(val, kval) ## multiple output files return multiple records.
            }
            meta <- rbind(meta, val)
        }
        colnames(meta) <- c("yml", "params", keys)

        ## if any data not exist (meta$output), then delete that record. 
        ind <- meta$output == "" | !file.exists(meta$output)
        if (any(ind)) {
            if (cleanup) {
                message("\nCleaning up invalid data records...")
                ymls_rm <- meta$yml[ind]
                dfrm <- data.frame(dir = dirname(ymls_rm), ptn = gsub(".yml", "", basename(ymls_rm)))
                apply(dfrm, 1, function(x) {file.remove(list.files(x[1], x[2], full.names = TRUE))})
            }
            if (checkData)
                meta <- meta[!ind, ]
        }
        
        ## if any duplicated (1 data matches multiple yml files), only keep the most recent ones.
        dup <- duplicated(meta$output)
        if (any(dup)) {
            yml_keep <- c()
            uniqd <- unique(meta$output)
            for (i in seq_along(uniqd)) {
                yml <- meta$yml[meta$output == uniqd[i]]
                ymld <- gsub("[[:alpha:]]|_", "", gsub(".yml$", "", basename(yml)))
                keep <- which(ymld == max(ymld))
                yml_keep <- c(yml_keep, yml[keep])
            }
            idx <- meta$yml %in% yml_keep
            ymls_rm <- meta$yml[!idx]
            ## remove older intermediate files
            if (cleanup) {
                dfrm <- data.frame(dir = dirname(ymls_rm), ptn = gsub(".yml", "", basename(ymls_rm)))
                apply(dfrm, 1, function(x) {file.remove(list.files(x[1], x[2], full.names = TRUE))})
            }
            meta  <- meta[meta$yml %in% yml_keep, ]
        }
        if (any(c(ind, dup)) && cleanup) {
            message("\nInvalid or older intermediate files are removed! Turn cleanup = FALSE to disable!") ## only print once! 
        }
    }
    meta
}
