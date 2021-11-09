
runCWLDataBatch <- function(cwl, outdir, prefix, version = "",
                            notes = "", docker = TRUE,
                            BPPARAM = SerialParam(), ...){
    res <- bplapply(1, runBatch, libs = c("Rcwl", "tools"),
                    fun = runCWLData,
                    cwl = cwl,
                    outdir = outdir,
                    prefix = prefix,
                    version = version,
                    notes = notes,
                    docker = docker,
                    BPPARAM = BPPARAM, ...)
    return(res[[1]])
} 

#' @param cwl the `cwlProcess` object from `searchData()` or
#'     `loadData()`.
#' @param outdir The directory to store the outputs. Will
#'     automatically create if not exist.
#' @param prefix The string that labels the data which will be added
#'     as prefix to the 4 output files. e.g., "gencode_human_38".
#' @param version User assigned version for the data generated. e.g.,
#'     "38", "v1.0", etc.
#' @param notes User assigned notes/keywords to retrieve the data by
#'     `searchData()` function.  e.g., "human", "reference genome",
#'     "gencode", "gtf", etc.
#' @param docker Default is TRUE.
#' @param ... Arguments to be passed into `Rcwl:runCWL()`.
#' @return The data files and 4 meta files: `.cwl`: The cwl script
#'     that was internally run to download the data; `.yml`: the
#'     values for the `cwlProcess` object, and some user added notes,
#'     versions etc. `.sh`: The script for data downloading and
#'     curation.  `.md`: ... (to-be-filled)
#' @importFrom tools md5sum
#' @export
#' @example
#' 
runCWLData <- function(cwl, outdir, prefix, version = "", notes = "", docker = TRUE, ...){
    if(docker == "singularity"){
        reqclass <- unlist(lapply(requirements(cwl), function(x)x$class))
        idx <- match("DockerRequirement", reqclass)
        iid <- requirements(cwl)[[idx]]$dockerImageId
        if(!is.null(iid)){
            requirements(cwl)[[idx]] <- requireDocker(iid)
        }
    }
    prefix <- sub("_$", "", paste0(prefix, "_", version))
    res <- runCWL(cwl = cwl, outdir = outdir,
                  yml_prefix = prefix,
                  yml_outdir = outdir,
                  docker = docker, ...)
    yfile <- file.path(outdir, paste0(prefix, ".yml"))
    notes <- paste(notes, collapse = " ")
    apd <- c(paste("# Output:", res$output),
             paste("# Notes:", notes),
             paste("# Version:", version),
             paste("# Date:", Sys.Date()))
    write(apd, yfile, append = TRUE)
    lapply(requirements(cwl), function(x){
        if(x$class == "InitialWorkDirRequirement" &&
           x$listing[[1]]$entryname == "script.sh"){
            write(x$listing[[1]]$entry, file.path(outdir, paste0(prefix, ".sh")))
        }
    })
    md5 <- md5sum(res$output)
    md5 <- cbind(md5, basename(names(md5)))
    write.table(md5, file.path(outdir, paste0(prefix, ".md5")),
                col.names = FALSE, row.names = FALSE, quote = FALSE)
    res$yml <- yfile
    return(res)
}

docker2sif <- function(Dockerfile, sif, buildArgs = ""){
    if(!file.exists(Dockerfile) & grepl("FROM", Dockerfile)){
        df <- tempfile()
        write(Dockerfile, df)
        Dockerfile <- df
    }
    cmd1 <- paste("spython recipe --parser docker", Dockerfile, paste0(Dockerfile, ".snowflake"))
    message(cmd1)
    system(cmd1)
    cmd2 <- paste("singularity build", buildArgs, sif, paste0(Dockerfile, ".snowflake"))
    message(cmd2)
    system(cmd2)
}

runBatch <- function(idx, libs, fun, ...){
    lapply(libs, library, character.only = TRUE)
    fun(...)
}
