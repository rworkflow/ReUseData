
getDataBatch <- function(cwl, outdir, prefix, version = "",
                            notes = "", docker = TRUE,
                            BPPARAM = SerialParam(), ...){
    res <- bplapply(1, runBatch, libs = c("Rcwl", "tools"),
                    fun = getData,
                    cwl = cwl,
                    outdir = outdir,
                    prefix = prefix,
                    version = version,
                    notes = notes,
                    docker = docker,
                    BPPARAM = BPPARAM, ...)
    return(res[[1]])
} 

#' getData: Generate the curated dataset from an provided data recipe.
#' 
#' @param cwl the `cwlProcess` object from `searchData()` or
#'     `loadData()`.
#' @param outdir The directory to store the outputs. Will
#'     automatically create if not exist or provided.
#' @param prefix The string that labels the data which will be added
#'     as prefix to the 4 output files. "gencode_human_38".
#' @param version User specified version for the data generated for
#'     local data management. Takes string. Default is "". When
#'     provided, it will be pasted with `prefix` argument using "_" to
#'     serve as the final prefix for output files. E.g.,
#'     "gencode_human_38_v1.0".
#' @param notes User assigned notes/keywords to retrieve the data by
#'     `searchData()` function.  e.g., "human", "reference genome",
#'     "gencode", "gtf", etc. Multiple notes will be pasted using blank space. 
#' @param docker Default is TRUE.
#' @param ... Arguments to be passed into `Rcwl:runCWL()`.
#' @return The data files and 4 meta files: `.cwl`: The cwl script
#'     that was internally run to download the data; `.yml`: the
#'     values for the `cwlProcess` object, and some user added notes,
#'     versions etc. `.sh`: The script for data downloading and
#'     curation.  `.md`: ... (to-be-filled)
#' @importFrom Rcwl runCWL
#' @importFrom tools md5sum
#' @export
## #' @examples

getData <- function(cwl, outdir, prefix, version = "", notes = "", docker = TRUE, ...){
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
    ## if no output generated: 
    if (is.null(res$output)) {
        ## allfiles <- list.files(outdir, pattern=paste0(prefix, "."), full.names=TRUE)
        file.remove(file.path(outdir, paste0(prefix, ".yml")))
        stop(paste0("HINT: The output file was not successfully generated. ",
                    "Please check the recipe (output globbing pattern, ",
                    "input parameters, parameter types, etc.)\n",
                    "Temporary yaml file was deleted ",
                    "to avoid future error in dataSearch():",
                    file.path(outdir, paste0(prefix, ".yml"))))
        }
    yfile <- file.path(outdir, paste0(prefix, ".yml"))
    notes <- paste(notes, collapse = " ")
    apd <- c(paste("# output:", res$output),
             paste("# notes:", notes),
             paste("# version:", version),
             paste("# date:", Sys.Date()))
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
