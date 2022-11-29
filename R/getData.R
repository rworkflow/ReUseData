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

#' getData
#'
#' Evaluation of data recipes to generate curated dataset of interest. 
#' 
#' @param cwl the data recipe in `cwlProcess` S4 class.
#' @param outdir Character string specifying the directory to store
#'     the output files. Will automatically create if not exist or
#'     provided.
#' @param prefix Character string that labels the data which will be
#'     added as prefix to the automatically generated annotation and
#'     workflow files. e.g., "gencode_human_38".
#' @param version Character string of user specified version for the
#'     data generated for local data management. Default is
#'     empty. When provided, it will be pasted with `prefix` argument
#'     using "_" to serve as the final prefix for output files. E.g.,
#'     "gencode_human_38_v1.0".
#' @param notes User assigned notes/keywords to annotate the data and
#'     be used for keywords matching in `dataSearch(keywords = )`.
#' @param docker Whether to use docker when evaluating the data recipe
#'     as a CWL workflow. Default is TRUE.
#' @param ... Arguments to be passed into `Rcwl:runCWL()`.
#' @return The data files and 4 meta files: `.cwl`: The cwl script
#'     that was internally run to get the data; `.yml`: the input
#'     parameter values for the data recipe and user specified data
#'     annoation notes, versions etc; `.sh`: The script for data
#'     processing; `.md`: checksum file to verify the integrity of
#'     generated data files.
#' @importFrom Rcwl runCWL
#' @importFrom tools md5sum
#' @export 
#' @examples
#' rcp <- recipeLoad("gencode_transcripts")
#' rcp$species <- "human"
#' rcp$version <- "42"
#' outdir <- file.path(tempdir(), "SharedData")
#' res <- getData(rcp,
#'         outdir = outdir, 
#'         prefix = "gencode_annotation_human_42",
#'         notes = c("gencode", "human", "42"),
#'         showLog = TRUE)
#' dir(outdir)
#' 

getData <- function(cwl, outdir, prefix, version = character(), notes = c(),
                    docker = TRUE, ...){
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
        ## file.remove(file.path(outdir, paste0(prefix, ".yml")))
        stop(paste0("HINT: The output file was not successfully generated. ",
                    "Please check the recipe (output globbing pattern, ",
                    "input parameters, parameter types, etc.)\n"))
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
