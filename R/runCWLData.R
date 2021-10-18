
runCWLDataBatch <- function(cwl, outdir, prefix, version = "", notes = "", docker = TRUE, BPPARAM = SerialParam(), ...){
    res <- bplapply(1,
             runBatch,
             libs = c("Rcwl", "tools"),
             fun = runCWLData,
             cwl = cwl,
             outdir = outdir,
             prefix = prefix,
             version = version,
             notes = notes,
             docker = docker,
             BPPARAM = BPPARAM,
             ...)
    return(res[[1]])
}
library(tools)
#' @importFrom tools md5sum
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
    notes <- paste(paste(notes, collapse = " "), version, Sys.Date())
    apd <- c(paste("# output:", res$output),
             paste("# notes:", notes))
    write(apd, yfile, append = TRUE)
    lapply(requirements(cwl), function(x){
        if(x$class == "InitialWorkDirRequirement" &&
           x$listing[[1]]$entryname == "script.sh"){
            write(x$listing[[1]]$entry, file.path(outdir, paste0(prefix, ".sh")))
        }
    })
    md5 <- md5sum(res$output)
    md5 <- cbind(md5, names(md5))
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
