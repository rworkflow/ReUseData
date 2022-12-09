#' getData
#'
#' Evaluation of data recipes to generate curated dataset of interest. 
#' 
#' @param rcp the data recipe in `cwlProcess` S4 class.
#' @param outdir Character string specifying the directory to store
#'     the output files. Will automatically create if not exist or
#'     provided.
#' @param prefix Character string specifying the file name of the
#'     annotation files (.yml, .cwl, .sh, .md5).
#' @param notes User assigned notes/keywords to annotate the data and
#'     be used for keywords matching in `dataSearch(keywords = )`.
#' @param conda Whether to use conda to install required software when
#'     evaluating the data recipe as a CWL workflow. Default is FALSE.
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
#' library(Rcwl)
#' recipeLoad("ensembl_liftover", return = TRUE)
#' Rcwl::inputs(ensembl_liftover)
#' ensembl_liftover$species <- "human"
#' ensembl_liftover$from <- "GRCh37"
#' ensembl_liftover$to <- "GRCh38"
#' outdir <- file.path(tempdir(), "SharedData")
#' res <- getData(ensembl_liftover,
#'         outdir = outdir, 
#'         notes = c("ensembl", "liftover", "human", "GRCh37", "GRCh38"),
#'         showLog = TRUE)
#' dir(outdir)
#' 

getData <- function(rcp, outdir, prefix = NULL, notes = c(), conda = FALSE, ...){
    if(is.null(prefix)){
        if(length(rcp@label) > 0){
            rcp_name <- gsub(" ", "_", rcp@label)
        }else{
            rcp_name <- deparse(substitute(rcp))
        }
        xn <- lapply(inputs(rcp), function(x){
            if(grepl("http|ftp", x@value)){
                basename(x@value)
            }else{
                gsub(" ", "_", x@value)
            }
        })
        xn <- do.call(paste, list(xn, collapse="_"))
        prefix <- paste(rcp_name, xn, sep="_")  ## "rcp_inputs.xx" 
   }
    res <- runCWL(cwl = rcp, outdir = outdir,
                  yml_prefix = prefix,
                  yml_outdir = outdir,
                  ...)
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
             paste("# date:", Sys.Date()))
    write(apd, yfile, append = TRUE)
    lapply(requirements(rcp), function(x){
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

## docker2sif <- function(Dockerfile, sif, buildArgs = ""){
##     if(!file.exists(Dockerfile) & grepl("FROM", Dockerfile)){
##         df <- tempfile()
##         write(Dockerfile, df)
##         Dockerfile <- df
##     }
##     cmd1 <- paste("spython recipe --parser docker", Dockerfile, paste0(Dockerfile, ".snowflake"))
##     message(cmd1)
##     system(cmd1)
##     cmd2 <- paste("singularity build", buildArgs, sif, paste0(Dockerfile, ".snowflake"))
##     message(cmd2)
##     system(cmd2)
## }

runBatch <- function(idx, libs, fun, ...){
    lapply(libs, library, character.only = TRUE)
    fun(...)
}
