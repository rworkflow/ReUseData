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
#' @param BPPARAM The options for `BiocParallel::bpparam`.
#' @param ... Arguments to be passed into `Rcwl:runCWL()`.
#' @return The data files and 4 meta files: `.cwl`: The cwl script
#'     that was internally run to get the data; `.yml`: the input
#'     parameter values for the data recipe and user specified data
#'     annotation notes, versions etc; `.sh`: The script for data
#'     processing; `.md`: checksum file to verify the integrity of
#'     generated data files.
#' @importFrom Rcwl runCWL
#' @importFrom tools md5sum
#' @importFrom basilisk basiliskStart basiliskStop
#' @importFrom Rcwl env_Rcwl
#' @export
#' @examples
#' library(Rcwl)
#' outdir <- file.path(tempdir(), "SharedData")
#'
#' ## Example 1
#' echo_out <- recipeLoad("echo_out")
#' Rcwl::inputs(echo_out)
#' echo_out$input <- "Hello World!"
#' echo_out$outfile <- "outfile"
#' res <- getData(echo_out,
#'                outdir = outdir,
#'                notes = c("echo", "hello", "world", "txt"),
#'                showLog = TRUE)
#'
#' # Example 2
#' \dontrun{
#' ensembl_liftover <- recipeLoad("ensembl_liftover")
#' Rcwl::inputs(ensembl_liftover)
#' ensembl_liftover$species <- "human"
#' ensembl_liftover$from <- "GRCh37"
#' ensembl_liftover$to <- "GRCh38"
#'
#' res <- getData(ensembl_liftover,
#'         outdir = outdir, 
#'         notes = c("ensembl", "liftover", "human", "GRCh37", "GRCh38"),
#'         showLog = TRUE)
#' dir(outdir)
#' }

getData <- function(rcp, outdir, prefix = NULL, notes = c(), conda = FALSE,
                    BPPARAM = NULL, ...){
    if(is.null(prefix)){
        if(length(rcp@label) > 0){
            rcp_name <- gsub(" ", "_", rcp@label)
        }else{
            rcp_name <- deparse(substitute(rcp))
        }
        xn <- lapply(inputs(rcp), function(x){
            if(length(x@value) == 1 && grepl("http|ftp", x@value)){
                basename(x@value)
            }else if(is(x@value, "list")){
                basename(x@value$path)
            }else{
                gsub(" ", "_", x@value)
            }
        })
        xn <- do.call(paste, list(xn, collapse="_"))
        prefix <- paste(rcp_name, xn, sep="_")  ## "rcp_inputs.xx" 
    }
    ## check cwltool version
    if(file.exists(Sys.which("cwltool"))){
        cwlver <- system2("cwltool", "--version", stdout = TRUE)
        cwlver <- as.numeric(sub("\\.[0-9]*$", "", sub(".* ", "", cwlver)))
        if(cwlver < 3.1){
            cl <- basiliskStart(env_Rcwl)
            on.exit(basiliskStop(cl))
        }
    }

    if(is.null(BPPARAM)){
        res <- runCWL(cwl = rcp, outdir = outdir,
                      yml_prefix = prefix,
                      yml_outdir = outdir,
                      conda = conda,
                      ...)
    }else{
        res <- runCWLBP(cwl = rcp, outdir = outdir,
                      yml_prefix = prefix,
                      yml_outdir = outdir,
                      conda = conda, BPPARAM = BPPARAM,
                      ...)
    }
    ## if no output generated: 
    if (is.null(res$output)) {
        ## file.remove(file.path(outdir, paste0(prefix, ".yml")))
        stop("HINT: The output file was not successfully generated. ",
             "Please check the recipe (output globbing pattern, ",
             "input parameters, parameter types, etc.)\n")
    }
    yfile <- file.path(outdir, paste0(prefix, ".yml"))
    notes <- paste(notes, collapse = " ")
    apd <- c(paste("# output:", res$output),
             paste("# notes:", notes),
             paste("# date:", Sys.time()))
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

runBatch <- function(idx, libs, fun, ...){
    lapply(libs, library, character.only = TRUE)
    fun(...)
}
