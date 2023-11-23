#' recipeMake
#'
#' Constructor function of data recipe
#' @rdname recipeMake
#' @param shscript character string. Can take either the file path to
#'     the user provided shell script, or directly the script content,
#'     that are to be converted into a data recipe.
#' @param paramID Character vector. The user specified parameter ID
#'     for the recipe.
#' @param paramType Character vector specifying the type for each
#'     `paramID`. One parameter can be of multiple types in
#'     list. Valid values are "int" for integer, "boolean" for
#'     boolean, "float" for numeric, "File" for file path, "File[]"
#'     for an array of files, etc. Can also take "double", "long",
#'     "null", "Directory". See details.
#' @param outputID the ID for each output.
#' @param outputType the output type for each output.
#' @param outputGlob the glob pattern of output files. E.g., "hg19.*".
#' @param requireTools the command-line tools to be used for data
#'     processing/curation in the user-provided shell script. The
#'     value here must exactly match the tool name. E.g., "bwa",
#'     "samtools", etc. A particular version of that tool can be
#'     specified in the format of "tool=version", e.g.,
#'     "samtools=1.3".
#' @return a data recipe in `cwlProcess` S4 class with all details
#'     about the shell script for data processing/curation, inputs,
#'     outputs, required tools and corresponding docker files. It is
#'     readily taken by `getData()` to evaluate the shell scripts
#'     included and generate the data locally. Find more details with
#'     `?Rcwl::cwlProcess`.
#' @importFrom Rcwl InputParam OutputParam InputParamList condaPackage
#'     OutputParamList
#' @details For parameter types, more details can be found here:
#'     "https://www.commonwl.org/v1.2/CommandLineTool.html#CWLType".
#'
#' `recipeMake` is a convenient function for wrapping a shell script
#'     into a data recipe (in `cwlProcess` S4 class). Please use
#'     `Rcwl::cwlProcess` for more options and functionalities,
#'     especially when the recipe gets complicated, e.g., needs a
#'     docker image for a command-line tool, or one parameter takes
#'     multiple types, etc. Refer to this recipe as an example:
#'     https://github.com/rworkflow/ReUseDataRecipe/blob/master/reference_genome.R
#' @export
#' @examples
#' \dontrun{
#' library(Rcwl)
#' ##############
#' ### example 1
#' ##############
#' 
#' script <- "
#' input=$1
#' outfile=$2
#' echo \"Print the input: $input\" > $outfile.txt
#' "
#' rcp <- recipeMake(shscript = script,
#'                   paramID = c("input", "outfile"),
#'                   paramType = c("string", "string"),
#'                   outputID = "echoout",
#'                   outputGlob = "*.txt")
#' inputs(rcp)
#' outputs(rcp)
#' rcp$input <- "Hello World!"
#' rcp$outfile <- "outfile"
#' res <- getData(rcp, outdir = tempdir(),
#'                notes = c("echo", "hello", "world", "txt"),
#'                showLog = TRUE)
#' readLines(res$out)
#'
#' ##############
#' ### example 2
#' ##############
#' 
#' shfile <- system.file("extdata", "gencode_transcripts.sh", package = "ReUseData")
#' readLines(shfile)
#' rcp <- recipeMake(shscript = shfile,
#'                   paramID = c("species", "version"),
#'                   paramType = c("string", "string"),
#'                   outputID = "transcripts", 
#'                   outputGlob = "*.transcripts.fa*",
#'                   requireTools = c("wget", "gzip", "samtools")
#'                   )
#' Rcwl::inputs(rcp)
#' rcp$species <- "human"
#' rcp$version <- "42"
#' res <- getData(rcp,
#'         outdir = tempdir(), 
#'         notes = c("gencode", "transcripts", "human", "42"),
#'         showLog = TRUE)
#' res$output
#' dir(tempdir())
#' }
recipeMake <- function(shscript = character(),
                       paramID = c(),  
                       paramType = c(),
                       outputID = c(),
                       outputType = c("File[]"), ## default is "File[]" file array
                       outputGlob = character(0),
                       requireTools = character(0)) {
    if(file.exists(shscript)){
        shscript <- paste(readLines(shscript), collapse = "\n")
    }
    shscript <- gsub('\\$\\{(.*?)\\}', '"$\\1"', shscript)
    ## This remove {} from evaluting as javascript. So
    ## abcd${test}_abcd${test} => abcd"$test"_abcd"$test"
    req1 <- requireShellScript(script = shscript)
    req2 <- requireNetwork()
    req3 <- requireJS()
    reqs <- list(req1, req2, req3)
    if (length(requireTools) > 0) {
        hint1 <- requireSoftware(packages = lapply(requireTools, condaPackage))
    }else{
        hint1 <- list()
    }
    
    input_param_list <- list()
    for (i in seq_along(paramID)) {
        id <- paramID[i]
        tp <- paramType[i]
        input_param <- InputParam(id=id, type=tp, position=i)  ##? InputArrayParam()
        input_param_list[[i]] <- input_param
    }
    output_param_list <- list()
    for (i in seq_along(outputID)) {
        id <- outputID[i]
        tp <- outputType[i]
        output_param <- OutputParam(id=id, type=tp, glob=outputGlob)
        output_param_list[[i]] <- output_param
    }

    rcpinputs <- input_param_list ## if empty, rcpinputs is list()
    if (length(input_param_list) > 0)
        rcpinputs <- do.call(InputParamList, input_param_list)

    rcpoutputs <- output_param_list ## if empty, rcpoutputs is list()
    if (length(output_param_list) > 0)
        rcpoutputs <- do.call(OutputParamList, output_param_list)

    rcp <- cwlProcess(cwlVersion = "v1.2",
                      baseCommand = ShellScript(),
                      requirements = reqs,
                      inputs = rcpinputs,
                      outputs = rcpoutputs,
                      )
    if(length(hint1) > 0){
        hints(rcp) <- list(hint1)
    }
    return(rcp)
}









