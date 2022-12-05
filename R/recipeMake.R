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
#'
#' ##############
#' ### example 1
#' ##############
#' library(Rcwl)
#' 
#' shfile <- system.file("extdata", "demo_script.sh", package = "ReUseData")
#' readLines(shfile)
#' rcp <- recipeMake(shscript = shfile,
#'                   paramID = c("species", "version"),
#'                   paramType = c("string", "string"),
#'                   outputID = "annotation", 
#'                   outputGlob = "gencode.v*.annotation.gtf"
#'                   )
#' Rcwl::inputs(rcp)
#' rcp$species <- "human"
#' rcp$version <- "42"
#' \dontrun{
#' res <- getData(rcp,
#'         outdir = tempdir(), 
#'         notes = c("gencode", "human", "42"),
#'         showLog = TRUE)
#' res$output
#' dir(tempdir())
#' }
#' 
#' ##############
#' ### example 2
#' ##############
#' 
#' script <- '
#' aa=$1
#' bb=$2
#' echo "Print the input: $aa" > $bb.txt
#' '
#' rcp <- recipeMake(shscript = script,
#'                   paramID = c("aa", "bb"),
#'                   paramType = c("string", "string"),
#'                   outputID = "echoout",
#'                   outputGlob = "*.txt")
#' inputs(rcp)
#' outputs(rcp)
#' rcp$aa <- "Hello World!"
#' rcp$bb <- "outfile"
#' res <- getData(rcp, outdir = tempdir(),
#'                notes = c("echo", "txt", "test"),
#'                showLog = TRUE)
#' readLines(res$out)
#' 
recipeMake <- function(shscript = character(),
                       paramID = c(),  
                       paramType = c(),
                       outputID = c(),
                       ## outputType = c(), ## default is "File[]" file array
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
        ## dockerfile <- CondaTool(tools = requireTools)
        ## req4 <- requireDocker(File = dockerfile, ImageId = paste0(gsub("=.*", "", requireTools), collapse="_"))
        ## reqs[[4]] <- req4
        reqs[[4]] <- requireSoftware(packages = lapply(requireTools, condaPackage))
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
        ## tp <- outputType[i]
        output_param <- OutputParam(id=id, type="File[]", glob=outputGlob)
        output_param_list[[i]] <- output_param
    }

    rcpinputs <- input_param_list ## if empty, rcpinputs is list()
    if (length(input_param_list))
        rcpinputs <- do.call(InputParamList, input_param_list)

    rcpoutputs <- output_param_list ## if empty, rcpoutputs is list()
    if (length(output_param_list))
        rcpoutputs <- do.call(OutputParamList, output_param_list)

    cwlProcess(cwlVersion = "v1.2",
               baseCommand = ShellScript(),
               requirements = reqs,
               inputs = rcpinputs,
               outputs = rcpoutputs,
               )
}
