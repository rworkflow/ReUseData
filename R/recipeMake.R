#' recipeMake
#'
#' Function to make a data recipe
#' @param shscript character string. Can take either the file path to
#'     the user provided shell script, or directly the script content,
#'     that are to be converted into a data recipe.
#' @param paramID the ID for each parameter to pass in.
#' @param paramType Character string specifying the type for each
#'     `paramID`. Valid values are "int" for integer, "boolean" for
#'     boolean, "float" for numeric, "File" for file path, "File[]"
#'     for an array of files, etc. Can also take "double", "long",
#'     "null", "Directory". Find more details:
#'     "https://www.commonwl.org/v1.2/CommandLineTool.html#CWLType".
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
#' @importFrom Rcwl InputParam OutputParam InputParamList
#'     OutputParamList
#' @details This function is a convenient function for wrapping a
#'     shell script into a data recipe (in `cwlProcess` S4
#'     class). Please use `Rcwl::cwlProcess` for more options and
#'     functionalities.
#' @examples
#' ref_genome <- recipeMake(shscript = "", paramID = c("fasta"), paramType=c("string"), )
#'
#'
recipeMake <- function(shscript = "",  ## FIXME: support a valid URL for downloading
                       paramID = c(),  
                       paramType = c(),
                       outputID = c(),
                       ## outputType = c(),
                       outputGlob = character(0),
                       requireTools = character(0)) {
    ## browser()
    if(file.exists(shscript)){
        shscript <- paste(readLines(shscript), collapse = "\n")
    }
    req1 <- requireShellScript(script = shscript)
    req2 <- requireNetwork()
    req3 <- requireJS()
    reqs <- list(req1, req2, req3)
    if (length(requireTools) > 0) {
        dockerfile <- CondaTool(tools = requireTools)
        req4 <- requireDocker(File = dockerfile, ImageId = paste0(gsub("=.*", "", requireTools), collapse="_"))
        reqs[[4]] <- req4
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
   
    cwlProcess(cwlVersion = "v1.2",
               baseCommand = ShellScript(),
               requirements = reqs,
               inputs = do.call(InputParamList, input_param_list),
               outputs = do.call(OutputParamList, output_param_list)
               )
}
