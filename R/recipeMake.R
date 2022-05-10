#' recipeMake
#'
#' Function to make a data recipe
#' @param shscript character string with file path to the user
#'     provided shell script to be converted into a data recipe.
#' @param
#' @importFrom Rcwl InputParam OutputParam InputParamList OutputParamList
#' @examples
#' ref_genome <- recipeMake(shscript = "", paramID = c("fasta"), paramType=c("string"), )
#'
#'
recipeMake <- function(shscript = "",
                       paramID = c(),
                       paramType = c(),
                       outputID = c(),
                       outputType = c(),
                       outputGlob = "",
                       requireTools = "") {
    req1 <- requireShellScript()
    req2 <- requireNetwork()
    req3 <- requireJS()
    dockerfile <- CondaTool(tools = requireTools)
    req4 <- requireDocker(File = dockerfile, imageID = "?")
    
    input_param_list <- list()
    for (i in seq_along(paramID)) {
        id <- paramID[i]
        tp <- paramType[i]
        input_param <- InputParam(id=id, type=tp, position=i)
        input_param_list[[i]] <- input_param
    }
    output_param_list <- list()
    for (i in seq_along(outputID)) {
        id <- outputID[i]
        tp <- outputType[i]
        output_param <- OutputParam(id=id, type=tp, glob=outputGlob)
        output_param_list[[i]] <- output_param
    }
   
    cwlProcess(cwlVersion = "v1.2",
               baseCommand = shellScript(),
               requirements = list(),
               inputs = InputParamList(),
               outputs = OutputParamList())
}
