## echo_out

script <- '
input=$1
outfile=$2
echo "Print the input: $input" > $outfile.txt
'
echo_out <- recipeMake(shscript = script,
                       paramID = c("input", "outfile"),
                       paramType = c("string", "string"),
                       outputID = "echoout",
                       outputGlob = "*.txt")

echo_out <- addMeta(
    cwl = echo_out,
    label = "echo_out",
    doc = "Print some character strings to a txt file",
    inputLabels = c("input", "outfile"),
    inputDocs = c("The character strings to print to file",
                  "The output file name"),
    outputLabels = c("echoout"),
    outputDocs = c("The text file with printed strings"),
    extensions = list(
        author = "rworkflow team",
        date = Sys.Date(),
        example = paste(
            "## Get data from evaluting recipe",
            "echo_out <- recipeLoad('echo_out')",
            "echo_out$input <- 'Hello World'",
            "echo_out$outfile <- 'echoHelloWorld'",
            "getData(echo_out, outdir = 'data/folder', notes = c('echo', 'hello', 'world'))",
            "",
            "## Get data from local catch",
            "dataUpdate('data/folder')", 
            "dataSearch(c('echo', 'hello'))",
            "", 
            sep="\n"))
)

