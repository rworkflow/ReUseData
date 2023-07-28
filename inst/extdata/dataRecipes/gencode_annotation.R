## gencode_annotation

script <- "
species=$1
version=$2
wget http://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_$species/release_$version/gencode.v$version.annotation.gtf.gz
gzip -d gencode.v$version.annotation.gtf.gz
"

gencode_annotation <- recipeMake(shscript = script,
                  paramID = c("species", "version"),
                  paramType = c("string", "string"),
                  outputID = "annotation", 
                  outputGlob = "gencode.v*.annotation.gtf"
                  )  

gencode_annotation <- addMeta(
    cwl = gencode_annotation,
    label = "gencode_annotation",
    doc = "Download and unzip annotation files from gencode",
    inputLabels = c("species", "version"),
    inputDocs = c("'human' or 'mouse'",
                  paste0("Character string. Case sensitive. ",
                         "must match available versions for each species under source URL link. ",
                         "e.g., 'M31' (species='mouse'), '42' (species='human') ")),
    outputLabels = c("annotation"),
    outputDocs = c("the unzipped annotation file: `gencode.v$version.annotation.gtf`"),
    extensions = list(
        author = "rworkflow team", 
        url = "http://ftp.ebi.ac.uk/pub/databases/gencode/",
        date = Sys.Date(),
        example = paste(
            "## Get data from evaluting recipe",
            "gencode_annotation <- recipeLoad('gencode_annotation')",
            "gencode_annotation$species <- 'human'",
            "gencode_annotation$version <- '42'",
            "getData(gencode_annotation, outdir = 'data/folder', notes = c('gencode', 'annotation', 'human', '42'))",
            "",
            "## Get data from local catch",
            "dataUpdate('data/folder')", 
            "dataSearch(c('gencode', 'annotation', 'human', '42'))",
            "", 
            "## Get data from Google bucket directly",
            "dataUpdate('data/folder', cloud=TRUE)",
            "dh <- dataSearch(c('gencode', 'annotation'))", 
            "getCloudData(dh[1], outdir = 'data/folder')",
            sep="\n"))
)
