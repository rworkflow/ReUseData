## gencode_transcripts

script <- "
species=$1
version=$2
if [ $species == 'human' ] && [ $version -gt 22 ]
then
  trans='transcripts'
else 
  trans='pc_transcripts'
fi
wget https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_$species/release_$version/gencode.v$version.$trans.fa.gz
gzip -d gencode.v$version.$trans.fa.gz
samtools faidx gencode.v$version.$trans.fa
"

gencode_transcripts <- recipeMake(script, 
                  paramID = c("species", "version"),
                  paramType = c("string", "string"),
                  outputID = "transcripts",
                  outputGlob = "*transcripts.fa*")

gencode_transcripts <- addMeta(
    cwl = gencode_transcripts,
    label = "gencode_transcripts",
    doc = "Download, unzip, and index transcripts files from gencode",
    inputLabels = c("species", "version"),
    inputDocs = c("'human' or 'mouse'",
                  paste0("Character string. Case sensitive. ",
                         "must match available versions for each species under source URL link. ",
                         "e.g., 'M31' (species='mouse'), '42' (species='human') ")),
    outputLabels = c("transcripts"),
    outputDocs = c("the samtool indexed annotation files: `*transcripts.fa`, `*transcripts.fa.fai`"),
    extensions = list(
        author = "rworkflow team", 
        url = "http://ftp.ebi.ac.uk/pub/databases/gencode/",
        date = Sys.Date(),
        example = paste(
            "## Get data from evaluating recipe",
            "recipeLoad('gencode_transcripts', return = TRUE)",
            "gencode_transcripts$species <- 'mouse'",
            "gencode_transcripts$version <- 'M31'",
            "getData(gencode_transcripts, outdir = 'data/folder', notes = c('gencode', 'transcripts', 'mouse', 'M31'))",
            "",
            "## Get data from Google bucket directly",
            "dataUpdate('data/folder', cloud=TRUE)",
            "dh <- dataSearch(c('gencode', 'transcripts'))", 
            "getCloudData(dh[1], outdir = 'data/folder')",
            sep="\n"))
)
