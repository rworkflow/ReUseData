## ensembl_liftover

script <- "
species=$1
from=$2
to=$3
if [ $species == 'human' ]
then 
  species='homo_sapiens'
elif [ $species == mouse ]
then 
  species='mus_musculus'
fi
wget https://ftp.ensembl.org/pub/assembly_mapping/$species/${from}_to_${to}.chain.gz
gzip -d ${from}_to_${to}.chain.gz"

ensembl_liftover <- recipeMake(shscript = script,
                  paramID = c("species", "from", "to"), 
                  paramType = c("string", "string", "string"), 
                  outputID = "liftover", 
                  outputGlob = "*.chain",
                  requireTools = c("wget", "gzip")) 

ensembl_liftover <- addMeta(
    cwl = ensembl_liftover,
    label = "ensembl_liftover",
    doc = "Download and unzip genome liftover file from Ensembl",
    inputLabels = c("species", "from", "to"),
    inputDocs = c(paste0("'human' or 'mouse', which maps to the ",
                         "'homo_sapiens' or 'mus_musculus' folder under source URL"),
                  "original genome build (case sensitive, must match source URL)",
                  "target genome build (case sensitive, must match source URL)"),
    outputLabels = c("liftover"),
    outputDocs = c("the unzipped liftover file: `${from}_to_${to}.chain`"),
    extensions = list(
        author = "rworkflow team",
        url = "https://ftp.ensembl.org/pub/assembly_mapping/",
        date = Sys.Date(),
        example = paste(
            "## Get data from evaluting recipe",
            "recipeLoad('ensembl_liftover', return = TRUE)",
            "ensembl_liftover$species <- 'human'",
            "ensembl_liftover$from <- 'GRCh37'",
            "ensembl_liftover$to <- 'GRCh38'",
            "getData(ensembl_liftover, outdir = 'data/folder', notes = c('grch37', 'grch38'))",
            "",
            "## Get data from Google bucket directly",
            "dataUpdate('data/folder', cloud=TRUE)",
            "dh <- dataSearch(c('ensembl', 'GRCh37'))", 
            "getCloudData(dh[1], outdir = 'data/folder')",
            sep="\n"))
)
