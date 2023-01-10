script <- "
filename=$1
idx=$2

url=https://storage.googleapis.com/gcp-public-data--broad-references/hg38/v0/$filename
wget $url
if [[ $idx ]]; then wget $url.$idx; fi
"
gcp_broad_gatk_hg38 <- recipeMake(shscript = script,
                                  paramID = c("filename", "idx"),
                                  paramType = c("string", "string?"),
                                  outputID = "gfile",
                                  outputGlob = "$(inputs.filename)*")

gcp_broad_gatk_hg38 <- addMeta(
    gcp_broad_gatk_hg38,
    label = "gcp_broad_gatk_hg38",
    doc = "GATK annotation bundle for hg38 from GCP (gs://gcp-public-data--broad-references/hg38/v0/)",
    inputLabels = c("file name", "index"),
    inputDocs = c("The file basename to download from the broad GCP bucket",
                  "The 'idx' or 'tbi' index file if existing"),
    outputLabels = c("gfile"),
    outputDocs = c("The downloaded annotation files"),
    extensions = list(
        author = "rworkflow team",
        date = Sys.Date(),
        url = "https://console.cloud.google.com/storage/browser/gcp-public-data--broad-references/hg38/v0",
        example = paste(
            "recipeLoad('gcp_broad_gatk_hg38', return = TRUE)",
            "gcp_broad_gatk_hg38$filename <- '1000G_omni2.5.hg38.vcf.gz'",
            "getData(gcp_broad_gatk_hg38, outdir = 'data/folder', notes = c('gcp', 'broad', 'reference', 'hg38', 'v0', '1000G', 'omni2.5')",
            sep="\n"))
)
