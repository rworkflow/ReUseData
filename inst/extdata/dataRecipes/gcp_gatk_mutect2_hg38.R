script <- "
filename=$1
idx=$2

url=https://storage.googleapis.com/gatk-best-practices/somatic-hg38/$filename
wget $url
if [[ $idx ]]; then wget $url.$idx; fi
"
gcp_gatk_mutect2_hg38 <- recipeMake(shscript = script,
                                  paramID = c("filename", "idx"),
                                  paramType = c("string", "string?"),
                                  outputID = "gfile",
                                  outputGlob = "$(inputs.filename)*")

gcp_gatk_mutect2_hg38 <- addMeta(
    gcp_gatk_mutect2_hg38,
    label = "gcp_gatk_mutect2_hg38",
    doc = "GATK mutect2 bundle for hg38 from GCP (gs://gatk-best-practices/somatic-hg38)",
    inputLabels = c("file name", "index"),
    inputDocs = c("The file basename to download from the broad GCP bucket",
                  "The 'idx' or 'tbi' index file if existing"),
    outputLabels = c("gfile"),
    outputDocs = c("The downloaded annotation files"),
    extensions = list(
        author = "rworkflow team",
        date = Sys.Date(),
        url = "https://console.cloud.google.com/storage/browser/gatk-best-practices/somatic-hg38",
        example = paste(
            "recipeLoad('gcp_gatk_mutect2_hg38', return = TRUE)",
            "gcp_gatk_mutect2_hg38$filename <- 'small_exac_common_3.hg38.vcf.gz'",
            "gcp_gatk_mutect2_hg38$idx <- 'tbi'",
            "getData(gcp_gatk_mutect2_hg38, outdir = 'data/folder', notes = c('gcp', 'broad', 'mutect2', 'small_exac_common'))",
            sep="\n"))
)
