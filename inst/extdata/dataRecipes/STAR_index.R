script <- "
ref=$1
gtf=$2
genomeDir=$3
threads=$4
sjdb=$5

if [ ! -f $ref ]
then
  wget $ref
  ref=`basename $ref`
fi 

if [ ! -f $gtf ]
then
  wget $gtf
  gtf=`basename $gtf`
fi

STAR --runMode genomeGenerate --runThreadN $threads --genomeDir $genomeDir --genomeFastaFiles $ref --sjdbGTFfile $gtf  --sjdbOverhang $sjdb
"

p1 <- InputParam(id = "ref", type = list("string", "File"), position = 1)
p2 <- InputParam(id = "gtf", type = list("string", "File"), position = 2)
p3 <- InputParam(id = "genomeDir", type = "string", position = 3)
p4 <- InputParam(id = "threads", type = "int", position = 4)
p5 <- InputParam(id = "sjdb", type = "int", position = 5, default = 100)
o1 <- OutputParam(id = "idx", type = "Directory", glob = "$(inputs.genomeDir)")
req1 <- requireShellScript(script)
req2 <- requireDocker("quay.io/biocontainers/star:2.7.9a--h9ee0642_0")
req3 <- requireNetwork()
STAR_index <- cwlProcess(cwlVersion = "v1.2",
                         requirements = list(req1, req2, req3),
                         baseCommand = ShellScript(),
                         inputs = InputParamList(p1, p2, p3, p4, p5),
                         outputs = OutputParamList(o1))

STAR_index <- addMeta(
    STAR_index,
    label = "STAR_index",
    doc = "ultrafast universal RNA-seq and scRNAseq aligner",
    inputLabels = c("reference genome", "GTF", "genomeDir", "threads", "sjdbOverhang"),
    inputDocs = c("The reference genome file", "gene annotation file", "The directory for index files.", "The number of threads to use", "The length of the genomic sequence around the annotated junction"),
    outputLabels = c("STAR index"),
    outputDocs = c("The directory for STAR index files"),
    extensions = list(
        author = "rworkflow team",
        date = Sys.Date(),
        url = "https://github.com/alexdobin/STAR",
        example = paste(
            "recipeLoad('STAR_index.R', return = TRUE)",
            "STAR_index$ref <- 'gcpData/reference_genome/GRCh38.primary_assembly.genome.fa'",
            "STAR_index$gtf <- 'gcpData/gencode_annotation/gencode.v42.annotation.gtf'",
            "STAR_index$sjdb <- 100",
            "STAR_index$genomeDir <- 'GRCh38.GENCODE.v42_100'",
            "STAR_index$threads <- 16",
            "getData(STAR_index, outdir = 'gcpData', notes =c('STAR_index', 'GRCh38.primary_assembly', 'gencode.v42', 'star_2.7.9a'))",
            sep="\n"))
)
