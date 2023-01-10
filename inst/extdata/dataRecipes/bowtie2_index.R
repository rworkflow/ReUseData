
script <- "
genome=$1
prefix=`basename $genome`
bowtie2-build $genome $prefix
"

bowtie2_index <- recipeMake(shscript = script,
                            paramID = c("genome"),
                            paramType = c("File"),
                            outputID = "btIdx",
                            outputGlob = "*.bt2",
                            requireTools = "bowtie2")

bowtie2_index <- addMeta(
    bowtie2_index,
    label = "bowtie2_index",
    doc = "Bowtie 2 is an ultrafast and memory-efficient tool for aligning sequencing reads to long reference sequences. `bowtie2-build` is used to build the index files.",
    inputLabels = "reference genome",
    inputDocs = "The reference genome file",
    outputLabels = c("bowtie2 index"),
    outputDocs = c("A list of bowtie index files"),
    extensions = list(
        author = "rworkflow team",
        date = Sys.Date(),
        url = "https://bowtie-bio.sourceforge.net/bowtie2/index.shtml",
        example = paste(
            "recipeLoad('bowtie2_index.R', return = TRUE)",
            "bowtie2_index$genome <- 'GRCh38.primary_assembly.genome.fa'",
            "getData(bowtie2_index, outdir = 'data/folder', notes = c('bowtie2_index', 'GRCh38.primary_assembly'))",
            sep="\n"))
)
