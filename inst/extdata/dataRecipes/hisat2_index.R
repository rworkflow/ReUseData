
script <- "
genome=$1
prefix=`basename $genome`
hisat2-build $genome $prefix
"

hisat2_index <- recipeMake(shscript = script,
                           paramID = c("genome"),
                           paramType = c("File"),
                           outputID = "htIdx",
                           outputGlob = "*.ht2*",
                           requireTools = "hisat2")

hisat2_index <- addMeta(
    hisat2_index,
    label = "hisat2_index",
    doc = "HISAT2 is a fast and sensitive alignment program for mapping next-generation sequencing reads (both DNA and RNA) to a population of human genomes as well as to a single reference genome. `hisat2-build` is used to build the index files.",
    inputLabels = "reference genome",
    inputDocs = "The reference genome file",
    outputLabels = c("hisat2 index"),
    outputDocs = c("A list of index files"),
    extensions = list(
        author = "rworkflow team",
        date = Sys.Date(),
        url = "http://daehwankimlab.github.io/hisat2/",
        example = paste(
            "recipeLoad('hisat2_index.R', return = TRUE)",
            "hisat2_index$genome <- 'GRCh38.primary_assembly.genome.fa'",
            "getData(hisat2_index, outdir = 'data/folder', notes = c('hisat2_index', 'GRCh38.primary_assembly'))",
            sep="\n"))
)
