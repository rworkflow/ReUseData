script <- "
  wget https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_42/GRCh38.primary_assembly.genome.fa.gz
"
gencode_genome_grch38 <- recipeMake(script, 
                                    outputID = "genome",
                                    outputGlob = "GRCh38.primary_assembly.genome.fa.gz")

gencode_genome_grch38 <- addMeta(
    cwl = gencode_genome_grch38,
    label = "gencode_genome_grch38",
    doc = "Download human genome GRCh38 from GENCODE release 42",
    outputLabels = c("genome"),
    outputDocs = c("The `GRCh38.primary_assembly.genome.fa.gz` fasta file"),
    extensions = list(
        author = "rworkflow team",
        url = "https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_42/GRCh38.primary_assembly.genome.fa.gz",
        date = Sys.Date(),
        example = paste(
            "Get data from evaluting recipe",
            "recipeLoad('gencode_genome_grch38', return = TRUE)",
            "getData(gencode_genome_grch38, outdir = 'data/folder', notes = c('gencode', 'genome', 'grch38', 'release 42'))",
            "",
            "## Get data from Google bucket directly",
            "dataUpdate('data/folder', cloud = TRUE)",
            "dh <- dataSearch(c('gencode', 'genome', 'grch38'))",
            "getCloudData(dh[dataNames(dh) == 'GRCh38.primary_assembly.genome.fa.gz'], outdir = 'data/folder')", 
            sep = "\n"))
    )
