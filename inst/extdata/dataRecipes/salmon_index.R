
script <- "
genome=$1
transcript=$2

grep '^>' $genome | cut -d ' ' -f 1 > decoys.txt
sed -i -e 's/>//g' decoys.txt
cat $transcript $genome > gentrome.fa
salmon index -t gentrome.fa -d decoys.txt -p 12 -i salmon_index --gencode
"

salmon_index <- recipeMake(shscript = script,
                           paramID = c("genome", "transcript"),
                           paramType = c("File", "File"),
                           outputID = "salmonIdx",
                           outputType = "Directory",
                           outputGlob = "salmon_index",
                           requireTools = "salmon==1.3.0")

salmon_index <- addMeta(
    salmon_index,
    label = "salmon_index",
    doc = "Salmon is a tool for wicked-fast transcript quantification from RNA-seq data. The recipe is to build a salmon index for your transcriptome. The script is prepared according to this instruction (https://combine-lab.github.io/alevin-tutorial/2019/selective-alignment/.)",
    inputLabels = c("reference genome", "transcripts"),
    inputDocs = c("The fasta file for reference genome",
                  "The fasta file for transcripts from gencode"),
    outputLabels = c("salmon index"),
    outputDocs = c("A directory containing salmon index files"),
    extensions = list(
        author = "rworkflow team",
        date = Sys.Date(),
        url = "https://salmon.readthedocs.io/en/latest/salmon.html",
        example = paste(
            "salmon_index <- recipeLoad('salmon_index.R')",
            "salmon_index$genome <- 'GRCh38.primary_assembly.genome.fa'",
            "salmon_index$transcript <- 'gencode.v42.transcripts.fa'",
            "getData(salmon_index, outdir = 'data/folder', notes = c('salmon_index', 'GRCh38.primary_assembly', 'gencode.v42'))",
            "",
            "## Get data from local catch",
            "dataUpdate('data/folder')", 
            "dataSearch(c('salmon_index', 'GRCh38.primary_assembly', 'gencode.v42'))",
            "", 
            sep="\n"))
)
