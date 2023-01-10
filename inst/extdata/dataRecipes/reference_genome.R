## reference_genome

script <- "
fasta=$1
if [ ! -f $fasta ]
then
  wget $fasta
else
  cp $fasta .
fi

fa=`basename $fasta`

if [[ $fa =~ \\.gz$ ]]
then
  gzip -d $fa
  fa=`basename $fa .gz`
fi

fn=`basename $fa .fa`
fn=`basename $fn .fasta`
mv $fa $fn.fa
fa=$fn.fa

samtools faidx $fa
picard CreateSequenceDictionary R=$fa O=$fn.dict
bwa index $fa
"

p1 <- InputParam(id = "fasta", type = list("string", "File"))
o1 <- OutputParam(id = "fa", type = "File", glob = "*.fa",
                  secondaryFile = list(".fai",
                                       "^.dict",
                                       ".amb",
                                       ".ann",
                                       ".bwt",
                                       ".pac",
                                       ".sa"))

req1 <- requireShellScript(script)
requireTools <- c("bwa", "samtools", "picard")
req2 <- requireSoftware(packages = lapply(requireTools, condaPackage))

req3 <- requireNetwork()
req4 <- requireJS()
reference_genome <- cwlProcess(cwlVersion = "v1.2",
                  baseCommand = c("bash", "script.sh"),
                  requirements = list(req1, req3, req4),
                  hints = list(req2),
                  inputs = InputParamList(p1),
                  outputs = OutputParamList(o1))

reference_genome <- addMeta(
    reference_genome,
    label = "reference_genome",
    doc = "Download (if not previously exist as a local file), rename (as *.fa), and index the reference genome with samtools and bwa",
    inputLabels = c("reference genome"),
    inputDocs = c("Can be a file path (if locally available) or a url as indicated in 'Data source'"),
    outputLabels = c("indexed reference genome"),
    outputDocs = c("*.fa, *.fai files, and some secondary files"),
    extensions = list(
        author = "rworkflow team",
        url = c("http://ftp.1000genomes.ebi.ac.uk/vol1/ftp/technical/reference/",
                "http://ftp.ensembl.org/pub/release-104/fasta/homo_sapiens/dna/",
                "http://ftp.ensembl.org/pub/release-104/fasta/mus_musculus/dna/"),
        date = Sys.Date(),
        example = paste(
            "## Get data from evaluating recipe",
            "recipeLoad(reference_genome, return=TRUE)",
            "reference_genome$fasta = 'http://ftp.ensembl.org/pub/release-104/fasta/homo_sapiens/dna/Homo_sapiens.GRCh38.dna.chromosome.MT.fa.gz'",
            "getData(reference_genome, outdir = 'data/folder', notes = c('homo sapiens', 'grch38', 'ensembl'), conda = TRUE, docker = FALSE)",
            "",
            "## Get data from Google bucket directly",
            "dataUpdate('data/folder', cloud=TRUE)",
            "dh <- dataSearch(c('homo sapiens', 'grch38', '1000 genomes'))", 
            "getCloudData(dh, outdir = 'data/folder')",
            sep = "\n"))
)
