script <- "
build=$1
dbname=$2
wget https://hgdownload.cse.ucsc.edu/goldenpath/$build/database/${dbname}.txt.gz -O ${dbname}_${build}.txt.gz
gzip -d ${dbname}_${build}.txt.gz
wget https://hgdownload.cse.ucsc.edu/goldenpath/$build/database/${dbname}.sql -O ${dbname}_${build}.sql
"

ucsc_database <- recipeMake(shscript = script,
                  paramID = c("build", "dbname"),
                  paramType = c("string", "string"),
                  outputID = "database", 
                  outputGlob = "$(inputs.dbname)_$(inputs.build)*"
                  )  

ucsc_database <- addMeta(
    cwl = ucsc_database,
    label = "ucsc_database",
    doc = "UCSC genome annotation database from UCSC golden path",
    inputLabels = c("build", "dbname"),
    inputDocs = c("genome build version, e.g. 'hg19', 'hg38', 'mm39'",
                  "genome annotation database name, e.g. 'refGene', 'knownGene'"),
    outputLabels = c("database"),
    outputDocs = c("the unzipped annotation txt file and sql script"),
    extensions = list(
        author = "rworkflow team", 
        url = "https://hgdownload.soe.ucsc.edu/downloads.html",
        date = Sys.Date(),
        example = paste(
            "## Get data from evaluting recipe",
            "ucsc_database <- recipeLoad('ucsc_database')",
            "ucsc_database$build <- 'hg38'",
            "ucsc_database$dbname <- 'refGene'",
            "getData(ucsc_database, outdir = 'data/folder', notes = c('ucsc', 'annotation', 'database', 'hg38', 'refGene'))",
            "",
            "## Get data from local catch",
            "dataUpdate('data/folder')", 
            "dataSearch(c('ucsc', 'annotation', 'database', 'hg38', 'refGene'))",
            "", 
            sep="\n"))
)
