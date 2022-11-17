species=$1
version=$2
wget http://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_$species/release_$version/gencode.v$version.annotation.gtf.gz
gzip -d gencode.v$version.annotation.gtf.gz
