species=$1
version=$2
if [ $species == 'human' ] && [ $version -gt 22 ]
then
  trans='transcripts'
else 
  trans='pc_transcripts'
fi
wget https://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_$species/release_$version/gencode.v$version.$trans.fa.gz
gzip -d gencode.v$version.$trans.fa.gz
samtools faidx gencode.v$version.$trans.fa
