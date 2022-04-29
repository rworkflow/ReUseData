
fasta=$1
if [ ! -f $fasta ]
then
  wget $fasta
else
  cp $fasta .
fi

fa=`basename $fasta`

if [[ $fa =~ \.gz$ ]]
then
  bgzip -d $fa
  fa=`basename $fa .gz`
fi

fn=`basename $fa .fa`
fn=`basename $fn .fasta`
samtools faidx $fa
picard CreateSequenceDictionary R=$fa O=$fn.dict
bwa index $fa

