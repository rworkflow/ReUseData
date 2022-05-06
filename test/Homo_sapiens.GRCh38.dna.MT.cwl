cwlVersion: v1.2
class: CommandLineTool
baseCommand:
- sh
- script.sh
requirements:
- class: InitialWorkDirRequirement
  listing:
  - entryname: script.sh
    entry: |2

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
    writable: false
- class: NetworkAccess
  networkAccess: true
- class: InlineJavascriptRequirement
inputs:
  fasta:
    type:
    - string
    - File
    inputBinding:
      separate: true
outputs:
  fa:
    type: File
    secondaryFiles:
    - .fai
    - ^.dict
    - .amb
    - .ann
    - .bwt
    - .pac
    - .sa
    outputBinding:
      glob: $(inputs.fasta.split('/').slice(-1)[0].replace('.gz', ''))
