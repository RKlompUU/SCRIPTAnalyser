#!/bin/sh

stack install || exit 1


for input in scripts/*; do
  scrpt=`basename "$input"`
  output=".regression-new/$scrpt"
  cmp=".regression-old/$scrpt"

  echo "test input: $input,\toutput: $output"

  SCRIPTAnalyser-exe 10 < "$input" > $output

  if [ `cmp --silent "$cmp" "$output"` ];
  then
    echo "files differ"
    kdiff3 "$cmp" "$output" &
  else
    mv -v "$output" "$cmp"
  fi
done
