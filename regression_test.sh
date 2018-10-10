#!/bin/sh

stack install || exit 1


for input in scripts/*; do
  scrpt=`basename "$input"`
  output=".regression-new/$scrpt"
  cmp=".regression-old/$scrpt"

  echo "test input: $input,\toutput: $output"

  SCRIPTAnalyser-exe < "$input" > $output

  if cmp --silent "$cmp" "$output"
  then
    mv -v "$output" "$cmp"
  else
    echo "files differ"
    kdiff3 "$cmp" "$output" &
  fi
done
