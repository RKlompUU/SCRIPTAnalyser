#!/bin/sh

for input in scripts/*; do
  scrpt=`basename "$input"`
  output=".regression-new/$scrpt"
  cmp=".regression-old/$scrpt"

  mv -v "$output" "$cmp"
done
