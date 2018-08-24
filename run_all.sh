#!/bin/sh

for input in scripts/*; do
  read -p "Test input: $input, press space to start..." key

  SCRIPTAnalyser-exe $@ < "$input"

done
