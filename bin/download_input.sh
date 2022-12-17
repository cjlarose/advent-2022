#!/bin/bash

inputs_dir=./inputs

if [[ $# -le 1 ]]
then
  echo 'Usage : download_input.sh problem_number [problem_number]...' >&2
  exit 1
fi

if [[ -z $ADVENT_SESSION_COOKIE ]]
then
  echo 'Missing environment variable ADVENT_SESSION_COOKIE' >&2
  exit 1
fi

mkdir -p "$inputs_dir"

for problem_number in "$@"; do
  url='https://adventofcode.com/2022/day/'$problem_number'/input'
  output_filename="$inputs_dir/$problem_number".txt
  curl --silent "$url" -H 'Cookie: '"$ADVENT_SESSION_COOKIE" > "$output_filename"
done
