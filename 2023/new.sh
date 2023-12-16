#!/bin/bash

guard() {
  if [ -z $2 ]; then
    echo "Error: $1"
    exit 1
  fi
}

pad() {
  printf "%02d" $1
}

build_url() {
  echo "https://adventofcode.com/2023/day/$(echo "$1" | sed "s/^0*//")"
}

to_snake_case() {
  tr "[:upper:]" "[:lower:]" | tr ' ' '_'
}

extract_problem_name() {
  grep -E "\--- Day [0-9]+: [a-zA-Z ]+.*\---" |
    sed "s#.* Day [0-9]*: ##" |
    sed "s#[^a-zA-Z ]* ---.*##"
}

while getopts ":d:" flag; do
  case "${flag}" in
  d) day=${OPTARG} ;;
  esac
done

guard "-d is a required argument" $day

day=$(pad "$day")
problem=$(curl $(build_url "$day") | extract_problem_name | to_snake_case)

guard "Could not extract problem name" $problem

echo "Creating day:$day problem:$problem"

template=./_template
src=./bin/day_$day

mkdir $src
cp $template/test.* $src
cp $template/template.ml $src/$problem.ml

cat $template/dune |
  sed "s/%PROBLEM_NAME%/$problem/" |
  sed "s/%DAY%/$day/" >$src/dune

touch "./data/${day}_${problem}"
