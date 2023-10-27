#!/bin/bash

function guard() {
  if [ -z $2 ]; then
    echo "$1 is a required argument"
    exit 1
  fi
}

while getopts ":d:n:" flag; do
  case "${flag}" in
  d) day=${OPTARG} ;;
  n) problem=${OPTARG} ;;
  esac
done

guard "-d" $day
guard "-n" $problem

echo "Creating day $day problem $problem"

template=./_template
src=./bin/day_$day

mkdir $src
cp $template/test.* $src
cp $template/template.ml $src/$problem.ml

cat $template/dune |
  sed "s/%PROBLEM_NAME%/$problem/" |
  sed "s/%DAY%/$day/" >$src/dune

touch "./data/${day}_${problem}"
