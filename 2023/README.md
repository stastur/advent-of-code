# Advent of code 2023

Solutions for https://adventofcode.com/2023 in OCaml

## Initializing challenge

```bash
./new.sh -d 01
```

## Running exercise

```bash
dune exec day_1 -- --input-path "data/01_trebuchet"
```

## Running tests

```bash
# compare program output with test.expected, no logs - no errors
dune runtest

# running test for particular day in watch mode
dune runtest bin/day_01 -w
```
