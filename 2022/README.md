# Advent of code 2022

Solutions for https://adventofcode.com/2022 in OCaml

## Initializing challenge

```bash
./new.sh -d 01 -n calorie_counting
```

## Running exercise

```bash
dune exec day_1 -- --input-path "data/01_calorie_counting"
```

## Running tests

```bash
# compare program output with test.expected, no logs - no errors
dune runtest

# running test for particular day in watch mode
dune runtest bin/day_01 -w
```
