#!/usr/bin/env bash

function contains() {
  if [ $# -lt 2 ]; then
    echo "Wrong number of arguments to contains"
    exit 1
  fi
  if [[ $1 =~ (^|[[:space:]])"$2"($|[[:space:]]) ]]; then
    return 0
  else
    return 1
  fi
}

function process() {
  if [ $# -lt 2 ]; then
    echo "Wrong number of arguments to process"
    exit 1
  fi
  local FILE="$1"
  local DEMAND="$2"

  # Remove superfluous headers from the CSVs.
  local FIXED="$FILE-fixed.csv"
  sed '1!{/^Unix Time/d;}' "$FILE" > "$FIXED"

  # Hand off the rest to the R script.
  ./results.R "$FIXED" "$DEMAND"

  return $?
}

DEMANDONE="list_filter.csv list_map.csv list_reverse.csv rope_mergesort.csv quickhull.csv"
DEMANDALL="list_eager_filter.csv list_eager_map.csv list_reverse.csv rope_mergesort.csv quickhull.csv"
DEMANDNA="rope_min.csv rope_sum.csv rope_median.csv"

PATHS=$(find ../out ! -name '*fixed*' | grep csv)
for FPATH in $PATHS; do
  FILE=$(basename "$FPATH")
  if $(contains "$DEMANDONE" "$FILE"); then
    process "$FPATH" "one"
  fi
  if $(contains "$DEMANDALL" "$FILE"); then
    process "$FPATH" "all"
  fi
  if $(contains "$DEMANDNA"  "$FILE"); then
    process "$FPATH" "NA"
  fi
done
