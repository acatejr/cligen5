#!/usr/bin/env bash
# run_tucson.sh — example: generate a WEPP climate file for Tucson, AZ.
#
# Usage: examples/run_tucson.sh [YEARS] [OUTPUT]
#   YEARS   number of years to simulate (default 30)
#   OUTPUT  output file (default tucson.cli in the current directory)
#
# Uses the station parameter file tests/parfiles/AZ028820.PAR
# (TUCSON WBO AZ, 45 years of record). Builds bin/cligen first if needed.

set -euo pipefail

ROOT=$(cd "$(dirname "$(readlink -f "$0")")/.." && pwd)
CLIGEN="$ROOT/bin/cligen"
PARFILE="$ROOT/tests/parfiles/AZ028820.PAR"

YEARS=${1:-30}
OUTPUT=${2:-tucson.cli}

if [[ ! -f $PARFILE ]]; then
  echo "error: station file not found: $PARFILE" >&2
  exit 1
fi

if [[ ! -x $CLIGEN ]]; then
  echo "Building CLIGEN..."
  make -C "$ROOT" --no-print-directory
fi

# CLIGEN options take their value with no space (-y30, not -y 30):
#   -i  station parameter file
#   -o  output file
#   -t5 continuous WEPP climate file
#   -b1 first simulated year is year 1
#   -y  number of years
#   -F  overwrite the output file without asking
# CLIGEN's screen output is only shown if the run fails.
if ! log=$("$CLIGEN" -i"$PARFILE" -o"$OUTPUT" -t5 -b1 -y"$YEARS" -F 2>&1 </dev/null); then
  echo "$log" >&2
  echo "error: CLIGEN failed" >&2
  exit 1
fi

echo "Wrote $OUTPUT ($YEARS years, Tucson, AZ)"
