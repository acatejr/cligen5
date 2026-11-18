# Regression test setup and Python cleanup (2026-11-18)

Branch: `split-source-files` (tracking issue: https://github.com/acatejr/cligen5/issues/1)

## Python references removed

The Python port is not part of this project, so references to it were removed:

- **`CLAUDE.md`:** removed the archived-port section, its entry in the layout list, and the
  `make reference` line.
- **`Makefile`:** removed the `reference` target, which existed only for the Python port.
- **Kept:** the fact behind that target. At `-O2` gcc vectorizes `fouri1` and calls glibc's SIMD
  `sinf`/`cosf` (libmvec), which can differ from scalar calls in the last bits. This is now under
  "Key behaviors to preserve" in `CLAUDE.md`.

No Python references remain outside `tests/`.

## Makefile committed

The Makefile is committed to `split-source-files` as `a898f75` ("Add Makefile for building with
gfortran"). Targets: `build` (default, `bin/cligen`), `debug`, `run`, `run-debug`, `version`,
`usage`, `clean`, `help`.

## `tests/regress.sh`

Checks that the source in the working tree produces byte-for-byte the same output as the
author's code.

- **How it works:** builds the author's code from `upstream/main` (via `git archive`) and the
  working tree with the same compiler and flags (default `gfortran -O2 -std=legacy
  -fno-automatic`). Runs both on the same station files and compares every output file, the
  screen output, and the exit code.
- **What it runs:** 9 cases per station:

  | Case       | Arguments                     | Notes                          |
  |------------|-------------------------------|--------------------------------|
  | `t5_I0`    | `-t5 -b1 -y100 -I0`           | continuous WEPP, no interpolation |
  | `t5_I1`    | `-t5 -b1 -y100 -I1`           | linear interpolation           |
  | `t5_I2`    | `-t5 -b1 -y100 -I2`           | Fourier interpolation          |
  | `t5_I3`    | `-t5 -b1 -y100 -I3`           | mean-preserving interpolation  |
  | `t5_r100`  | `-t5 -b1 -y30 -r100`          | discard 100 random numbers per stream |
  | `t3`       | `-t3 -b1 -y30`                | CREAMS/GLEAMS file             |
  | `t2`       | `-t2 -b1 -y30`                | multi-year screen output       |
  | `t4`       | `-t4`                         | single storm; stdin `7 15 1`, `2.5`, `6`, `0.3`, `3.0` |
  | `t7`       | `-t7`                         | TR-55 design storm; stdin `7 15 1`, `6.3` |

- **What it keeps:** outputs that match are deleted right away (a full run would otherwise write
  tens of GB). Outputs that differ go to `tests/out/diffs/`. The list of differences is in
  `tests/out/results.txt`.
- **Exit status:** 0 all identical, 1 differences found, 2 setup or build error.
- **Options:**
  - `-n N` every Nth station (default 25, about 106 stations)
  - `-a` all 2,642 stations (estimated about an hour on 2 cores; not yet run)
  - `-j JOBS` parallel jobs (default: number of CPUs)
  - `-r REV` reference revision (default `upstream/main`)
  - `-k` keep all outputs
- **Build:** compiles whatever `.f` and `.inc` files it finds, so it needs no changes once the
  code moves into `src/` and `include/`.
- **Header note:** CLIGEN writes its command line into each output header, so both builds are
  run with the same arguments (same station file path, output file named `out`).

## Checks

- **Unchanged branch:** identical to `upstream/main` on 6 stations (`-n 500`), and again on 3
  stations (`-n 1000`) after a small fix.
- **Catching a change:** with the version number temporarily changed in `cligen.f`
  (`5.3220` → `5.3221`), the script reported 48 differing files across 3 stations. `cligen.f`
  was then restored.

## Local-only files

`tests/regress.sh`, `tests/parfiles/`, `tests/samples/`, `tests/out/`, `bin/`, `CLAUDE.md`, and
`claude-resume.sh` are listed in `.git/info/exclude`, so they stay out of commits and pull
requests.

## Next steps

- Reword issue #1's task "save reference outputs" (the script rebuilds the reference each run
  instead) and tick it off.
- Run the default 106-station check as the baseline before starting the split.
