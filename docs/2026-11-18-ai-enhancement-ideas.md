# Ideas: how AI could enhance CLIGEN (2026-11-18)

Brainstorming notes, roughly ordered from most practical to most speculative.

One principle runs through all of them: CLIGEN's value is that it's simple, reproducible and
statistically checked. AI is most useful around the generator, in the parameters that go in and
the tools people use to run it, rather than as a replacement for it.

## 1. Better station parameters (the strongest case)

CLIGEN is only as good as its PAR files, and they come from a limited number of stations with
uneven record lengths. In Arizona alone they range from 24 to 100 years.

- **Parameters for places without a station.** Most fields aren't next to a weather station.
  Machine-learning models trained on station parameters plus elevation, terrain, distance to the
  coast and gridded climate data could estimate a PAR file for any latitude/longitude. Standard
  spatial interpolation already exists as a baseline, so an ML approach would need to clearly beat
  it to be worth using.
- **Updating parameters.** The 2015 database reflects past climate. Parameters could be re-derived
  from newer observations, or from downscaled climate-model projections to produce future-climate
  PAR files for erosion scenarios. That second use is where interest is growing.
- **Checking the database.** Scan all 2,642 files for values that are physically odd or don't fit
  their neighbors: an SD that's implausible for its mean, P(W/W) or P(W/D) values out of line with
  nearby stations, or temperature profiles that look like data-entry errors. This doesn't even
  need ML, and could be done with the files already on hand.

## 2. Sub-daily storm characteristics

This is the part of CLIGEN that matters most for erosion and that it does in the simplest way.
Storm duration, time to peak and peak intensity are derived from a few parameters (`MX .5 P`,
`TP5`, `TP6`, the `Time Pk` distribution). Radar-based high-resolution precipitation records now
exist, and models trained on them could:

- estimate intensity parameters where gauges are sparse,
- test how well CLIGEN's storm shapes match observed storms,
- propose better ways of generating intensity.

Any of this would need to be checked against breakpoint rainfall records, and it would change WEPP
results, so it would have to go through the model's maintainers.

## 3. Known weaknesses of the generator

CLIGEN uses a first-order Markov chain for wet and dry days. Generators of this kind are known to
underestimate long dry spells and multi-day wet periods, and each station is generated on its own,
so nearby points aren't correlated. ML could:

- **measure** these biases by comparing long CLIGEN runs to observed records across all stations,
  which is a useful study in itself,
- **adjust** the parameters to compensate, keeping CLIGEN's structure.

Generative AI weather models (deep-learning generators) can produce realistic multi-site weather.
But they lose what makes CLIGEN trustworthy: transparency, the QC tests in `ranset`, exact
reproducibility, and the ability to explain where a value came from. They're better treated as a
research comparison than a replacement.

## 4. Language-model tools for users and developers

- **A natural-language front end.** "Give me 50 years of climate for a field near Casa Grande"
  could become: find the nearest suitable station, build the right command line (no spaces, the
  correct `-t`), run it, and explain the output. That would avoid common mistakes such as running
  without `-i`, which leads to the "stations file" error.
- **Explaining output:** summarizing a `.cli` file, or comparing generated statistics with the PAR
  file's statistics in plain language.
- **Documentation:** the PAR file format, and the meaning of fields like `TYPE=` (not yet
  determined from the source), could be documented from the source, though it needs checking by
  someone who knows the model's history.
- **Code work:** refactoring, `IMPLICIT NONE`, explicit `SAVE` and similar steps, always gated by
  the regression check (`tests/regress.sh`).

## Picks

- **Quickest and most useful:** the database scan (1, "Checking the database") and a
  natural-language front end (4).
- **Most scientific value:** parameters for places without a station (1) and storm intensity (2).
  Both are research projects that need validation against observed data, and probably
  collaboration with the USDA-ARS group that maintains CLIGEN.

A good first step is the PAR file scan: it needs only the station files already on hand, and it
would show whether the 2015 database has problems worth reporting.
