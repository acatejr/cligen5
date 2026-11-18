# How PAR files are generated: what the files and source suggest (2026-11-18)

CLIGEN doesn't make PAR files; it only reads them. The code that creates them isn't in this repo.
These notes record what the files and the source suggest, without guessing at the actual tool or
procedure.

## Clues in the files

**1. Most values are statistics computed from a station's daily records.**

- The header line gives the years of record (`YEARS= 45.` for Tucson).
- The monthly lines are standard summaries of daily data: mean, SD and skew of precipitation,
  P(W/W) and P(W/D), the mean and SD of daily max and min temperature, solar radiation and dew
  point.
- The station numbering, such as `AZ028820` (state code 02, station 8820), looks like NOAA
  cooperative weather station IDs. That would mean the daily records came from the national
  weather station network, but this hasn't been confirmed.

**2. Some values are borrowed from nearby stations.**

Most weather stations record only precipitation and temperature. Each file ends with an
`INTERPOLATED DATA` section naming up to three other stations and their weights for:

- **wind** (for Tucson: Tucson/Davis, Tucson/Int., Chandler)
- **solar radiation and `MX .5 P`** (Walnut Gulch, Phoenix, Yuma)
- **dew point**
- **time to peak**

So those values were combined from other stations, weighted, presumably by distance. A comment in
`src/station.f` says the station names are only reported in the output and aren't used in CLIGEN's
calculations: the weighting was done when the file was made.

**3. The values are in US units.** Precipitation is in inches and temperatures in °F. CLIGEN
converts them to mm and °C in its output.

**4. The database has been regenerated over time.** The two Tucson files on hand differ in years
of record and statistics. `tests/parfiles/AZ028820.PAR` has 45 years and is from the 1995
database; `tests/samples/az028820.par` has 40 years and is probably from the 2015 database, which
uses 1974–2013 for every station. (An earlier version of these notes had this backwards; see
`2026-11-18-par-file-generation-from-online-sources.md`.)

**5. There is at least one other source.** The source comments mention "files of concatenated FS
data", and the interactive prompts recommend "the new Forest Service data" over the older station
files. So PAR files have been produced by more than one group or method.

## Still unknown

*Update: several of these are answered in
`2026-11-18-par-file-generation-from-online-sources.md` (the `dat2par` program, the time-to-peak
calculation, the sources of the sub-daily values, and `TYPE=`, which is the TR-55 storm type).*


- the program or procedure used to compute the statistics (for example, the period used or how
  skew and the time-to-peak distribution were calculated),
- where the sub-daily values (`MX .5 P`, `TP5`, `TP6`, `Time Pk`) came from, beyond the station
  names listed,
- how the interpolation weights were calculated,
- what the `TYPE=` field means.

The USDA-ARS CLIGEN documentation page
(https://www.ars.usda.gov/midwest-area/west-lafayette-in/national-soil-erosion-research/docs/wepp/cligen/)
may cover how the 2015 database was built.
