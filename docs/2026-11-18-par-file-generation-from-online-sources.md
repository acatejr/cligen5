# How CLIGEN PAR files are generated: summary of online sources (2026-11-18)

## Sources read

1. ARS CLIGEN page:
   https://www.ars.usda.gov/midwest-area/west-lafayette-in/national-soil-erosion-research/docs/wepp/cligen/
2. "Format of CLIGEN weather station statistics input files" (D.C. Flanagan, 2001; updated by
   J. Frankenberger, 2008): https://www.ars.usda.gov/ARSUserFiles/50201000/WEPP/cligen/cligenparms.pdf
3. "General Description of the CLIGEN Model and its History" (notes by C.R. Meyer):
   https://www.ars.usda.gov/ARSUserFiles/50201000/WEPP/cligen/CLIGENDescription.pdf
4. Srivastava, A., D.C. Flanagan, J.R. Frankenberger, and B.A. Engel. 2019. "Updated climate
   database and impacts on WEPP model predictions." *Journal of Soil and Water Conservation*
   74(4):334–349. doi:10.2489/jswc.74.4.334.
   https://www.ars.usda.gov/ARSUserFiles/50201000/WEPP/JSWC-74-4-334-349.pdf

## How PAR files are made

PAR files are produced by a program called **`dat2par`**, written by D. Scheele and D. Hall at the
USDA Forest Service lab in Moscow, Idaho (Scheele and Hall 2000). It grew out of code recovered from
Arlin Nicks' computer after his death in 1997. It combines several input files (source 4):

| Input file | What it supplies | Where the data comes from |
|---|---|---|
| `.DAT` (one per station) | daily precipitation, Tmax, Tmin | NOAA National Climatic Data Center (NCDC) daily records |
| `STATIONS` | name, lat/long, elevation, `TYPE` | — |
| `STATPARM` | solar radiation and SD, `MX .5 P`, `TP5`, `TP6` | hourly National Solar Radiation Database; NCDC 15-minute precipitation; NOAA rainfall-frequency maps |
| `TIMEPEAK` | `Time Pk` distribution | NCDC 15-minute precipitation |
| `DEWPOINT` | dew point | NCDC hourly observations |
| `IDXALL`, `WIND` | 16-direction wind statistics | not stated |

### How each value is calculated

- **Precipitation:** mean precipitation per wet day (the monthly total divided by the number of wet
  days), its SD and skew, and P(W/W) and P(W/D). A wet day is any day with non-zero precipitation.
  Meyer notes the skew values closely match Excel's sample skewness formula (source 3).
- **Temperature:** monthly mean and SD of daily Tmax and Tmin.
- **Solar radiation:** monthly mean and SD, computed from hourly National Solar Radiation Database
  values (Wh/m², converted to Langleys). The 2015 database used 1991–2005 data.
- **`MX .5 P`:** the monthly mean of each day's maximum 30-minute rainfall intensity (in/hr).
- **`Time Pk`:** 12 cumulative fractions of storm time-to-peak, in bins of 1/12. To compute
  time-to-peak, gaps within a storm are removed. It's the time from the start of rain to the middle
  of the peak 15-minute interval, divided by the total storm duration (sources 2 and 4).
- **`TP5` / `TP6`:** the maximum 30-minute and 6-hour rainfall depths (inches). In 1995 they came
  from Hershfield's 1961 rainfall-frequency atlas (Technical Paper 40); in 2015 from updated NOAA
  precipitation-frequency maps. CLIGEN reads `TP6` but never `TP5` (confirmed in the code).
- **Dew point:** monthly mean daily dew point from hourly observations (°F).
- **Wind:** for 16 directions, the percentage of time, mean speed, SD and skew (m/s), plus percent
  calm. The 2015 database reused the 1995 wind values unchanged.
- **`TYPE=`:** the TR-55 24-hour design-storm distribution type, 1–4 (Soil Conservation Service
  Technical Release 55). CLIGEN reads it as `itype` for single-storm runs. The local files contain
  187 of type 1, 98 of type 2, 2,020 of type 3 and 337 of type 4.
- **Interpolation:** when a station lacks solar radiation, `MX .5 P`, time-to-peak, dew point or
  wind data, `dat2par` fills them in by "triangulation" from three nearby stations, weighted by
  proximity. The weighting formula isn't given. The station names and weights at the end of each
  PAR file are for information only and aren't used by CLIGEN.

## The two US databases

| | 1995 database | 2015 database |
|---|---|---|
| Stations | 2,642 | 2,765 |
| Daily precipitation/temperature record | variable, through 1992 (9–117 years) | 1974–2013 for every station (40 years) |
| Stations with their own solar radiation and `MX .5 P` | 142 | 822 |
| Stations with their own time-to-peak | 1,548 | 912 (the paper's text says 921) |
| Stations with their own dew point | 273 | 568 |
| Wind stations | 852 | the same 852, unchanged |

- The 2015 update was requested by USDA-NRCS, which plans to use WEPP in its field offices.
- **Error in the 1995 database:** minimum temperatures below −17.7 °C (0 °F) lost their negative
  sign in the `.DAT` files, affecting most stations. The paper's authors corrected 2,235 stations for
  their comparison. The sources don't say whether the downloadable 1995 PAR files were ever fixed.
- An older set (`v42data.zip`, one file per state) works only with CLIGEN 4.2.

## The local station files are the 1995 database

The files in `tests/parfiles/` are almost certainly the **1995** database, not the 2015 one:

- **Count and record lengths:** 2,642 files with records of 9 to 117 years, matching the 1995
  database exactly. Every 2015 file has 40 years.
- **Phoenix:** the mean minimum temperature in `AZ026481.PAR` works out to 14.9 °C. The paper gives
  14.8 °C for the corrected 1995 database and 17.1 °C for the 2015 one.
- **`tests/samples/az028820.par`** (Tucson, `YEARS= 40.`) is probably the 2015 file.

This means the 1995 negative-sign error in cold-climate minimum temperatures may be present in these
files.

## International stations

For sites outside the US, two programs build a PAR file from daily data in the international GDS
format (source 3). **`GenStPar`** computes the precipitation and temperature statistics.
**`FindMatch`** fills in the rest from the most similar US station, matching on monthly P(W/W)
(49%), monthly P(W/D) (49%), elevation (1%) and latitude (1%).

## Still not covered

- the interpolation weighting formula,
- the exact periods of record used for the 1995 solar radiation, intensity, time-to-peak, dew point
  and wind inputs (listed as "unknown" in the paper),
- whether the `dat2par` source code is available (the Scheele and Hall 2000 reference points to a
  Forest Service web page that wasn't checked).
