# Ideas: using AI to help a user build a PAR file (2026-11-18)

The most useful split is to let AI handle the judgment and the workflow, and let ordinary code
compute every number. A PAR file is mostly deterministic statistics, so a language model should
never "estimate" a value from memory. What makes building one hard today is everything around those
statistics: finding data, cleaning it, filling the values a station doesn't measure, and checking
the result.

Background on how PAR files are made: `2026-11-18-par-file-generation-from-online-sources.md`.

## 1. A guided assistant that runs the pipeline

A user starts with "I have 35 years of daily data from my farm" or "make a PAR file for this
latitude/longitude." The assistant then:

1. **Finds or reads the data.** That's either the user's CSV or the nearest stations in NOAA's
   daily station archive (GHCN-Daily, which the 2015 database used). It works out the columns,
   units and missing-value codes by asking the user rather than guessing.
2. **Checks data quality before computing anything:**
   - gaps, station moves and duplicate days,
   - impossible values,
   - the kind of error the 2019 paper (Srivastava et al.) found in the 1995 database, where minimum
     temperatures below 0 °F lost their negative sign.
3. **Chooses the period of record** with the user, for example the most recent 30–40 years, or
   1974–2013 to match the 2015 database. Meyer's notes treat 30 years as the minimum for a
   representative climate.
4. **Computes the top of the file with real code:** mean precipitation per wet day, SD, skew,
   P(W/W), P(W/D), and the mean and SD of Tmax and Tmin. It converts to inches and °F and writes the
   exact fixed-width format from the ARS format document.

The AI's value here is in steps 1–3 and in explaining its choices. Step 4 is just a script.

## 2. Filling the values a station doesn't measure

This is the hard part: solar radiation, `MX .5 P`, `Time Pk`, dew point and wind. Two classic
methods exist:

- **`dat2par`'s approach:** blend three nearby stations, weighted by distance.
- **`FindMatch`'s approach:** copy them from the most similar US station, scored 49% P(W/W),
  49% P(W/D), 1% elevation, 1% latitude.

AI could improve on both:

- **A better similarity measure.** Matching on wet-day probabilities alone ignores storm intensity
  and seasonality. A model could learn which station features actually predict `MX .5 P` and
  `Time Pk`.
- **Predicting the values directly.** The 2015 database has 822 stations with their own `MX .5 P`
  and 912 with their own time-to-peak. That's enough to train a model that predicts them from daily
  precipitation statistics, location and climate, and to test it on stations left out of training.
- **Using physics and gridded data where they're better.** Solar radiation can come from
  latitude-based clear-sky calculations or gridded solar datasets, and dew point from gridded
  climate data, rather than borrowing from a station 200 km away.

Whatever the method, the PAR file should record where each value came from: computed, borrowed from
a named station, or predicted by a model.

## 3. Checking the result by running CLIGEN

After building the file, the assistant runs CLIGEN on it for 100+ years and compares the generated
monthly statistics with the input statistics, the same idea as `tests/regress.sh`. It flags known
problems:

- precipitation skew above about 4.5, which Meyer found produces poor results,
- months where CLIGEN prints "Could not produce desired level of quality" (this warning appears in
  test runs for Tucson),
- values that are implausible next to nearby stations.

The AI explains each flag in plain language and suggests a fix.

## 4. A provenance report

Alongside the PAR file, the assistant writes a short report: the data source and period, the
stations used for each borrowed value and their weights, the cleaning steps, and the validation
results. That makes the file reviewable, which matters if it feeds conservation planning.

## Cautions

- **No model-generated numbers.** Every value comes from data, a documented method, or a validated
  model, and the provenance says which.
- **Use the right surrogate set.** The local station files are the 1995 database. For borrowing
  values, the 2015 database (`parfiles-2015.zip` on the ARS site) is newer and more consistent.
- **US-only inputs.** NOAA's frequency maps for `TP6` and the TR-55 storm `TYPE` don't exist outside
  the US, so international stations need another approach. The `GenStPar`/`FindMatch` route is the
  existing answer.

## A practical first step

A small tool that:

1. reads a daily CSV and computes the top section of a PAR file,
2. borrows the remaining values from the best-matching station, using `FindMatch`'s rule to start,
3. runs CLIGEN on the result and reports how well it reproduces the inputs.

The AI part could be added on top later, as a conversational front end and then a smarter matching
method.
