# CLIGEN in plain language (2026-11-18)

## What it is

CLIGEN (short for CLImate GENerator) is a computer program that makes up realistic weather. Give
it a weather station, and it produces as many years of day-by-day weather as you ask for: rain,
high and low temperatures, sunshine, humidity (dew point) and wind. It was developed by the US
Department of Agriculture in the late 1980s and 1990s and is still maintained by its soil erosion
lab in West Lafayette, Indiana.

## How it works

Think of it as a very careful dice-roller.

1. **It starts from a station's "climate fingerprint."** Each station has a parameter file, called
   a PAR file, that summarizes decades of real measurements month by month: how often it rains, how
   much it usually rains when it does, how hot and cold days typically get, and how much these
   vary. For example: "In July in Tucson, there's a fair chance of rain if it rained yesterday, and
   a typical rainy day brings a quarter inch."
2. **It rolls the dice for each day.** For every day it draws random numbers and uses the fingerprint
   to turn them into weather. First: does it rain today? (That depends partly on whether it rained
   yesterday.) If so, how much? Then: how hot, how cold, how sunny, how windy?
3. **It describes each storm,** not just the daily total: how long it lasts, when it's heaviest, and
   how intense the peak is. Most weather generators skip this, but it's what erosion models need,
   because a short downpour washes away far more soil than the same amount of gentle rain.
4. **It checks its own dice.** Random-number generators can drift and come out a little lopsided.
   CLIGEN tests its random numbers in batches and throws out batches that don't behave, so that
   over many years the made-up weather really does match the station's real averages and
   variability.

The result is weather that never actually happened, but that is statistically just like the real
climate at that place. Run it again with the same settings and you get exactly the same weather,
which makes results repeatable.

## Why use it

- **Real records are too short or have gaps.** Many places have only a few decades of data, often
  with missing days. CLIGEN can produce 100 or 1,000 years of complete weather, which is what you
  need to see rare events like the once-in-50-years storm.
- **It's built for soil erosion work.** Its main job is to drive WEPP, the USDA's model for
  predicting how much soil washes off fields and hillslopes, and WEPS, the companion wind erosion
  model. The USDA Natural Resources Conservation Service is adopting WEPP for its field offices.
  Those models need storm detail that ordinary weather records don't provide.
- **It covers thousands of locations.** Ready-made station files exist for thousands of US sites,
  and there are tools to build files for other countries from local daily records.
- **It's well tested, free and open.** It has been studied and refined for decades, and its source
  code is public.

## What to keep in mind

- **It's for long-term patterns, not individual days.** Over 30 years CLIGEN's weather matches the
  real climate well, but any single day may not make sense. For example, it decides rain and
  sunshine separately, so it can produce a rainy day that's also very sunny.
- **It reflects the climate of its data.** A station file built from 1974–2013 describes that
  period. It doesn't predict future climate unless someone builds a file for a future scenario.
- **Run it for at least 30 years.** That's the length the developers recommend for a representative
  picture of a site's climate.
