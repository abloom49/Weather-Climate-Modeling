#  Time-Series and Geospatial Analysis of U.S. Weather Trends (undergraduate project)

This project studies whether basic weather metrics show evidence of climate change over time.

## Research question
How have daily temperature, precipitation, and tornado occurrence changed over time in the United States?

## Data
- Daily weather summaries for major U.S. cities
- Tornado event records from the NOAA/NCEI Storm Events Database

## Repository Structure
- `scripts/temperature/` — temperature and precipitation trend analysis
- `scripts/tornado/` — tornado event modeling and geospatial analysis
- `scripts/archive/` — older exploratory and duplicate analyses
- `data/raw/` — raw input datasets
- `data/cleaned/` — cleaned and merged datasets
- `outputs/` — figures, tables, and summary outputs
- `docs/` — project writeup and supporting documentation

## Main analyses
- Time-series regression of daily temperature and precipitation levels across major U.S. cities
- Percentile-based identification of extreme heat and cold days using historical thresholds
- Geospatial mapping and k-means clustering of state-level tornado risk and weather exposure
- Time-varying variation and dispersion analysis of daily weather conditions
- Alternative functional-form testing

## Notes
This repository is organized by analysis topic rather than a single “master” script, since different files address different parts of the project.

## Files to start with
- `scripts/temperature/ProjectCode.R`
- `scripts/tornado/ProjectCodeTornado.R`
- `docs/Econ 108 Project Writeup - Google Docs.pdf`
