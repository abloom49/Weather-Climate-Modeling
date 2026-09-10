# Econ 108 Project

This project studies whether basic weather metrics show evidence of climate change over time.

## Research question
How have daily temperature, precipitation, and tornado occurrence changed over time in the United States?

## Data
- Daily weather summaries for major U.S. cities
- Tornado event records from the NOAA/NCEI Storm Events Database

## Repository structure
- `scripts/temperature/` — temperature and precipitation analyses
- `scripts/tornado/` — tornado-focused analyses
- `scripts/archive/` — older or exploratory versions kept for reference
- `data/raw/Weather Data/` — raw source files
- `data/cleaned/Cleaned Data/` — cleaned and merged datasets
- `docs/` — project writeup and proposal
- `outputs/` — figures and summary tables

## Main analyses
- Regression analysis of temperature and precipitation trends
- Extreme heat/cold analysis using percentile thresholds
- Tornado frequency and occurrence models
- Geographic and clustering visualizations

## Notes
This repository is organized by analysis topic rather than a single “master” script, since different files address different parts of the project.

## Files to start with
- `scripts/temperature/ProjectCode.R`
- `scripts/tornado/ProjectCodeTornado.R`
- `docs/Econ 108 Project Writeup - Google Docs.pdf`
