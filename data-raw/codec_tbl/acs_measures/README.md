# American Community Survey Measures

[![latest github release for acs_measures dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=acs_measures-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=acs_measures&expanded=false)

## About

Select population, socioeconomic, and housing measures derived from the U.S. Census American Community Survey (ACS) are generally available from 2013 through 2022 and were selected to cover three domains:

- **Population**: number of kids, number of households (with kids), single-parent households, racial and ethnic composition, limited English speaking households
- **Socioeconomic**: education, income, poverty, employment, health insurance, public income assistance
- **Housing**: vacancy, age, value, tenure, rent to income ratio

## Data

### Types of ACS measures

Measures and their definitions are contained in `hh_acs_measures.R`.
Generally, each of the derived ACS measures are expressed in one of three ways:

1. measures starting with **`n_`** represent a **number** of something, such as `n_household`

2. measures starting with **`prop_`** represent a **proportion** of some total, such as `prop_poverty`

3. measures starting with **`median_`** represent a **median** of a number in a population, such as `median_income`

### Missing Data

Some data are unavailable in certain earlier years because the question was not included in the ACS that year.  Additionally, data summaries may be [suppressed ](https://www.census.gov/programs-surveys/acs/technical-documentation/data-suppression.html) by the census bureau.
