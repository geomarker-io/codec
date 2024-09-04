# Harmonized Historical American Community Survey Measures

[![latest github release for hh_acs_measures dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=hh_acs_measures-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=hh_acs_measures&expanded=false)

## About

Select population, socioeconomic, and housing measures derived from the U.S. Census American Community Survey (ACS) are generally available from 2013 through 2022 and were selected to cover three domains:

- **Population**: number of kids, number of households (with kids), single-parent households, racial and ethnic composition, limited English speaking households
- **Socioeconomic**: education, income, poverty, employment, health insurance, public income assistance
- **Housing**: vacancy, age, value, tenure, rent to income ratio

## Data

### Types of ACS measures

Measures and their definitions based on the census variable names are listed below.  See the `hh_acs_measures.R` file for more details.

```
n_households ~ B11005_001E,
n_households_children ~ B11005_002E,
n_housing_units ~ B25001_001E,
median_home_value ~ B25077_001E,
prcnt_poverty ~ B17001_001E / B17001_002E,
prcnt_recieved_public_assistance_income ~ B19058_001E / B19058_002E,
prcnt_family_households_with_single_householder ~ B11001_004E / B11001_002E,
prcnt_employment_among_civilian_workforce ~ B23025_004E / B23025_003E,
prcnt_housing_units_occupied_by_renters ~ B25003_003E / B25003_001E,
prcnt_median_rent_to_income_ratio_among_renters ~ B25071_001E,
prcnt_housing_units_vacant ~ B25002_003E / B25002_001E,
prcnt_white_and_not_hispanic_or_latino ~ B03002_003E / B03002_001E,
prcnt_black_and_not_hispanic_or_latino ~ B03002_004E / B03002_001E,
prcnt_white_and_hispanic_or_latino ~ B03002_013E / B03002_001E,
prcnt_black_and_hispanic_or_latino ~ B03002_014E / B03002_001E
```

Each of the derived ACS measures are expressed in one of three ways:

1. measures starting with **`n_`** represent a **number** of something, such as `n_household`
    - always rounded to the nearest integer

2. measures starting with **`fraction_`** represent a **fraction** of some total, such as `fraction_poverty`
    - bounded between 0 and 1
	- always rounded to three decimal places (e.g. 0.064)

3. measures starting with **`median_`** represent a **median** of a number in a population, such as `median_income`
    - always expressed using three significant digits (e.g., 5,410 and 145,000)

Variables can be `n_`, `median_` or some other count, or `prcnt_` which is a percentage type and is defined using a numerator and denominator.

### Missing Data

Some data are unavailable in certain earlier years because the question was not included in the ACS that year.  Additionally, data summaries may be [suppressed ](https://www.census.gov/programs-surveys/acs/technical-documentation/data-suppression.html) by the census bureau.
