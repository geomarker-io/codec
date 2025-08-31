# American Community Survey Measures

## About

Select population, socioeconomic, and housing measures derived from the 2023 5-yr U.S. Census American Community Survey (ACS) and were selected to cover three domains:

- **Population**: number of kids, number of households (with kids), single-parent households, racial and ethnic composition, limited English speaking households
- **Socioeconomic**: education, income, poverty, employment, health insurance, public income assistance
- **Housing**: vacancy, age, value, tenure, rent to income ratio

## Data

### Types of ACS measures

Measures and their definitions are contained in `make.R`.
Generally, each of the derived ACS measures are expressed in one of three ways:

1. measures starting with **`n_`** represent a **number** of something, such as `n_household`

2. measures starting with **`prop_`** represent a **proportion** of some total, such as `prop_poverty`

3. measures starting with **`median_`** represent a **median** of a number in a population, such as `median_income`

### Missing Data

Additionally, data summaries may be [suppressed ](https://www.census.gov/programs-surveys/acs/technical-documentation/data-suppression.html) by the census bureau.
