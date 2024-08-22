# Harmonized Historical American Community Survey Measures

Selected measures from the American Community Survey are downloaded and harmonized in R.

Variables can be `n_`, `median_` or some other count, or `prcnt_` which is a percentage type and is defined using a numerator and denominator.

Measures and their definitions based on the census variable names are listed below.  See the `hh_acs_measures.R` file for more details

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
