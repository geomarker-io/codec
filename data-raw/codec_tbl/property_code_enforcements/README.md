# Property Code Enforcements

## About

Tract-level measures are derived from the [`property_code_enforcements-v1.0.1`](https://github.com/geomarker-io/parcel/releases/tag/property_code_enforcements-v1.0.1) data package stored in the [`parcel`](https://github.com/geomarker-io/parcel) repository.

## Data

The census tract-level number of property code enforcements (`n_property_code_enforcements`) is calculated by intersecting the jittered coordinates of the enforcements with the 2020 census tract boundaries and totaling them per year (2017 - present).
