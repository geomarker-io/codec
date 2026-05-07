# Property Code Enforcements

## About

Tract-level measures are derived from the [`property_code_enforcements-v1.0.1`](https://github.com/geomarker-io/parcel/releases/tag/property_code_enforcements-v1.0.1) data package stored in the [`parcel`](https://github.com/geomarker-io/parcel) repository.

## Data

The census tract-level number of property code enforcements (`n_property_code_enforcements`) is calculated by intersecting the jittered coordinates of the enforcements with the 2020 census tract boundaries and totaling them per year (2017 - present).

### Source Rights and Licenses

The Code Enforcement source dataset is published through the City of Cincinnati Open Data Portal with `licenseId` of `PUBLIC_DOMAIN` and attribution to the City of Cincinnati.
Source record: [Code Enforcement](https://data.cincinnati-oh.gov/api/views/cncm-znd6).
The source metadata states that records are maintained and stored by CAGIS and documents daily refresh, processing, source URL, access date, and dataset-level terms that CoDEC preserves with the derived tract-level measures.
