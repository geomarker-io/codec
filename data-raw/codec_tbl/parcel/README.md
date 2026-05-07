# Parcel Characteristics

## About

Census tract-level measures of parcel characteristics for all residential parcles in Hamilton County, Ohio.
Tract-level measures are derived from the data packages stored in the [`parcel`](https://github.com/geomarker-io/parcel) repository.
Version 0.1.0 of the `parcel` CoDEC data resource harmonizes [`cagis_parcels-v1.1.1`](https://github.com/geomarker-io/parcel/releases/tag/cagis_parcels-v1.1.1) and [`auditor_online_parcels-v0.2.1`](https://github.com/geomarker-io/parcel/releases/tag/auditor_online_parcels-v0.2.1).
View the metadata for each of these data packages for more information about their sources.

## Data

Parcel-level measures were aggregated to the tract level:

- median: `market_total_value`, `acreage`, `year_built`, and number of rooms
- fraction of parcels: by land use type, by homestead flag

Parcel land use types were grouped into more general categories:

- apartments: `apartment, 4-19 units`, `apartment, 20-39 units`, `apartment, 40+ units`, `office / apartment over`
- assisted housing: `metropolitan housing authority`, `lihtc res`
- condominiums: `condominium unit`, `condo or pud garage`
- single family homes: `single family dwelling`
- two to three family homes: `two family dwelling`, `three family dwelling`
- other

### Source Rights and Licenses

CAGIS parcel polygons are published through the City of Cincinnati Open Data Portal with `licenseId` of `PUBLIC_DOMAIN` and attribution to the Cincinnati Area Geographic Information Systems consortium (CAGIS).
Source record: [Hamilton County Parcel Polygons](https://data.cincinnati-oh.gov/api/views/g24g-2pi5).
The linked CAGIS ArcGIS item also provides the data as public open data with an as-is/no-warranty disclaimer.
Linked source terms: [Hamilton County Parcels - Open Data](https://www.arcgis.com/home/item.html?id=24faaf8c6d7948c5abea26d834e282bf).
Hamilton County Auditor downloads state that website data are public records under Ohio Revised Code 149.43, while also stating that the Auditor does not sell the data or endorse commercial use.
Auditor source terms: [Downloads and Public Records Request](https://www.hcauditor.org/downloads.asp).
CoDEC preserves CAGIS attribution, the CAGIS disclaimer, Auditor public-record language, source URLs, and access dates; raw Auditor attribute redistribution remains the part needing the clearest source-specific documentation.
