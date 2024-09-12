# Parcel Characteristics

[![latest github release for parcel dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=parcel-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=parcel&expanded=false)

Census tract-level measures of parcel characteristics for all residential parcles in Hamilton County, Ohio. Tract-level measures are derived from the data packages stored in the [`parcel`](https://github.com/geomarker-io/parcel) repository. Version 0.1.0 of the `parcel` CoDEC data resource harmonizes [`cagis_parcels-v1.1.1`](https://github.com/geomarker-io/parcel/releases/tag/cagis_parcels-v1.1.1) and [`auditor_online_parcels-v0.2.1`](https://github.com/geomarker-io/parcel/releases/tag/auditor_online_parcels-v0.2.1). View the metadata for each of these data packages for more information about their sources. 

Parcel-level measures were aggregated to the tract level: 

- median: `market_total_value`, `acreage`, `year_built`, and number of rooms
- fraction of parcels: by land use type, by homestead flag

Parcel land use types were grouped into more general categories as follows: 

- apartments: `apartment, 4-19 units`, `apartment, 20-39 units`, `apartment, 40+ units`, `office / apartment over`
- assisted housing: `metropolitan housing authority`, `lihtc res`
- condominiums: `condominium unit`, `condo or pud garage`
- single family homes: `single family dwelling`
- two to three family homes: `two family dwelling`, `three family dwelling`
- other
