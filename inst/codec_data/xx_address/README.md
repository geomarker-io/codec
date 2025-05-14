# Crime

[![latest github release for xx_address dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=xx_address-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=xx_address&expanded=false)

Census tract-level measures of crime incidents (including property crimes, violent crimes, other crimes, gunshots, and reported shootings) in Hamilton County, Ohio. Tract-level measures are derived from the data packages stored in the [`xx_address` repository](https://github.com/geomarker-io/xx_address), including [`crime_incidents-v0.1.2`](https://github.com/geomarker-io/xx_address/releases/tag/crime_incidents-v0.1.2), [`shotspotter-v0.1.2`](https://github.com/geomarker-io/xx_address/releases/tag/shotspotter-v0.1.2), and [`reported_shootings-v0.1.0`](https://github.com/geomarker-io/xx_address/releases/tag/reported_shootings-v0.1.0). View the metadata for each of these data packages for more information about their sources. 

Jittered (within the same block) latitude and longitude corresponding to the location of each reported crime are available from each data source. Crimes aggregated to the tract level by summing the number of crimes for each tract. For higher resolution crime data, see the [`xx_address` repository](https://github.com/geomarker-io/xx_address).

When aggregating to tract, counts are zero when there were no crimes reported in a given tract during a given month. Counts are missing when outside the spatial or temporal extent for the respective source data.