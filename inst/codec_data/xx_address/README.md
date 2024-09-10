# Crime

[![latest github release for xx_address dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=xx_address-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=xx_address&expanded=false)

Census tract-level measures of crime incidents (including property crimes, violent crimes, other crimes, and gunshots) in Hamilton County, Ohio. Tract-level measures are derived from the data packages stored in the [`xx_address` repository](https://github.com/geomarker-io/xx_address). Version 1.0.1 of the `xx_address` CoDEC data resource harmonizes `crime_incidents-v0.1.1` and `shotspotter-v0.1.1`. View the metadata for each of these data packages for more information about their sources. 

Crime measures were geocoded to the street range, then aggregated to the tract level by summing the number of crimes for all streets that intersect the tract. If a street range overlaps more than one tract, the crimes are counted for the tract in which the majority of the street lies.
