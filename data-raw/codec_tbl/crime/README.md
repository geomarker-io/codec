# Crime

## About

Census tract-level measures of crime incidents (including property crimes, violent crimes, other crimes, gunshots, and reported shootings) in Hamilton County, Ohio.

Tract-level measures are derived from the data packages stored in the [`xx_address` repository](https://github.com/geomarker-io/xx_address), including [`crime_incidents-v0.1.2`](https://github.com/geomarker-io/xx_address/releases/tag/crime_incidents-v0.1.2), [`shotspotter-v0.1.2`](https://github.com/geomarker-io/xx_address/releases/tag/shotspotter-v0.1.2), and [`reported_shootings-v0.1.0`](https://github.com/geomarker-io/xx_address/releases/tag/reported_shootings-v0.1.0). View the metadata for each of these data packages for more information about their sources.

## Data

Jittered (within the same block) latitude and longitude corresponding to the location of each reported crime are available from each data source.
Crimes are aggregated to the tract level by summing the number of crimes for each tract.
For higher resolution crime data, see the [`xx_address` repository](https://github.com/geomarker-io/xx_address).
When aggregating to tract, counts are zero when there were no crimes reported in a given tract during a given month.
Counts are missing when outside the spatial or temporal extent for the respective source data:

- `crime_incidents` and `reported_shootings` are geographically limited to the City of Cincinnati
- `shotspotter` is limited to specific time periods for different target neighborhoods
