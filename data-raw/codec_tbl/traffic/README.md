# Average Annual Vehicle-Meters Driven

## About

AADT stands for **A**verage **A**nual **D**aily **T**raffic.
Aggregated at the census tract-level, AADT is measured in vehicle-meter counts (`aadtm`) and grouped by class (`passenger`, `trucks_buses`, `tractor_trailer`).

Data is downloaded from the 2020 Highway Performance Monitoring System (HPMS) geodatabase hosted by ESRI using the {[appc](https://github.com/geomarker-io/appc)} package for R.
For more details about the HPMS, see:

- <https://www.fhwa.dot.gov/policyinformation/hpms.cfm>
- <https://data-usdot.opendata.arcgis.com/datasets/usdot::highway-performance-monitoring-system-hpms-2020/about>
- <https://www.fhwa.dot.gov/policyinformation/hpms/fieldmanual/hpms_field_manual_dec2016.pdf>

## Data

Only roads with `F_SYSTEM` classification of 1 ("interstate") or 2 ("principal arterial - other freeways and expressways") are used.
Passenger vehicles (FHWA 1-3) are calculated as the total minus FHWA class 4-7 (single unit) and 8-13 (combo).

For each 2020 census tract geography, sum the class-specific AADT for all intersecting roads, weighted by their intersecting lengths.
