# Average Annual Vehicle-Meters Driven

## About

Traffic is measured in AADTM or Annual Average Daily Traffic Meters, which is the average number of total meters driven by all vehicles per day when grouped into classes (trucks/buses, tractor/trailer, passenger).

For more details about the HPMS, see:

- <https://www.fhwa.dot.gov/policyinformation/hpms.cfm>
- <https://data-usdot.opendata.arcgis.com/datasets/usdot::highway-performance-monitoring-system-hpms-2020/about>
- <https://www.fhwa.dot.gov/policyinformation/hpms/fieldmanual/hpms_field_manual_dec2016.pdf>

## Data

Data is downloaded from the 2020 Highway Performance Monitoring System (HPMS) geodatabase hosted by ESRI using the {[appc](https://github.com/geomarker-io/appc)} package for R.
Only roads with `F_SYSTEM` classification of 1 ("interstate") or 2 ("principal arterial - other freeways and expressways") are used.
Passenger vehicles (FHWA 1-3) are calculated as the total minus FHWA class 4-7 (single unit) and 8-13 (combo).

For each 2020 census tract geography, sum the class-specific AADTM for all intersecting roads, weighted by their intersecting lengths.

### Source Rights and Licenses

HPMS data are produced by FHWA / USDOT.
As U.S. federal government data, HPMS can be redistributed and used to create derived CoDEC measures with HPMS source attribution, publication year, access URL, and access date retained.
