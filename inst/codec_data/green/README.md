# Green

[![latest github release for green dpkg](https://img.shields.io/github/v/release/geomarker-io/codec?sort=date&filter=green-*&display_name=tag&label=%5B%E2%98%B0%5D&labelColor=%238CB4C3&color=%23396175)](https://github.com/geomarker-io/codec/releases?q=green&expanded=false)

## About

Annual measures of greenness and built environment. The codec dpkg includes the most recently available vintage for each data product (2023 greenspace and impervious, 2021 tree canopy, and 2024 EVI)

## Data

### greenspace 

The National Landcover Database (NLCD) classifies land at a 30 m x 30 m resolution into one of [16 categories](https://www.mrlc.gov/data/type/land-cover). Here, a grid cell is considered greenspace if its NLCD land cover classification is in any category other than water, ice/snow, developed medium intensity, developed high intensity, or rock/sand/clay. Grid cells are aggregated to census tracts by calculating the percentage of overlapping "green" grid cells in each tract.

### tree canopy and impervious surface

Percentage [tree canopy](https://www.mrlc.gov/data/type/tree-canopy) and [impervious surface](https://www.mrlc.gov/data/type/fractional-impervious-surface) coverage at 30 m x 30 m resolution were obtained from NLCD and aggregated to census tract by calculating the median tree canopy percentage for grid cells overlapping each tract.

### EVI

The Enhanced Vegetation Index (EVI) is a measure of greenness that ranges from -0.2 to 1, with higher values corresponding to more vegetation. A cloud-free composite EVI raster at a resolution of 250 × 250 m was created by assembling individual images collected via remote sensing between June 9 and June 24, 2024. Tract averages were calculated using the median EVI of overlapping grid cells. The EVI raster file (`MOD13Q1.A2024161.h11v05.061.2024181211403.hdf`) was downloaded from https://search.earthdata.nasa.gov/search on 2025-03-12, scaled by 0.0001, and rounded to 3 decimal points 

### park service area coverage and park greenspace coverage

The percentage of area covered by at least one park service area (defined as a 10-minute walk or 0.25 mile buffer around each park) and the percentage of area covered by park greenspace was calculated for each tract. Data was obtained from correspondence with Cincinnati Parks in August 2024.