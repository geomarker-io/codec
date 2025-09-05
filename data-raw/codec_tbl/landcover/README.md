# Landcover, Built Environment, and Greenness

## About

Taken from https://github.com/geomarker-io/hamilton_landcover.
Census tract-level measures of greenness, imperviousness, and treecanopy are derived from the National Land Cover Database (NLCD) and NASA MODIS satellite data.

Although `year=2019` for this product, it is a compilation of other annual products with unique years denoted in the field names.

## Data

See https://github.com/geomarker-io/hamilton_landcover for data source scripts.

### Defining greenspace using NLCD land cover classifications

A grid cell is considered greenspace if its NLCD land cover classification is in any category except water, ice/snow, developed medium intensity, developed high intensity, rock/sand/clay.

### Enhanced Vegetation Index (EVI)

The Enhanced Vegetation Index (EVI) is a measure of greenness that ranges from -0.2 to 1, with higher values corresponding to more vegetation. A cloud-free composite EVI raster at a resolution of 250 × 250 m was created by assembling individual images collected via remote sensing between June 10 and June 25, 2018.
