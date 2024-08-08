# Environmental Justice Index

## About

> The Environmental Justice Index uses data from the U.S. Census Bureau, the U.S. Environmental Protection Agency, the U.S. Mine Safety and Health Administration, and the U.S. Centers for Disease Control and Prevention to rank the cumulative impacts of environmental injustice on health for every census tract. ... The EJI ranks each tract on 36 environmental, social, and health factors and groups them into three overarching modules and ten different domains.


The ATSDR's [Environmental Justice Index (EJI)](https://www.atsdr.cdc.gov/placeandhealth/eji/index.html) was most recently released in 2022, but utilizes data from several older sources.
Note that although the EJI data has a `year` of 2022 in the CoDEC data package, fields in the EJI are from different sources that are each actually older. 
Find the full documentation of the EJI in PDF form here: https://eji.cdc.gov/Documents/Data/2022/EJI_2022_Data_Dictionary_508.pdf

## Data

- Data download from https://www.atsdr.cdc.gov/placeandhealth/eji/eji-data-download.html as a geodatabase
- Data fields representing "estimates" (i.e., not "percentile" or "summed ranks") selected if not avilable elsewhere (e.g., not American Community Survey estimates)
- Field names were renamed to be longer and more descriptive
- Although this is the 2022 release, 2010 vintage census tract geographies and identifers are used in the EJI data
