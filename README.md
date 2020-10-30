# TAZ Tool
An R tool to pull LEHD, Decennial Ceneus, and ACS Data to create TAZ land use tables from an input shapefile. This was developed in an effort to create
land use data for transportation model years that do not fall on a decennial census year, but have LEHD and ACS data available.
## Overview
This model will create three directories (State_LEHD, State_Block, State_Output), if specified, and will read in all LEHD employment data for the specified states, their Census Block geograhies, and output TAZ crosswalk files for each state specified and aggregate the data. Then, using the `tidycensus` API, ACS data is gathered for the specified year and combined to create a TAZ land use table.


### Inputs:
- A TAZ .shp file
- A .xlsx file with the desired state and corresponding FIPS codes
### Outputs:
- A Crosswalk file linking TAZ IDs to Census Block GEOIDs and Census Tract GEOIDs (if specified)
- A TAZ landuse .xlsx or .csv file containing NAICS employment data and Census demograhic data for the specified years
