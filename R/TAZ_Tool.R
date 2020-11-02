
# This script processes LEHD work location data and Census ACS (tidycensus API)
# and aggregates the employment to TAZs with a given input shapefile
# for years not in the decennial census

# Author: bhargava.sana, updated by Joe Amoroso
###############################################################################

#-----------------------------------------------------------------------------------
# Packages 
#-----------------------------------------------------------------------------------
# Checking for and installing packages if necessary # 

list.of.packages <- c("tidyverse", "tidycensus","rgdal","rgeos","RCurl","reshape","data.table","R.utils","raster","readxl","writexl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Attach packages #
library(tidyverse)
library(tidycensus)
library(rgdal)
library(rgeos)
library(RCurl)
library(reshape)
library(data.table)
library(R.utils)
library(raster)
library(readxl)
library(writexl)

#setwd("C:/Users/joe.amoroso/TDOT Counts/Census_Data")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
getwd()

# Setting API key if blank # 
census_key = "b5cba8b05f656a75ac81af2be41283c07bca97bd"
tidycensus::census_api_key(key = census_key, install = T,overwrite = T)

#Inputs
#------
#Statewide Model TAZ Shapefile
# C:/Users/joe.amoroso/TDOT Counts/Census_Data/TAZ/2020/TAZ_2020

{
  dl_flag <- readline('Do you want to download LEHD data? (Y/N): ')
  gen_cswlk <- readline('Do you want to generate a crosswalk table? (Y/N): ')

if( dl_flag == 'Y' | dl_flag == 'y'){
  download_data = TRUE
  
} else {
  download_data = FALSE
  print('LEHD data will NOT be downloaded.')
}  
  
  if( gen_cswlk == 'Y' | gen_cswlk == 'y'){
    gen_cswlk = TRUE
    
  } else {
    gen_cswlk = FALSE
    print('A Crosswalk file will NOT be created.')
  
  }
}

if(gen_cswlk){
  f <- list.files(paste0(getwd(),'/inputs/Shapefile'),pattern = '*.shp')
  check <- is_empty(f)

  if(check){
    shp_path <- readline(prompt = 'Enter the Shapefile path or place in inputs/Shapefile and restart: ')
    shpdir <-  shp_path 
  } else{
  shpdir <- paste0(getwd(),'/inputs/Shapefile','/',f[1])
  }
  #shpdir <- "Q:/Projects/TN/13184_TNDOT_Statewide_Model/BRIAN_TEMP/BY2010/esri_shp/20140418_draft-v2"
  swtaz <- shapefile(shpdir)
}

census_year <- readline(prompt = 'Enter the Decennial Census Year (Hint: Probably 2010): ')
acs_year <- readline(prompt = 'Enter the ACS Year: ')
lehd_year <- readline(prompt = 'Enter the LEHD Year (skip 2011-2012): ')

#List of states that the statewide model zones cover

list  <- read_excel("inputs/FIPS_Codes.xlsx")
                               
stlist <- as.vector(list$Postal_Code)
stcodes <- as.vector(list$FIPS)

# # stlist <- c("AL","AR","DE","GA","IL",
# #             "IN","OH","PA","SC","VA",
# #             "WV","DC","MD","MO","TN",
# #             "LA","KY","NC","MS")
# stcodes <- c("01","05","10","13","17",
#              "18","39","42","45","51",
#              "54","11","24","29","47",
#              "22","21","37","28")

# URLs for downloading LEHD and Census block data (2018 ACS and 2015 LEHD) -----

baseurl <- "https://lehd.ces.census.gov/data/lodes/LODES7"
baseblkurl <- paste0("ftp://ftp2.census.gov/geo/tiger/TIGER",census_year,"/TABBLOCK/",census_year) # Make sure to add "/" after TABBLOCK for 2018

# Create Output Directories if they don't exist ---------------------- 

if(!dir.exists('outputs')){
  dir.create('outputs')
}

if(!dir.exists("outputs/State_LEHD")){
  dir.create('outputs/State_LEHD')
}

if(!dir.exists("outputs/State_Block")){
  dir.create('outputs/State_Block')
}

if(!dir.exists("outputs/LEHD_Output")){
  dir.create('outputs/LEHD_Output')
}


# Output directories and functions ----------
destdir_base <- "outputs/State_LEHD"
destblk_base <- "outputs/State_Block"
outbasedir <- "outputs/LEHD_Output"

# File download  # 
downloadOD <- function(durl,dfile)
{
  if(url.exists(durl))
      download.file(durl,dfile)
}

# Aggregate tract-level employment function # 
read_work_data <- function(x) {
  dat <- read_csv(x,col_types = cols(w_geocode = col_character()))
  return(dat)
}

# Download relevant LEHD and Census Block Shapefiles

if(download_data)
{
  print("Downloading LEHD Data. This may take some time. Take a break, you deserve it!")
  for(i in 1:length(stlist))
  {
    st <- tolower(stlist[i])
    print(c(i,st))
    
    ddir_wac <- file.path(destdir_base,st)
    if(!file.exists(ddir_wac))
      dir.create(ddir_wac,showWarnings = T)
   
    ddir <- file.path(destblk_base,st)
    if(!file.exists(ddir))
      dir.create(ddir,showWarnings = T)
    
    
    fname <- paste0(st,"_wac_S000_JT00_",lehd_year,".csv.gz")
    current_url <- paste(baseurl,st,"wac",fname,sep="/")
    download.file(current_url,file.path(ddir_wac,fname))
    gunzip(file.path(ddir_wac,fname),remove = T)
    
    fname <- paste0(st,"_wac_S000_JT03_",lehd_year,".csv.gz")
    current_url <- paste(baseurl,st,"wac",fname,sep="/")
    download.file(current_url,file.path(ddir_wac,fname))
    gunzip(file.path(ddir_wac,fname),remove = T)
    
    fname <- paste0(st,"_wac_S000_JT04_",lehd_year,".csv.gz")
    current_url <- paste(baseurl,st,"wac",fname,sep="/")
    download.file(current_url,file.path(ddir_wac,fname))
    gunzip(file.path(ddir_wac,fname),remove = T)
    
    ddir <- file.path(destblk_base,st,"BLOCK")
    if(!file.exists(ddir))
      dir.create(ddir)
    fname <- paste0("tl_",census_year,"_",stcodes[i],"_tabblock10.zip") # 2011 omits '10' at end of tabblock #
    current_url <- paste(baseblkurl,fname,sep="/")
    download.file(current_url,file.path(ddir,fname))
    unzip(file.path(ddir,fname),exdir=ddir)
  }
}

# ------------------------------------------------------------------------
# Generate and output correspondences between Census blocks and model TAZs ----
# ------------------------------------------------------------------------

if(gen_cswlk){

print('Creating Census Block to TAZ Correspondences')
for(i in 1:length(stlist))
{
  st <- tolower(stlist[i])
  print(c(i,st))
  ddir <- file.path(outbasedir,st)
  if(!file.exists(ddir))
    dir.create(ddir)
  
  cenblkdir <- file.path(destblk_base,st,"BLOCK")
  fname <- paste0("tl_",census_year,"_",stcodes[i],"_tabblock10")  # 2011 omits '10' at end of tabblock #
  cenblk <- readOGR(cenblkdir,fname)
  #convert to a points layer
  cenblkpoints <- SpatialPointsDataFrame(coordinates(cenblk),
                                         data.frame(cenblk),
                                         bbox=bbox(cenblk),
                                         proj4string=CRS(proj4string(cenblk)))
  
  
  #Add TAZ to the census block centroid points
  #-----------------------------------
  #the two shape files are in a different coordinate system
  proj4string(cenblkpoints)
  #[1] "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  proj4string(swtaz)
  #"+proj=lcc +lat_1=35.25 +lat_2=36.41666666666666 +lat_0=34.33333333333334 +lon_0=-86 +x_0=600000 +y_0=0
  #+datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"

  #convert the census block to the same utm projection as the taz
  cenblkpoints <- spTransform(cenblkpoints,CRS(proj4string(swtaz)))
  #this returns the row of data from the swtaz file for each point
  tazlist <- over(cenblkpoints,swtaz)
  #add these to the block data
  cenblkpoints@data <- cbind(cenblkpoints@data,tazlist)

  #are there some block centroids that fall outside a zone?
  nrow(cenblkpoints@data[is.na(cenblkpoints$TAZID),])
  
  if(nrow(cenblkpoints@data[is.na(cenblkpoints$TAZID),]) > 0){
    print(paste0('There are ',as.character(nrow(cenblkpoints@data[is.na(cenblkpoints$TAZID),])),'Block centroids that fall outside a TAZ.'))
  }
 
  outf <- file.path(ddir,paste0(st,"_cenblkcorr_","tnswtaz.csv"))
  write.csv(cenblkpoints@data,outf,row.names=F)
}
}

if(!gen_cswlk){
  cwk <- readline(prompt = 'Do you walk to input a Crosswalk file? (Y/N): ')
} else{
  cwk <- 'N'
}

if(cwk == 'Y' | cwk == 'y'){
  cwk_path <- readline(prompt = 'Please enter a CSV file path: ')
  cwk_file <- readr::read_csv(file.path(cwk_path))

}

if(cwk == 'Y' | cwk == 'y'){
  
    allcorr <- cwk_file
  } else{
#Read in the block-TAZ correspondences
    allcorr <- data.table()
    for(i in 1:length(stlist)){
    st <- tolower(stlist[i])
    read_dir <- file.path(outbasedir,st)
    fname <- paste0(st,"_cenblkcorr_","tnswtaz.csv")
    currdf <- read.csv(file.path(read_dir,fname))
    allcorr <- rbind(allcorr,data.table(currdf[,c("GEOID10","TAZID")])) }


    nrow(allcorr)
    nrow(allcorr[is.na(TAZID)])
    allcorr <- allcorr[!is.na(TAZID)]
    allcorr[,GEOID10:=as.character(GEOID10)]
    print('GEOID Characters:')
    table(nchar(allcorr$GEOID10))
    allcorr[,GEOID10:=ifelse(nchar(GEOID10)==14,paste0("0",GEOID10),GEOID10)]
    setkey(allcorr,GEOID10)

# Add Tract GEOID to Crosswalk File # 
    allcorr <- allcorr %>% 
       mutate(GEOID_Tract = substr(GEOID10,1,11)) %>%   # GEOID for tracts are 11 digits, Census Block Groups are 12 digits, Blocks are 15 #
       relocate(GEOID_Tract,.after=GEOID10)                                                 

  
}

# Get Tract Correlation from Block LEHD Correlation # 
# bgcorr <- allcorr %>% 
#   mutate(GEOID10 = substr(GEOID10,1,12)) %>% # GEOID for Census Block Groups are 12 digits, Blocks are 15 #
#   dplyr::rename(GEOID_BG = GEOID10) %>% 
#   distinct()
  
# Gather ACS Variables --------------

#v10 <- load_variables(acs_year, "acs5", cache = FALSE)

sf1 <- function(states){
 
   county_codes <- fips_codes[which(fips_codes$state == states),]$county_code  

   tidycensus::get_decennial(geography = 'block',
                               variables = 'H003002', # Total Occupied HH by block #
                               geometry = F,
                               state = states,
                               year = as.numeric(census_year),
                               county = county_codes)
}

us_sf1_tot_hh <- map(stlist,sf1)
us_sf1_tot_hh <- rbindlist(us_sf1_tot_hh)

acs_tot_pop <- function(states){ 
                 county_codes <- fips_codes[which(fips_codes$state == states),]$county_code  
                 tidycensus::get_acs(geography = 'tract',  # Total Population for every tract in US # 
                          variables = 'B01003_001',
                          geometry = F,
                          state = states,
                          county = county_codes,
                          year = as.numeric(acs_year),
                          show_call = F)
}

us_acs_tot_pop <- map(stlist,acs_tot_pop)
us_acs_tot_pop <- rbindlist(us_acs_tot_pop)

acs_gc_pop <- function(states){
              county_codes <- fips_codes[which(fips_codes$state == states),]$county_code
              tidycensus::get_acs(geography = 'tract',  # Total GQ Population # 
                         variables = 'B26001_001',
                         geometry = F,
                         state = states,
                         #county = county_codes,
                         year = as.numeric(acs_year))
}

us_acs_gc_pop <- map(stlist,acs_gc_pop)
us_acs_gc_pop <- rbindlist(us_acs_gc_pop)

acs_hh_pop <- function(states){
              county_codes <- fips_codes[which(fips_codes$state == states),]$county_code
              tidycensus::get_acs(geography = 'tract',  # Total HH Population in occupied housing units # 
                         variables = 'B25026_001',
                         geometry = F,
                         state = stlist,
                         #county = county_codes,
                         year = as.numeric(acs_year))
}

us_acs_hh_pop <- map(stlist,acs_hh_pop)
us_acs_hh_pop <- rbindlist(us_acs_hh_pop)

acs_tot_hh <- function(states){ 
              county_codes <- fips_codes[which(fips_codes$state == states),]$county_code
             tidycensus::get_acs(geography = 'tract',      # Total Housing Units # 
                         variables = 'B25001_001',
                         geometry = F,
                         state = states,
                         #county = county_codes,
                         year = as.numeric(acs_year))

}

us_acs_tot_hh <- map(stlist,acs_tot_hh)
us_acs_tot_hh <- rbindlist(us_acs_tot_hh)

acs_occ_hh <- function(states){
            county_codes <- fips_codes[which(fips_codes$state == states),]$county_code
            tidycensus::get_acs(geography = 'tract',      # Total Occupied Housing Units # 
                         variables = 'B25002_002',
                         geometry = F, 
                         state = states,
                         #county = county_codes,
                         year = as.numeric(acs_year))

}

us_acs_occ_hh <- map(stlist,acs_occ_hh)
us_acs_occ_hh <- rbindlist(us_acs_occ_hh)

acs_vac_hh <- function(states){
                 county_codes <- fips_codes[which(fips_codes$state == states),]$county_code
              tidycensus::get_acs(geography = 'tract',      # Total Vacant Housing Units # 
                         variables = 'B25002_003',
                         geometry = F,
                         state = states,
                         #county = county_codes,
                         year = as.numeric(acs_year))

}
us_acs_vac_hh <- map(stlist,acs_vac_hh)
us_acs_vac_hh <- rbindlist(us_acs_vac_hh)

acs_avg_hh_inc <- function(states){ 
                  county_codes <- fips_codes[which(fips_codes$state == states),]$county_code
                  tidycensus::get_acs(geography = 'tract',  # Median Income # 
                                      variables = 'B19013_001',
                                      geometry = F,
                                      state = states,
                                      #county = county_codes,
                                      year = as.numeric(acs_year))
 
}
us_acs_avg_hh_inc <- map(stlist,acs_avg_hh_inc)
us_acs_avg_hh_inc <- rbindlist(us_acs_avg_hh_inc)

acs_num_veh <- function(states){
                        county_codes <- fips_codes[which(fips_codes$state == states),]$county_code
                        tidycensus::get_acs(geography = 'tract',    # Aggregate number of vehicles available # 
                                            variables = 'B25046_001',
                                            geometry = F,
                                            state = states,
                                            year = as.numeric(acs_year),
                                            #county = county_codes,
                                            show_call = F)
}
us_acs_num_veh <- map(stlist,acs_num_veh)
us_acs_num_veh <- rbindlist(us_acs_num_veh)

acs_num_wrk <- function(states){
                         county_codes <- fips_codes[which(fips_codes$state == states),]$county_code
                         tidycensus::get_acs(geography = 'tract',    # Total number of people in labor force  # 
                                            variables = 'B08301_001',
                                            geometry = F,
                                            state = states,
                                            year = as.numeric(acs_year),
                                            #county = county_codes,
                                            show_call = F)
}

us_acs_num_wrk <- map(stlist,acs_num_wrk)
us_acs_num_wrk <- rbindlist(us_acs_num_wrk)


us_acs_tot_students <- tidycensus::get_acs(geography = 'tract',    # Tot number of student in HH (children ages 5-17) # 
                               variables = c('B09001_005','B09001_006','B09001_007','B09001_008','B09001_009'),
                               geometry = F,
                               state = stlist,
                               #county = county_codes,
                               year = as.numeric(acs_year))

us_acs_tot_students <- us_acs_tot_students %>%  # Aggregate 5 student-age variables
  group_by(GEOID) %>% 
  summarize(TOT_STUDENT = sum(estimate))


us_acs_senhh <- get_acs(geography = 'tract',    # Households - Households with one or more people 65 years and over # 
                        variables = 'B11007_002',  
                        geometry = F, state = stlist, year = as.numeric(acs_year))


# Combining and cleaning ACS data #

CENSUS_HH <- us_sf1_tot_hh %>%
  dplyr::select(GEOID,CENSUSHH = value)
  
TOT_POP <- us_acs_tot_pop %>% 
  dplyr::select(GEOID,TOTPOP = estimate)

GQ_POP <- us_acs_gc_pop %>%
  dplyr::select(GEOID,GQPOP = estimate)

HH_POP <- us_acs_hh_pop %>% 
  dplyr::select(GEOID,HHPOP = estimate)

TOT_HH <- us_acs_tot_hh %>% 
  dplyr::select(GEOID,TOTHH = estimate) 

TOT_OCC_HH <-  us_acs_occ_hh %>% 
  dplyr::select(GEOID,TOT_OCC_HH = estimate)

TOT_VAC_HH <- us_acs_vac_hh %>% 
  dplyr::select(GEOID,TOT_VAC_HH = estimate)

HH_INC <- us_acs_avg_hh_inc %>% 
  dplyr::select(GEOID,HHINC = estimate)

TOT_WORK <- us_acs_num_wrk %>% 
  dplyr::select(GEOID,TOTWRK = estimate)

TOT_VEH <- us_acs_num_veh %>%
  dplyr::select(GEOID,TOTVEH = estimate)

TOT_STU <- us_acs_tot_students %>%
  dplyr::select(GEOID,TOT_STUDENT)

SEN_HH <- us_acs_senhh %>% 
  dplyr::select(GEOID,SENHH = estimate)

ACS <- TOT_POP %>% 
  left_join(HH_POP,by = 'GEOID') %>% 
  left_join(TOT_HH, by = 'GEOID') %>% 
  left_join(GQ_POP, by = 'GEOID') %>% 
  left_join(TOT_OCC_HH, by = 'GEOID') %>% 
  left_join(TOT_VAC_HH, by = 'GEOID') %>%
  left_join(HH_INC, by ='GEOID') %>% 
  left_join(TOT_WORK, by = 'GEOID') %>% 
  left_join(TOT_VEH, by = 'GEOID') %>%
  left_join(TOT_STU, by = 'GEOID') %>% 
  left_join(SEN_HH, by = 'GEOID')
  

 # -------------------------------------------------------
 # Aggregate 50 state (and DC) block-level employment data
 #--------------------------------------------------------
 
 LEHD_files_all     <- list.files(destdir_base, pattern = paste0('*JT00_',census_year,'.csv'),full.names = T, recursive = T)
 #LEHD_files_private <- list.files(destdir_base, pattern = '*JT03_2010.csv',full.names = T, recursive = T)
 #LEHD_files_fed     <- list.files(destdir_base, pattern = '*JT04_2010.csv',full.names = T, recursive = T)
 
 lehd_all_list     <- sapply(LEHD_files_all,read_work_data,simplify = F, USE.NAMES = T)
 #lehd_private_list <- sapply(LEHD_files_private,read_work_data,simplify = F, USE.NAMES = T)
 #lehd_fed_list     <- sapply(LEHD_files_fed,read_work_data,simplify = F, USE.NAMES = T)
 
 lehd_all     <- rbindlist(lehd_all_list)
 #lehd_private <- rbindlist(lehd_private_list)
 #lehd_fed     <- rbindlist(lehd_fed_list)
 
 rm(lehd_all_list)  # ,lehd_private_list,lehd_fed_list)
 
 # Aggregate Block group data by census tract (All employment) ------------
 lehd_all <- lehd_all %>% 
  dplyr::rename(Total_Jobs = C000,
             NAICS_11 = CNS01,
             NAICS_21 = CNS02,
             NAICS_22 = CNS03,
             NAICS_23 = CNS04,
             NAICS_31_33 = CNS05,
             NAICS_42 = CNS06,
             NAICS_44_45 = CNS07,
             NAICS_48_49 = CNS08,
             NAICS_51 = CNS09,
             NAICS_52 = CNS10,
             NAICS_53 = CNS11,
             NAICS_54 = CNS12,
             NAICS_55 = CNS13,
             NAICS_56 = CNS14,
             NAICS_61 = CNS15,
             NAICS_62 = CNS16,
             NAICS_71 = CNS17,
             NAICS_72 = CNS18,
             NAICS_81 = CNS19,
             NAICS_92 = CNS20) %>% 
   dplyr::select(w_geocode,Total_Jobs,NAICS_11,NAICS_21,NAICS_22,NAICS_23,
                 NAICS_31_33,NAICS_42,NAICS_44_45,NAICS_48_49,
                 NAICS_51,NAICS_52,NAICS_53,NAICS_54,NAICS_55,
                 NAICS_56,NAICS_61,NAICS_62,NAICS_71,NAICS_72,
                 NAICS_81, NAICS_92
                 )
 
 # Join LEHD Data to Crosswalk File  ---------------
 
 
 crosswalk <- allcorr %>% 
   left_join(CENSUS_HH, by = c('GEOID10' = 'GEOID'))
 
 # Join ACS tract-level total occ hh to get shares #
 
 crosswalk <- crosswalk %>% 
   left_join(TOT_OCC_HH, by = c('GEOID_Tract' = 'GEOID'))
 
 crosswalk$SHARE <- crosswalk$CENSUSHH / crosswalk$TOT_OCC_HH
 crosswalk$SHARE <- ifelse(is.infinite(crosswalk$SHARE) | is.nan(crosswalk$SHARE),0,crosswalk$SHARE)
 
 # Output crosswalk with shares/ hhs to outputs # 
 if(gen_cswlk){
   write_csv(crosswalk,file.path('outputs/Crosswalk_File.csv'))
 } 
 
 # Join LEHD Block Level to crosswalk file # 
 land_use <- crosswalk %>% 
   left_join(lehd_all, by = c('GEOID10' = 'w_geocode'))
 
 ACS <- ACS %>% 
   dplyr::select(-TOT_OCC_HH)
 
 land_use <- land_use %>% 
   left_join(ACS, by = c('GEOID_Tract' = 'GEOID'))
 
 # Create mean HH size for TAZ #
 land_use <- land_use %>%
   mutate(HHSIZE = TOTPOP / CENSUSHH)
 
 
# Disaggregate ACS data to blocks #
 
  land_use <- land_use %>%
   group_by(GEOID_Tract) %>%
   mutate(TOTPOP = TOTPOP * SHARE,
          HHPOP = HHPOP * SHARE,
          GQPOP = GQPOP * SHARE,
          HH = TOT_OCC_HH * SHARE,
          SENHH = SENHH * SHARE,
          HHSIZE = HHSIZE * SHARE,
          HHWRK = (TOTWRK * SHARE) / CENSUSHH,
          HHVEH = (TOTVEH * SHARE) / CENSUSHH,
          HHSTD = (TOT_STUDENT * SHARE) / CENSUSHH)
 
 # Aggregate Land Use to TAZ # 
 
 land_use_job <- land_use %>% 
   group_by(TAZID) %>% 
   summarize(across(starts_with(c('NAICS','Total')),~sum(.x,na.rm = T)))
 
 land_use_acs <- land_use %>% 
   group_by(TAZID) %>% 
   summarize(TOTPOP = sum(TOTPOP,na.rm = T),
             HHPOP = sum(HHPOP,na.rm = T),
             GQPOP = sum(GQPOP,na.rm = T),
             HH = sum(HH,na.rm = T),
             HHSIZE = mean(HHSIZE,na.rm = T),
             HHINC = mean(HHINC,na.rm = T),
             SENHH = sum(SENHH,na.rm = T),
             HHWRK = mean(HHWRK,na.rm = T),
             HHVEH = mean(HHVEH,na.rm = T),
             HHSTD = mean(HHSTD,na.rm = T)
             )

 land_use_taz <- land_use_acs %>%
   left_join(land_use_job, by = 'TAZID') %>%
   relocate(Total_Jobs, .after = HHSTD)
 
 
 # Write new TAZ landuse to EXcel file, or CSV # 
 output <- readline('Do you want a CSV or XLSX file? (CSV/XLSX): ')
 
 if(output == 'XLSX' | output == 'xlsx'){
 write_xlsx(land_use_taz,paste0('outputs/','TN_',acs_year,'_TAZ.xlsx'))
 } else{
   write_csv(land_use_taz,paste0('outputs/','TN_',acs_year,'_TAZ.csv'))
 }
 
 #####################################################################################################################################
 # pop_emp <- ACS_2018_5YR_Populaton %>% 
 #   left_join(emp, by = c('GEOID' = 'GEOID_Tract'))
 # 
 # # Add Avg HH Income Data---------
 # ACS_S1901 <-  read_csv("ACS_2018_MedHHInc/ACSST5Y2018.S1901_data_with_overlays_2020-09-08T121758.csv")
 # 
 # ACS_S1901 <- ACS_S1901[-1,]
 # 
 # hh_inc <- ACS_S1901 %>% 
 #   select(GEO_ID,S1901_C01_013E) %>%  # S1901_C01_013E = Median Income Per HH # 
 #   mutate(GEO_ID = substr(GEO_ID,10,nchar(GEO_ID))) %>% # Remove first 9 characters in ACS file to join to other GEOIDs # 
 #   rename(AVG_HH_INC = S1901_C01_013E)
 # 
 # pop_emp_inc <- pop_emp %>% 
 #   left_join(hh_inc,by = c('GEOID' = 'GEO_ID'))
 # 
 # ---------------------------------------------------------------------------- 
 #  alltrips <- data.table()
 # for(i in 1:length(stlist))
 # {
 #   st <- tolower(stlist[i])
 #   print(c(i,st))
 #   
 #   stdt <- data.table()
 #   ddir <- file.path(destdir_base,st)
 #   fname <- paste0(st,"_od_main_JT00_2010.csv.gz")
 #   stdt <- rbind(stdt,data.table(read.csv(gzfile(file.path(ddir,fname)),as.is=T)))
 #   fname <- paste0(st,"_od_aux_JT00_2010.csv.gz")
 #   stdt <- rbind(stdt,data.table(read.csv(gzfile(file.path(ddir,fname)),as.is=T)))
 #   stdt <- stdt[,list(w_geocode,h_geocode,S000)]
 #   stdt[,w_geocode:=as.character(w_geocode)]
 #   stdt[,w_geocode:=ifelse(nchar(w_geocode)==14,paste0("0",w_geocode),w_geocode)]
 #   stdt[,h_geocode:=as.character(h_geocode)]
 #   stdt[,h_geocode:=ifelse(nchar(h_geocode)==14,paste0("0",h_geocode),h_geocode)]
 #   setkey(stdt,w_geocode)
 #   setnames(allcorr,c("w_geocode","wtaz"))
 #   stdt <- merge(stdt,allcorr,all.x=T)
 #   setkey(stdt,h_geocode)
 #   setnames(allcorr,c("h_geocode","htaz"))
 #   stdt <- merge(stdt,allcorr,all.x=T)
 #   stdt <- stdt[!is.na(htaz) & !is.na(wtaz)]
 #   setkey(stdt,htaz,wtaz)
 #   stdt <- stdt[,list(trips=sum(S000)),by=list(htaz,wtaz)]
 #   alltrips <- rbind(alltrips,stdt)
 # }
 # nrow(alltrips)
 # alltrips <- alltrips[,list(trips=sum(trips)),by=list(htaz,wtaz)]
 # nrow(alltrips)
 # #Add the return from work trips for OD matrix
 # alltrips_ret <- alltrips
 # setnames(alltrips_ret,c("wtaz","htaz","trips"))
 # alltrips <- rbind(alltrips,alltrips_ret)
 # alltrips <- alltrips[,list(trips=sum(trips)),by=list(htaz,wtaz)]
 # alltrips <- alltrips[order(htaz,wtaz)]
 # #Output work OD flows
 # write.csv(alltrips,file.path(outbasedir,"work_ODtriptable_tnsw.csv"),row.names=F)
 # 
 
 # # Aggregate Block group data by census tract (private employment) ------------
 # lehd_private <- lehd_private %>% 
 #   mutate(GEOID_Tract = substr(w_geocode,1,11)) %>%  # GEOID for Census Tract are 11 digits, Block groups are 12-15 #
 #   group_by(GEOID_Tract) %>%
 #   summarize(Total_Jobs = sum(C000),
 #             NAICS_11 = sum(CNS01),
 #             NAICS_21 = sum(CNS02),
 #             NAICS_22 = sum(CNS03),
 #             NAICS_23 = sum(CNS04),
 #             NAICS_31_33 = sum(CNS05),
 #             NAICS_42 = sum(CNS06),
 #             NAICS_44_45 = sum(CNS07),
 #             NAICS_48_49 = sum(CNS08),
 #             NAICS_51 = sum(CNS09),
 #             NAICS_52 = sum(CNS10),
 #             NAICS_53 = sum(CNS11),
 #             NAICS_54 = sum(CNS12),
 #             NAICS_55 = sum(CNS13),
 #             NAICS_56 = sum(CNS14),
 #             NAICS_61 = sum(CNS15),
 #             NAICS_62 = sum(CNS16),
 #             NAICS_71 = sum(CNS17),
 #             NAICS_72 = sum(CNS18),
 #             NAICS_81 = sum(CNS19),
 #             NAICS_92 = sum(CNS20))
 # 
 # # Aggregate Block group data by census tract (federal employment) ------------
 # lehd_fed <- lehd_fed %>% 
 #   mutate(GEOID_Tract = substr(w_geocode,1,11)) %>%  # GEOID for Census Tract are 11 digits, Block groups are 12-15 #
 #   group_by(GEOID_Tract) %>%
 #   summarize(Total_Jobs = sum(C000),
 #             NAICS_11 = sum(CNS01),
 #             NAICS_21 = sum(CNS02),
 #             NAICS_22 = sum(CNS03),
 #             NAICS_23 = sum(CNS04),
 #             NAICS_31_33 = sum(CNS05),
 #             NAICS_42 = sum(CNS06),
 #             NAICS_44_45 = sum(CNS07),
 #             NAICS_48_49 = sum(CNS08),
 #             NAICS_51 = sum(CNS09),
 #             NAICS_52 = sum(CNS10),
 #             NAICS_53 = sum(CNS11),
 #             NAICS_54 = sum(CNS12),
 #             NAICS_55 = sum(CNS13),
 #             NAICS_56 = sum(CNS14),
 #             NAICS_61 = sum(CNS15),
 #             NAICS_62 = sum(CNS16),
 #             NAICS_71 = sum(CNS17),
 #             NAICS_72 = sum(CNS18),
 #             NAICS_81 = sum(CNS19),
 #             NAICS_92 = sum(CNS20))
 # 
 # # land_use <- ACS %>% 
 #   left_join(lehd_all,by = c('GEOID' = 'GEOID_Tract'))
 # 
 # # Join Tract to TAZ correlation to land use # 
 # land_use <- land_use %>% 
 #   left_join(allcorr,by = c('GEOID' = 'GEOID_Tract')) %>% 
 #   relocate(TAZID,.after = GEOID)
 