setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library(raster)
library(utils)
library(rgeos)
library(rgdal)
library(fs)
library(RStoolbox) #
library(tidyverse)
library(sf)
library(httr)
library(jsonlite)


base <- 'https://urban.microsoft.com/api/EclipseData/'

api_key <- ' 7C713FE6-BB43-484E-963A-EBA89A203658'

device_list <- "GetDeviceList?city=Chicago"

latest_reading <- "GetLatestReadings?city=Chicago"


#Tthis requires more optional parameters
# The amount of data returned by this API call can be massive. 
# Timeouts may occur. See notes below. . If devices (subset of devices separated 
# by comma as query parameter) is not included then the maximum timeframe is 24 
# hours. If devices is included then the maximum timeframe is 7 days.

# 
# OPTIONAL PARAMETER : city (limits to devices for a specific deployment)
# 
# OPTIONAL PARAMETER : devices (limits the result to a subset of specific devices)
# 
# OPTIONAL PARAMETER : hours ( latest N Hours -- limits the result to the most recent 
#                              N hours)
# 
# OPTIONAL PARAMETERS : BeginDateTime and/or EndDateTime : limit the result to a 
# specific timeframe (note - these are UTC. Note - "2021-05-24" is identical to 
# "2021-05-24 00:00". Use time = 23:59:59 to indicate end-of-day".


#SAMPLE CALL : https://urban.microsoft.com/api/EclipseData/GetReadings?devices=2005,2009&hours=24

#SAMPLE CALL : https://urban.microsoft.com/api/EclipseData/GetReadings?devices=2005&startDateTime=2021-08-01&endDateTime=2021-08-01%2023:59:59

# this gives all the readings in CHicago for the last 48 hours
#sensor_readings <- "GetReadings?city=Chicago&hours=48"
sensor_readings <- "GetReadings?city=Chicago&startDateTime=2021-08-01&endDateTime=2021-08-01%2023:59:59"

API_URL <- paste0(base, device_list)
raw_data <- GET(API_URL,add_headers(ApiKey=api_key))
df_device_list <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
df_device_list$latitude <- as.numeric(df_device_list$latitude)
df_device_list$longitude <- as.numeric(df_device_list$longitude)
df_device_list <- df_device_list %>% dplyr::filter(latitude>1 & latitude<8000)
df_device_list <- df_device_list %>% dplyr::filter(longitude>-100)
start_dates <- df_device_list$deploymentStartDateTime
end_dates <- df_device_list$deploymentEndDateTime
df_device_list <- df_device_list %>% dplyr::filter(
  (lubridate::ymd_hms(deploymentEndDateTime)-lubridate::ymd_hms(deploymentStartDateTime))>lubridate::days(7)
)
df_device_list <- df_device_list[!rev(duplicated(rev(df_device_list$msrDeviceNbr))),]
df_device_list<- st_as_sf(df_device_list,coords = c("longitude","latitude"),crs="+proj=longlat +datum=WGS84")
df_device_list %>% st_geometry() %>% plot()
# 147 total device in chicago, but not all are active

list_of_devices <- df_device_list$msrDeviceNbr
start_dates <- df_device_list$deploymentStartDateTime
end_dates <- df_device_list$deploymentEndDateTime

# API_URL <- paste0(base, latest_reading)
# raw_data <- GET(API_URL,add_headers(ApiKey=api_key))
# df_latest_reading <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
# # only 116 devices in the latest reading

df_loop <- data.frame(msrDeviceNbr=character(),
                         mean_calibratedpm25=as.numeric(),
                         max_calibratedpm25=as.numeric(),
                         min_calibratedpm25=as.numeric(),
                         mean_pM25=as.numeric(),
                         max_pM25=as.numeric(),
                         min_pM25=as.numeric(),
                         mean_pM10=as.numeric(),
                         max_pM10=as.numeric(),
                         min_pM10=as.numeric())

# make a loop to get the closest device reading to August 1 2020
for(i in 1:length(list_of_devices)){
  print(i)
  print(list_of_devices[i])
  temp_url <-paste0("GetReadings?devices=",list_of_devices[i],"&startDateTime=",
                    "2021-08-01","&endDateTime=","2021-08-30%2023:59:59")
  API_URL <- paste0(base, temp_url)
  API_URL <- gsub(" ", "%", API_URL)
  raw_data <- GET(API_URL,add_headers(ApiKey=api_key))
  temp_df <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
  mean_calibratedpm25 <- mean(temp_df$calibratedPM25,na.rm=T)
  max_calibratedpm25 <- max(temp_df$calibratedPM25,na.rm=T)
  min_calibratedpm25 <- min(temp_df$calibratedPM25,na.rm=T)
  
  mean_pM25 <- mean(temp_df$pM25,na.rm=T)
  max_pM25 <- max(temp_df$pM25,na.rm=T)
  min_pM25 <- min(temp_df$pM25,na.rm=T)
  
  mean_pM10 <- mean(temp_df$pM10,na.rm=T)
  max_pM10 <- max(temp_df$pM10,na.rm=T)
  min_pM10 <- min(temp_df$pM10,na.rm=T)
 temp_row<-data.frame(list_of_devices[i],mean_calibratedpm25,max_calibratedpm25,min_calibratedpm25,
      mean_pM25,max_pM25,min_pM25,mean_pM10,max_pM10,min_pM10)  
 names(temp_row)<- names(df_loop)
 df_loop<-bind_rows(df_loop,temp_row)
  
}

df_loop <- df_loop %>% filter(is.na(mean_calibratedpm25)==FALSE)


# combine with the recent data
API_URL <- paste0(base, latest_reading)
raw_data <- GET(API_URL,add_headers(ApiKey=api_key))
latest_df <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
latest_df <- latest_df %>% filter(!msrDeviceNbr %in% df_loop$msrDeviceNbr)

mean_calibratedpm25 <- mean(latest_df$calibratedPM25,na.rm=T)
max_calibratedpm25 <- max(latest_df$calibratedPM25,na.rm=T)
min_calibratedpm25 <- min(latest_df$calibratedPM25,na.rm=T)

mean_pM25 <- mean(latest_df$pM25,na.rm=T)
max_pM25 <- max(latest_df$pM25,na.rm=T)
min_pM25 <- min(latest_df$pM25,na.rm=T)

mean_pM10 <- mean(latest_df$pM10,na.rm=T)
max_pM10 <- max(latest_df$pM10,na.rm=T)
min_pM10 <- min(latest_df$pM10,na.rm=T)
temp_row<-data.frame(latest_df$msrDeviceNbr,mean_calibratedpm25,max_calibratedpm25,
                     min_calibratedpm25,mean_pM25,max_pM25,min_pM25,mean_pM10,
                     max_pM10,min_pM10)  
names(temp_row)<- names(df_loop)
df_loop<-bind_rows(df_loop,temp_row)


df_device_list <- df_device_list %>% select(
  msrDeviceNbr,deploymentStartDateTime,deploymentEndDateTime
  )
df <- left_join(df_device_list,df_loop)
df <- df %>% filter(is.na(mean_calibratedpm25)==FALSE)
df <- df %>% mutate(longitude = unlist(map(df$geometry,1)),
         latitude = unlist(map(df$geometry,2)))
df <- df %>% st_drop_geometry()
write.csv(df, "data/chicago_air_quality.csv",row.names = FALSE)
#raw_data <- GET("https://urban.microsoft.com/api/EclipseData/GetDeviceList?city=Chicago",add_headers(ApiKey=api_key))
#raw_data <- GET("https://urban.microsoft.com/api/EclipseData/GetLatestReadings?city=Chicago",add_headers(ApiKey=api_key))

# df <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
# df$latitude <- as.numeric(df$latitude)
# df$longitude <- as.numeric(df$longitude)
# df <- df %>% dplyr::filter(latitude>1 & latitude<8000)
# df <- df %>% dplyr::filter(longitude>-100)
# df<- st_as_sf(df,coords = c("longitude","latitude"),crs="+proj=longlat +datum=WGS84")
# df %>% st_geometry() %>% plot()


################################################################################
                      # Urban Heat Island Calcuation #
################################################################################
# pulls the names of the masked files for the loop below
raster_paths <- fs::dir_info("data/lst/mask/")%>%
  dplyr::filter(str_detect(path,".tif")) %>%
  dplyr::select(path) %>% sapply(., function(y) gsub("_01_T.*", "", y)) %>% unique() %>% as.list()

raster_names <- sprintf("timepoint%s",seq(1:4))

# need to reread in the masked data assign to an object in R, and rename layers 
# to be something meaningful
for(i in 1:4){
  temp_stack <- fs::dir_info("data/lst/mask/") %>%
    dplyr::filter(str_detect(path, unlist(raster_paths[i])) & str_detect(path,".tif")) %>%
    dplyr::filter(str_detect(path, "[B12345670]_mask.tif")) %>%
    dplyr::filter(!str_detect(path, "B11")) %>%
    dplyr::select(path)%>%
    pull()%>%
    as.character() %>%
    # Load our raster layers into a stack
    stack()
  
  # Name the Bands based on where they sample the electromagentic spectrum
  names(temp_stack) <- c("band_10",'ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')
  
  assign(str_c("chicago_rstack",raster_names[i]), temp_stack)
}

################################################################################
##### Calculate Temperature and Vegetation for each time point  #####
################################################################################
### Vegetation ###

raster_list <- c(chicago_rstacktimepoint1,chicago_rstacktimepoint2,
                 chicago_rstacktimepoint3,chicago_rstacktimepoint4)

# Live green vegetation can be represented with the NIR and Red Bands through
# the normalised difference vegetation index (NDVI) as chlorophyll reflects in the
# NIR wavelength, but absorbs in the Red wavelength.

# Short function to calculate NVDI
NDVIfun <- function(NIR, Red) {
  NDVI <- (NIR - Red) / (NIR + Red)
  return(NDVI)
}

# Calculate temperature from Landsat data
# this process is not currently in a function, so needs to be calculate by hand
# this data is stored in a file that stores sensor data that the satellite collects
# (separate from the images) - codebooks for these can be found on the landsat website

raster_paths <- dir_info("data/lst/mask")%>%
  dplyr::filter(str_detect(path,".tif")) %>%
  dplyr::select(path) %>% sapply(., function(y) gsub("_01_T.*", "", y)) %>% unique() %>% as.list()

raster_paths <- gsub("data/lst/mask/LC08_L1TP","",raster_paths)
# this function calculates lst from the landsat data
lst_calculate <- function(raster_stack= raster_stack ,mtl_path_stem=mtl_path_stem, band_10=band_10) {
  
  # calculate ndvi
  ndvi_temp <- raster_stack
  ndvi <- NDVIfun(ndvi_temp$NIR, ndvi_temp$red)
  
  MTL<-dir_info("data/lst/")%>%
    dplyr::filter(str_detect(path, mtl_path_stem)) %>%
    dplyr::filter(str_detect(path, "MTL.txt")) %>%
    dplyr::select(path)%>%
    pull() %>%
    readMeta() # a specific function for reading landsat MTL metadata files
  
  # #To see all the attributes
  # head(MTL)
  
  # need to use band 10 as this thermal layer is best correlated with temperature
  offsetandgain <-MTL %>%
    getMeta("B10_dn", metaData = ., what = "CALRAD") # Extract bandwise information from ImageMetaData
  
  # offsetandgain
  
  # Calculate the Top of Atmopshere (TOA) spectral radiance from the Digital Number (DN) 
  # Run the calculation using the 10 band raster layer
  # clean up band 10 -> remove excessively low values
  # mean_10 <- getValues(band_10) %>% mean(.,na.rm=T)
  # sd_10 <- getValues(band_10) %>% sd(.,na.rm=T)
  # low_bound <- mean_10-(sd_10*4)
  # band_10 <- reclassify(band_10, cbind(-Inf, low_bound, NA)) # get rid of excessively low values
  
  TOA <- offsetandgain$gain *
    band_10 + 
    offsetandgain$offset
  
  # Calculate Brightness temperature:
  # is the radiance travelling upward from the top of the atmosphere to the satellite 
  # in units of the temperature of an equivalent black body.
  # Instead of hardcoding these values we can extract them from our MTL
  # handbook for looking up the values
  # https://prd-wret.s3-us-west-2.amazonaws.com/assets/palladium/production/atoms/files/LSDS-1574_L8_Data_Users_Handbook_v4.0.pdf
  
  
  Calidata <- MTL$CALBT%>%
    terra::as.data.frame()%>%
    mutate(Band=rownames(.))%>%
    filter(Band=="B10_dn")
  
  # subset the columns
  K1 <- Calidata %>%
    dplyr::select(K1)%>%
    pull()
  
  K2 <- Calidata %>%
    dplyr::select(K2)%>%
    pull()
  
  Brighttemp <- (K2 / log((K1 / TOA) + 1))
  
  # Earlier we calculated NDVI, let's use that to determine emissivity of each pixel
  # First we need to calculate the fractional vegetation of each pixel
  # Fractional vegetation cover is the ratio of vertically projected area of 
  # vegetation to the total surface extent.
  facveg <- (ndvi-0.2/0.5-0.2)^2
  
  # Now compute the emissivity
  # Emissivity is the ratio absorbed radiation energy to total incoming radiation 
  # engery compared to a blackbody (which would absorb everything)
  emiss <- 0.004*facveg+0.986
  
  # Land Surface Temperature
  # Finally, we need to get our LST following the equation from Weng et al. 
  # 2004 (also summarised in Guja et al. (2018) and Avdan and Jovanovska (2016))
  Boltzmann <- 1.38*10e-23
  Plank <- 6.626*10e-34
  c <- 2.998*10e8
  
  p <- Plank*(c/Boltzmann)
  
  #define remaining varaibles
  lambda <- 1.09e-5
  #run the LST calculation
  LST <- Brighttemp/(1 +(lambda*Brighttemp/p)*log(emiss))
  # check the values
  LST
  
  # Need to convert to Celsius from Kelvin
  LST <- LST-273.15
  return(LST)
}


# iterate over my 4 timepoints to calculate land surface temperature
for(i in 1:4){
  my_stack <- raster_list[[i]]
  lst <- lst_calculate(raster_stack = my_stack, mtl_path_stem = unlist(raster_paths[i]), band_10 = my_stack$band_10)
  lst <- reclassify(lst, cbind(-Inf, 0, NA)) # get rid of low (impossible) values
  assign(str_c("lst",i,sep = "_"), lst)
}


chicago <- st_read("cluster_data/chicago_boundaries/blocks/geo_export_0a836b4b-86b6-416d-a2a3-4700d035bb01.shp")
chicago <- chicago %>% filter(!tractce10 %in% c("980000","770602"))
chicago <- st_transform(chicago, 26916) # convert to NAD83 / UTM zone 16N
chicago <- chicago %>% mutate(block_area=st_area(.))
chicago <- chicago %>% dplyr::filter(block_area>=units::set_units(100, m^2))

# extract tract means from each timepoint
tracts_lst_1 <- raster::extract(lst_1,chicago,fun=mean, na.rm=TRUE,df=TRUE)
tracts_lst_2 <- raster::extract(lst_2,chicago,fun=mean, na.rm=TRUE,df=TRUE)
tracts_lst_3 <- raster::extract(lst_3,chicago,fun=mean, na.rm=TRUE,df=TRUE)
tracts_lst_4 <- raster::extract(lst_4,chicago,fun=mean, na.rm=TRUE,df=TRUE)

# combine timepoints to get a 'summer' mean
lst_stack <- stack(c(lst_1,lst_2,lst_3))
# calculate average of the combined stack
lst_avg <- calc(lst_stack,fun=mean, na.rm=TRUE)
# extract averages to tracts
tracts_lst_avg <- raster::extract(lst_avg,chicago,fun=mean, na.rm=TRUE,df=TRUE)

# do the same thing but first normalize the temperatures
lst_1_scaled <- raster::scale(lst_1)
lst_2_scaled <- raster::scale(lst_2)
lst_3_scaled <- raster::scale(lst_3)
lst_4_scaled <- raster::scale(lst_4)

# extract scaled tract means from each timepoint
tracts_lst_1_scaled <- raster::extract(lst_1_scaled,chicago,fun=mean, na.rm=TRUE,df=TRUE)
tracts_lst_2_scaled <- raster::extract(lst_2_scaled,chicago,fun=mean, na.rm=TRUE,df=TRUE)
tracts_lst_3_scaled <- raster::extract(lst_3_scaled,chicago,fun=mean, na.rm=TRUE,df=TRUE)
tracts_lst_4_scaled <- raster::extract(lst_4_scaled,chicago,fun=mean, na.rm=TRUE,df=TRUE)


# combine to get a mean of the scaled values
lst_stack_scaled <- stack(c(lst_1_scaled,lst_2_scaled,lst_3_scaled)) # not includiing 4 here...
# calculate average of the combined scaled stack
lst_avg_scaled <- calc(lst_stack_scaled,fun=mean, na.rm=TRUE)
# extract averages scaled values to tracts
tracts_lst_avg_scaled <- raster::extract(lst_avg_scaled,chicago,fun=mean, na.rm=TRUE,df=TRUE)

chicago <- bind_cols(chicago,tracts_lst_1,tracts_lst_1_scaled,tracts_lst_2,tracts_lst_2_scaled,
                     tracts_lst_3,tracts_lst_3_scaled,tracts_lst_4,tracts_lst_4_scaled,
                     tracts_lst_avg,tracts_lst_avg_scaled)
names(chicago) <- c("blockce10","countyfp10","geoid10","name10","statefp10",
                    "tract_bloc","tractce10","block_area","drop_1","tracts_lst_1",
                    "drop_2","tracts_lst_1_scaled","drop_3","tracts_lst_2","drop_4",
                    "tracts_lst_2_scaled","drop_5","tracts_lst_3","drop_6",
                    "tracts_lst_3_scaled","drop_7","tracts_lst_4","drop_8",
                    "tracts_lst_4_scaled","drop_9","tracts_lst_avg","drop_10",
                    "tracts_lst_avg_scaled","geometry")
tmap::tm_shape(chicago) +
  tmap::tm_fill("tracts_lst_avg_scaled",style = "cont")

chicago <- chicago %>% st_drop_geometry() %>% select(-starts_with("drop"))

write.csv(chicago,"data/chicago_block_heat.csv",row.names = FALSE)

################################################################################

ndvi_1 <- NDVIfun(raster_list[[1]]$NIR, raster_list[[1]]$red)
ndvi_2 <- NDVIfun(raster_list[[2]]$NIR, raster_list[[2]]$red)
ndvi_3 <- NDVIfun(raster_list[[3]]$NIR, raster_list[[3]]$red)
ndvi_4 <- NDVIfun(raster_list[[4]]$NIR, raster_list[[4]]$red)

lst_stack <- stack(c(ndvi_1,ndvi_2,ndvi_3,ndvi_4))
# calculate average of the combined stack
lst_avg <- calc(lst_stack,fun=mean, na.rm=TRUE)

chicago <- st_read("cluster_data/chicago_boundaries/blocks/geo_export_0a836b4b-86b6-416d-a2a3-4700d035bb01.shp")
chicago <- chicago %>% filter(!tractce10 %in% c("980000","770602"))
chicago <- st_transform(chicago, 26916) # convert to NAD83 / UTM zone 16N
chicago <- chicago %>% mutate(block_area=st_area(.))
chicago <- chicago %>% dplyr::filter(block_area>=units::set_units(100, m^2))

# # extract averages to tracts
# blocks_ndvi_avg <- raster::extract(lst_avg,chicago,fun=mean, na.rm=TRUE,df=TRUE)

# get some more things to add to clusters
impervious_surface <-  lst_avg %>% raster::reclassify(.,c(-Inf, .1, 1,  .1, 1, 0))
high_veg <-  lst_avg %>% raster::reclassify(.,c(-Inf, .25, 0,  .25, 1, 1))
# 8/12 adding in plantable areas - will get a % that tree cover % can be subtracted from 
plantable_land <-  lst_avg %>% raster::reclassify(.,c(-Inf, 0,.1,  .1, 1, 1))

chicago_plantable_land <- raster::extract(plantable_land,chicago,fun=mean,na.rm=T,df=T)
# get by block
chicago_impervious <- raster::extract(impervious_surface,chicago,fun=mean,na.rm=T,df=T)
chicago_high_veg <- raster::extract(high_veg,chicago,fun=mean,na.rm=T,df=T)

chicago_plantable_land<- chicago_plantable_land %>% dplyr::select(plantable_land=layer)
chicago <- bind_cols(chicago,chicago_plantable_land)

chicago_impervious<- chicago_impervious %>% dplyr::select(impervious_surface=layer)
chicago_high_veg<- chicago_high_veg %>% dplyr::select(high_veg=layer)


chicago <- bind_cols(chicago,chicago_impervious)
chicago <- bind_cols(chicago,chicago_high_veg)
chicago <- chicago %>% st_drop_geometry() %>% dplyr::select(geoid10,impervious_surface,high_veg,plantable_land)

write.csv(chicago,"data/block_ndvi_metrics.csv",row.names = F)

################################################################################

chicago <- st_read("cluster_data/chicago_boundaries/blocks/geo_export_0a836b4b-86b6-416d-a2a3-4700d035bb01.shp")
chicago <- chicago %>% filter(!tractce10 %in% c("980000","770602"))
chicago <- st_transform(chicago, 26916) # convert to NAD83 / UTM zone 16N
chicago <- chicago %>% mutate(block_area=st_area(.))
chicago <- chicago %>% dplyr::filter(block_area>=units::set_units(100, m^2))


# connect to equity data
#df1 <- read_csv("data/ACSDT5Y2019.B02001_2022-08-11T100930/ACSDT5Y2019.B02001_data_with_overlays_2022-06-14T125056.csv",skip = 1)
#df2 <- read_csv("data/ACSDT5Y2019.B03003_2022-08-11T101122/ACSDT5Y2019.B03003-Data.csv",skip=1)
df3 <- read_csv("data/ACSDT5Y2019.B17010_2022-08-11T103618/ACSDT5Y2019.B17010-Data.csv",skip=1)
df4 <- read_csv("data/ACSDT5Y2019.B03002_2022-08-12T065151/ACSDT5Y2019.B03002_data_with_overlays_2022-08-12T065151.csv",skip=1)

# starting with block groups, assign values to block level for race and poverty
# df1 <- df1 %>% select(block_group_id=id,total=`Estimate!!Total:`,
#                       white_alone=`Estimate!!Total:!!White alone`,
#                       black_alone=`Estimate!!Total:!!Black or African American alone`,
#                       asian_alone=`Estimate!!Total:!!Asian alone`)
# df1 <- df1 %>% mutate(
#   pct_white_alone=white_alone/total,
#   pct_black_alone=black_alone/total,
#   pct_asian_alone=asian_alone/total,
#   pct_other_race=1-pct_white_alone-pct_black_alone-pct_asian_alone
# )
# 
# df2 <- df2 %>% select(block_group_id=Geography,
#                       total=`Estimate!!Total:`,
#                       hispanic_total=`Estimate!!Total:!!Hispanic or Latino`)
# 
# df2 <- df2 %>% mutate(
#   pct_hispanic=hispanic_total/total
# )

df3 <- df3 %>% select(block_group_id=Geography,
                      total=`Estimate!!Total:`,
                      income_below_pov=`Estimate!!Total:!!Income in the past 12 months below poverty level:`)
df3 <- df3 %>% mutate(
  pct_below_pov=income_below_pov/total
)

df4 <- df4 %>% select(block_group_id=id,
                      total=`Estimate!!Total:`,
                      latino=`Estimate!!Total:!!Hispanic or Latino:`,
                      white_alone=`Estimate!!Total:!!Not Hispanic or Latino:!!White alone`,
                      black_alone=`Estimate!!Total:!!Not Hispanic or Latino:!!Black or African American alone`,
                      asian_alone= `Estimate!!Total:!!Not Hispanic or Latino:!!Asian alone`
                      )

df4 <- df4 %>% mutate(
  pct_white_alone=white_alone/total,
  pct_black_alone=black_alone/total,
  pct_asian_alone=asian_alone/total,
  pct_latino=latino/total,
  pct_other_race=1-pct_white_alone-pct_black_alone-pct_asian_alone-pct_latino
) %>% select(block_group_id,starts_with("pct"))
df_final <- left_join(df4,df3,by="block_group_id")
df_final <- df_final %>% select(-total,-income_below_pov)

df_final$block_group_id <- gsub("1500000US",'',df_final$block_group_id)
chicago <- chicago %>% mutate(
  block_group_id = paste0(statefp10,countyfp10,tract_bloc, sep="")
)

chicago$block_group_id <- str_sub(chicago$block_group_id, end=-4)
#df_final <- left_join(df_final,df3,by="block_group_id")

chicago <- left_join(chicago,df_final,by="block_group_id")
chicago <- chicago %>% st_drop_geometry() %>% select(geoid10,block_group_id,starts_with("pct"))

write.csv(chicago, "chicago_block_demographics.csv",row.names = F)
