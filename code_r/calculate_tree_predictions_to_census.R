setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library(raster)
library(tidyverse)
# x tidyr::extract() masks raster::extract()
# x dplyr::filter()  masks stats::filter()
# x dplyr::lag()     masks stats::lag()
# x dplyr::select()  masks raster::select()
library(sf)

# # raster files
# rast_list <- list.files("D:/final_data/2019_predictions_results/") %>% Filter(function(x) {str_detect(x,"tif")}, .)
# rast_list_path <- unlist(lapply(rast_list, function(i){paste0("D:/final_data/2019_predictions_results/",i)}))
# 
# all_2019 <- raster::stack(paste0("D:/final_data/all_2019.tif"))
# names(all_2019)<- c("tree_binary","tree_height","r","g","b")
# chicago_outline <- sf::st_read("../../term one/Principles of Spatial Analysis/data/raw/assessment/chicago_boundaries/geo_export_1889da91-0253-48fe-bbfd-92c0593875b4.shp")
# 
# 
# 
# test <- raster::stack(paste0("D:/final_data/2019_predictions_results/",rast_list[1252]))
# names(test)<- c("tree_binary","tree_height","r","g","b")
# test$tree_binary[test$tree_binary[] < .5 ] = NA # check the use of braces to acces values of the raster
# raster::plotRGB(test,3,4,5)
# raster::plot(test$tree_binary,add=T,legend=F)
# raster::plot(test$tree_height)
# 
# #A "VRT" is a virtual raster. It can treat different files (typically adjacent non-overlapping tiles, but that is not required) as a single data source (file). With that, you could then do
# 
# 
# #12:40
# # v <- terra::vrt(rast_list_path) 
# terra::writeRaster(all_2019$tree_binary, "D:/final_data/all_2019_tree_binary.tif",overwrite=T)
# 
# 
# tree_canopy_height<- raster::mask(all_2019$tree_height, tree_binary,maskvalue=0,updatevalue=0)
# terra::writeRaster(all_2019$tree_binary, "D:/final_data/all_2019_tree_binary.tif",overwrite=T)

chicago <- st_read("C:/Users/johnf/Box/UCL/thesis/cluster_data/chicago_boundaries/blocks/geo_export_0a836b4b-86b6-416d-a2a3-4700d035bb01.shp")
chicago <- chicago %>% filter(!tractce10 %in% c("980000","770602"))
chicago <- st_transform(chicago, 26916) # convert to NAD83 / UTM zone 16N
chicago <- chicago %>% mutate(block_area=st_area(.))
chicago <- chicago %>% dplyr::filter(block_area>=units::set_units(100, m^2))

tree_binary <- raster::stack("D:/final_data/all_2019_tree_binary.tif")
#tree_binary <- terra::rast("D:/final_data/all_2019_tree_binary.tif")

chicago <- st_transform(chicago, st_crs(tree_binary)) # convert to NAD83 / UTM zone 16N
#chicago <- terra::vect(chicago)



# JUST DID IN QGIS
# start_time <- Sys.time()
# tree_binary_pct <- raster::extract(tree_binary,chicago,fun=mean,na.rm=T,df=T)
# #tree_binary_pct <- terra::extract(tree_binary,chicago,fun=mean,na.rm=T,df=T)
# chicago <- bind_cols(chicago,tree_binary_pct)
# end_time <- Sys.time()
# loop_time <- end_time - start_time
# 
# start_time2 <- Sys.time()
# tree_canopy <- terra::rast("D:/final_data/all_2019_tree_canopy_height.tif")
# tree_canopy_pct <- terra::extract(tree_canopy,chicago,fun=mean,na.rm=T,df=T)
# chicago <- bind_cols(chicago,tree_binary_pct)
# end_time2 <- Sys.time()
# loop_time2 <- end_time2 - start_time2


###############################################################################
# Any reasonable data that can be pulled from AoT?
micro_aq <- read.csv("C:/Users/johnf/Box/UCL/thesis/data/chicago_air_quality.csv")

nodes <- read_csv("../data/aot_data/AoT_2022-03-23/nodes.csv")
df <- read_csv("D:/aot/summary_file.csv")

df$timestamp <- lubridate::ymd_hms(df$timestamp)

df <- df %>% filter(timestamp >= "2019-04-01" & timestamp <="2020-10-01")

df2 <- df %>% filter(timestamp >= "2020-06-01" & timestamp <="2020-10-01") 
df2 <- df2 %>% filter(is.na(value)==FALSE)
df3 <- df %>% filter(timestamp >= "2019-06-01" & timestamp <="2019-10-01")
df3 <- df3 %>% filter(is.na(value)==FALSE)


df2_pm <- df2 %>% filter(timestamp >= "2020-08-01" & timestamp <="2020-08-07")
df2_pm <- df2_pm %>% filter(parameter %in% (c(
  "pm25_atm","pm10_atm")))
df2_pm <- df2_pm %>% select(node_id,parameter,value) %>% group_by(node_id,parameter) %>% 
  summarise(mean_val=mean(value,na.rm=T),
            max_val=max(value,na.rm=T),
            min_val=min(value,na.rm=T)
            )
df2_pm<- df2_pm %>% filter(!node_id %in% "001e06113107")
df2_pm <- df2_pm %>% filter(!node_id %in% df3_pm$node_id)


df3_pm <- df3 %>% filter(timestamp >= "2019-08-01" & timestamp <="2019-08-07")
df3_pm <- df3_pm %>% filter(parameter %in% (c(
  "pm25_atm","pm10_atm")))
df3_pm <- df3_pm %>% select(node_id,parameter,value) %>% group_by(node_id,parameter) %>% 
  summarise(mean_val=mean(value,na.rm=T),
            max_val=max(value,na.rm=T),
            min_val=min(value,na.rm=T)
  )
aot_aq <- bind_rows(df3_pm,df2_pm)
rm(df2,df3,df3_pm,df2_pm)
nodes <- nodes %>% select(node_id,latitude=lat,longitude=lon)
aot_aq <- left_join(aot_aq,nodes,by="node_id")

aot_aq_wide <- aot_aq %>% pivot_wider(
  id_cols = c("node_id","latitude","longitude"),
  names_from = parameter,
  values_from = c("mean_val","max_val","min_val" )
)

aot_aq_wide <- aot_aq_wide %>% select(
  msrDeviceNbr=node_id,
  mean_pM10=mean_val_pm10_atm,
  mean_pM25=mean_val_pm25_atm,
  max_pM10=max_val_pm10_atm,
  max_pM25=max_val_pm25_atm,
  min_pM10=min_val_pm10_atm,
  min_pM25=min_val_pm25_atm,
  latitude,longitude 
  )
micro_aq$msrDeviceNbr <- as.character(micro_aq$msrDeviceNbr)
aq_final <- bind_rows(micro_aq,aot_aq_wide)

aq_final<- st_as_sf(aq_final,coords = c("longitude","latitude"),crs="+proj=longlat +datum=WGS84")
aq_final %>% st_geometry() %>% plot()

#write.csv(aq_final,"../data/all_airquality.csv",row.names = FALSE)
aq_final <- st_transform(aq_final, 26916) # convert to NAD83 / UTM zone 16N

library("gstat")
library("geoR")
datafile_sp_prj <- as(aq_final, "Spatial")
# use variogram() function to compute the semivariance with a null model Mean_SO2 as outcome
# SO2_emp.variogram <- variogram(mean_pM10~1, datafile_sp_prj)
# # Compute the object to reveal a table
# SO2_emp.variogram
# plot(SO2_emp.variogram)
# 
# exp_SO2_emp.variogram <- fit.variogram(SO2_emp.variogram, model = vgm(0, "Exp", 380, 602))
# exp_SO2_emp.variogram
# plot(SO2_emp.variogram, exp_SO2_emp.variogram)

library(gstat) # Use gstat's idw routine
library(sp) 
chicago_boundary <- st_read("../../term one/Principles of Spatial Analysis/data/raw/assessment/chicago_boundaries/geo_export_1889da91-0253-48fe-bbfd-92c0593875b4.shp")
# Change  projection
chicago_boundary <- st_transform(chicago_boundary,26916) #NAD83 UTM Zone 16N (EPSG 26916)

chicago_raster <- raster(chicago_boundary)
res(chicago_raster) <- 100
# chicago_raster <- as(chicago_raster, 'SpatialGridDataFrame')
# grd <- as.data.frame(spsample(aq_final_sp,"regular",n=50000))
# names(grd)       <- c("X", "Y")
# coordinates(grd) <- c("X", "Y")
# gridded(grd)     <- TRUE  # Create SpatialPixel object
# fullgrid(grd)    <- TRUE  # Create SpatialGrid object
# proj4string(grd) <- proj4string(aq_final_sp)
# 
# aq_final_sp <- as(aq_final, "Spatial")
# aq_idw <- gstat::idw(mean_pM25~1,aq_final_sp,newdata=grd, idp=3)
# # aq_predicted_vals <- raster(aq_idw)
# plot(aq_predicted_vals)
aq_final_2 <- aq_final %>% filter(mean_pM25<150)
# USING 151 points in the end
fitmax <- gstat::gstat(formula = mean_pM10 ~ 1, data = aq_final_2, nmax = 2)
maxint <- raster::interpolate(chicago_raster, model=fitmax,ext=extent(chicago_boundary))
plot(maxint)
plot(chicago_boundary %>% st_geometry(),add=T)

maxint <- mask(maxint,chicago)
maxint <- raster::scale(maxint)
maxint <- raster::extract(maxint,chicago,fun=mean,na.rm=T,df=T)
chicago <- bind_cols(chicago,maxint)


chicago <- chicago %>% rename(mean_pm10=var1.pred)
tmap::tm_shape(chicago) +
  tmap::tm_fill("mean_pm10",style = "cont")

chicago <- chicago %>% select(geoid10,mean_pm25,mean_pm10) %>% st_drop_geometry()

write.csv(chicago,"../data/block_airquality.csv",row.names = F)
# df3 <- df %>% filter(sensor %in% "o3") %>% filter(is.na(value)==FALSE)
# unique(df3$node_id) # 86
# df3 <- df %>% filter(sensor %in% "so2") %>% filter(is.na(value)==FALSE)
# unique(df3$node_id) # 86
# df3 <- df %>% filter(sensor %in% "tmp421") %>% filter(is.na(value)==FALSE)
# unique(df3$node_id) # 118
# df3 <- df %>% filter(parameter %in% "pm25_cf1") %>% filter(is.na(value)==FALSE)
# unique(df3$node_id) # 23


                              