setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library(raster)
library(gstat)
library(geoR)
library(tidyverse)
library(sf)
library(tmap)
library(tidyLPA)

# Cluster Profiles Data cleaning

# at the census tract/block level create the following set of variables
# -	Street Network Data
# -	Zoning District (2016/current)
# -	Land Use Data (2015- https://www.cmap.illinois.gov/data/land-use/inventory)
# -	Traffic Data? (Average daily traffic counts)
# ???	http://apps.dot.illinois.gov/gist2/ should give average daily counts
# -	Population Density? (can calculate from census)
# -	GreenSpace (parks data, % of area that is greenspace)
# -	Vacant lots (% of areas that is vacant lots)
# -	Overground Train?
#   -	Highway?
#   -	% of area covered by water (lake/river?)
# -	Average Temperature
# -	Wind data? - probs not available by census tract/block
# -	https://www.cmap.illinois.gov/programs/water/stormwater/flood-index CMAP flood index?
#   -	https://docs.google.com/document/d/1ukkNO3ZJKxfKgVLOuj8XCJPzwH-4hIHouSC66pbk-fc/edit tree crown density using other measures


# For each of the measures i need to get a single value at the level of geography that
# i want to run my later models at, most likely census tract (as opposed to blocK)


# read in data
chicago <- st_read("cluster_data/chicago_boundaries/blocks/geo_export_0a836b4b-86b6-416d-a2a3-4700d035bb01.shp")
chicago <- chicago %>% filter(!tractce10 %in% c("980000","770602"))
chicago <- st_transform(chicago, 26916) # convert to NAD83 / UTM zone 16N
chicago <- chicago %>% mutate(block_area=st_area(.))
chicago <- chicago %>% dplyr::filter(block_area>=units::set_units(100, m^2))

chicago <- chicago %>% mutate(
  block_group_id = paste0(statefp10,countyfp10,tract_bloc, sep="")
)

chicago$block_group_id <- str_sub(chicago$block_group_id, end=-2)
#chicago <- chicago %>% distinct(block_group_id, .keep_all = T)

chicago <- chicago %>% group_by(block_group_id) %>%  summarise(do_union=T)
chicago <- chicago %>% mutate(block_area=st_area(.))

# calculate percentent of each tract in water
water <- st_read("cluster_data/Waterways_Chicago/geo_export_110c65a5-f82f-43cd-9b41-9ae24632d43d.shp")
water <- water %>% filter(!name %in% "LAKE MICHIGAN")
#water %>% st_geometry() %>% plot()
water <- st_transform(water, 26916) # convert to NAD83 / UTM zone 16N

# pct of water is pretty low, might need to change to distance to lake michigan
# or contains water feature y/n
water_intersect <- st_intersection(chicago, water) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  st_drop_geometry()  %>% # drop geometry as we don't need it
  mutate(water_pct=intersect_area/block_area) %>% 
  select(block_group_id,water_pct) %>% group_by(block_group_id) %>% summarise(
    has_water_feature=sum(water_pct,na.rm=T)
  ) %>% mutate(has_water_feature = 1)

chicago <- left_join(chicago,water_intersect,by="block_group_id")
chicago$has_water_feature <- ifelse(is.na(chicago$has_water_feature)==TRUE,0,1)
rm(water,water_intersect)
  

# calculate percentent of each tract in parks - this over NDVI greenspace?
parks <- st_read("cluster_data/Parks - Chicago Park District Park Boundaries (current)/geo_export_d12ed06e-43e5-4ff3-b026-bdf3a327564e.shp")
#parks %>% st_geometry() %>% plot()
parks <- st_transform(parks, 26916) # convert to NAD83 / UTM zone 16N

# pct of parks is pretty low, might need to change to distance to greensace with NVDI
parks_intersect_pct <- st_intersection(chicago, parks) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  st_drop_geometry()  %>% # drop geometry as we don't need it
  mutate(parks_pct=intersect_area/block_area) %>% 
  select(block_group_id,parks_pct) %>% group_by(block_group_id) %>% summarise(
    has_parks=sum(parks_pct,na.rm=T)
  )

chicago <- left_join(chicago,parks_intersect_pct,by="block_group_id")
chicago$has_parks <- ifelse(is.na(chicago$has_parks)==TRUE,0,1)
rm(parks,parks_intersect_pct)

# roads
roads <- st_read("cluster_data/il_dot_roads_2017/HWY2017.shp")
#roads <- st_read("cluster_data/il_dot_roads_2017/RRX2017.shp") # railway
roads <- roads %>% dplyr::filter(COUNTY_NAM %in% "COOK")
#roads %>% st_geometry() %>% plot()
roads <- roads %>% dplyr::filter(is.na(AADT)==FALSE)

roads <- st_transform(roads, 26916) # convert to NAD83 / UTM zone 16N

traffic_counts_raster <- raster(chicago)
res(traffic_counts_raster) <- 250
raster_traffic <- raster::rasterize(roads, traffic_counts_raster, "AADT",fun=function(x,...)mean(x,na.rm=T),background=7500)

raster_traffic2 <- mask(raster_traffic,chicago)
raster_traffic2 <- raster::scale(raster_traffic)
chicago_traffic <- raster::extract(raster_traffic,chicago,fun=mean,na.rm=T,df=T)
chicago <- bind_cols(chicago,chicago_traffic)
tm_shape(chicago) +
  tm_fill("layer",style = "cont")
chicago <- chicago %>% dplyr::select(-"ID...6")
chicago <- chicago %>% rename(average_traffic=layer)
rm(r, raster_traffic,chicago_traffic,traffic_counts_raster,v,x,roads)


# transit
transit_metric <- st_read("cluster_data/TransitAvailability2017/TransitAvailability_2017_CMAP_201706.shp")
transit_metric <- transit_metric %>% dplyr::select(ta_ind_17,pef_ind_17)
# Pedestrian Environment Factor
# transit accessibility index
# pct of parks is pretty low, might need to change to distance to greensace with NVDI
transit_metric <- st_transform(transit_metric, 26916) # convert to NAD83 / UTM zone 16N
transit_join <- st_join(chicago, transit_metric) %>% 
  dplyr::select(block_group_id,ta_ind_17,pef_ind_17) %>% group_by(block_group_id) %>% summarise(
    ta_ind_17=mean(ta_ind_17,na.rm=T),
    pef_ind_17=mean(pef_ind_17,na.rm=T)
  )
transit_join <- transit_join %>% st_drop_geometry()
chicago <- left_join(chicago,transit_join,by="block_group_id")
rm(transit_join,roads,transit_metric)

# # walkability aggregate score to tracts
# walkability <- st_read("cluster_data/walkability/Walkability.shp")
# walkability %>% st_geometry() %>% plot()
# #TotalScore

# integrate zoning district, % of each district?
# see secondcityzoning.org/zones/
zone_districts <- st_read("cluster_data/zoning_district/geo_export_95466e7f-7b95-4f6e-bf96-febc365f26fe.shp")
zone_districts <- st_transform(zone_districts, 26916) 
# maybe just pick a few of the more relevant categories, there are 12 categories...
zone_districts$zone_class <- str_replace_all(zone_districts$zone_class,"[:digit:]","")
zone_districts <- zone_districts %>% mutate(
  business_zone = ifelse(str_detect(zone_class,"B"),1,0),
  commercial_zone = ifelse(str_detect(zone_class,"^C"),1,0),
  downtown_zone = ifelse(str_detect(zone_class,"^D"),1,0),
  manufacturing_zone = ifelse(str_detect(zone_class,"^M"),1,0),
  parks_zone = ifelse(str_detect(zone_class,"POS"),1,0),
  residential_zone = ifelse(str_detect(zone_class,"^R"),1,0),
  planned_development_zone = ifelse(str_detect(zone_class,"^PD"),1,0)
) %>% dplyr::select(contains("_zon"))

zone_intersect_pct <- st_intersection(chicago, zone_districts) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  st_drop_geometry()  %>% # drop geometry as we don't need it
  mutate(zone_pct=as.numeric(intersect_area/block_area),
         pivot_name= case_when(
           business_zone==1~"business",
           commercial_zone==1~"commercial",
           downtown_zone==1~"downtown",
           manufacturing_zone==1~"manufacturing",
           parks_zone==1~"parks",
           residential_zone==1~"residential",        
          planned_development_zone==1~"planned_development"
         )) %>% 
  dplyr::select(block_group_id,pivot_name,zone_pct) %>% pivot_wider(
    id_cols = block_group_id,
    names_from = pivot_name,
    names_prefix="pct_",
    values_from = zone_pct,
    values_fn = sum,
    values_fill = 0
  ) %>% dplyr::select(-"pct_NA")


chicago <- left_join(chicago,zone_intersect_pct,by="block_group_id")
rm(zone_districts,zone_intersect_pct)

# % of some of the land use types?
land_use <- st_read("cluster_data/land_use_2015/Landuse2015_CMAP_v1.shp")
#land_use %>% st_geometry() %>% plot()
land_use <- land_use %>% dplyr::filter(!LANDUSE %in% c("5000","6000","9999"))
land_use <- st_transform(land_use, 26916) 
# try and condense the large # of cats
land_use <- land_use %>% mutate(
  resident_detached_area = ifelse(str_detect(LANDUSE,"1111"),1,0),
  resident_attached_area = ifelse(str_detect(LANDUSE,"1112"),1,0),
  resident_multifam_area = ifelse(str_detect(LANDUSE,"1130"),1,0),
  open_space_area = ifelse(str_detect(LANDUSE,"1151") | str_detect(LANDUSE,"^3"),1,0),
  commercial_area = ifelse(str_detect(LANDUSE,"^12"),1,0),
  institutional_area = ifelse(str_detect(LANDUSE,"^13"),1,0),
  industrial_area = ifelse(str_detect(LANDUSE,"^14"),1,0),
  agriculture_area = ifelse(str_detect(LANDUSE,"2000"),1,0),
  vacant_area = ifelse(str_detect(LANDUSE,"^4"),1,0),
  
) %>% dplyr::select(contains("_area"))

land_use_intersect_pct <- st_intersection(chicago, land_use) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  st_drop_geometry()  %>% # drop geometry as we don't need it
  mutate(land_use_pct=as.numeric(intersect_area/block_area),
         pivot_name= case_when(
           resident_detached_area==1~"resident_detached_area",
           resident_attached_area==1~"resident_attached_area",
           resident_multifam_area==1~"resident_multifam_area",
           open_space_area==1~"open_space_area",
           commercial_area==1~"commercial_area",
           institutional_area==1~"institutional_area",        
           industrial_area==1~"industrial_area",
           agriculture_area==1~"agriculture_area",
           vacant_area==1~"vacant_area",
         )) %>% 
  dplyr::select(block_group_id,pivot_name,land_use_pct) %>% pivot_wider(
    id_cols = block_group_id,
    names_from = pivot_name,
    names_prefix="pct_",
    values_from = land_use_pct,
    values_fn = sum,
    values_fill =0
  ) %>% dplyr::select(-"pct_NA")

chicago <- left_join(chicago,land_use_intersect_pct,by="block_group_id")
rm(land_use,land_use_intersect_pct)

pop_df <- read_csv("cluster_data/Population_by_2010_Census_Block.csv")
pop_df <- pop_df %>% dplyr::select(block_group_id="CENSUS BLOCK FULL",total_pop="TOTAL POPULATION")
pop_df$block_group_id <- str_sub(pop_df$block_group_id, end=-2)
#pop_df <- pop_df %>% distinct(block_group_id,.keep_all = T)
pop_df <- pop_df %>% group_by(block_group_id) %>% summarise(total_pop=sum(total_pop,na.rm=F))


chicago <- left_join(chicago,pop_df,by="block_group_id")
chicago <- chicago %>% dplyr::filter(is.na(total_pop)==FALSE)
chicago <- chicago %>% mutate(
  pop_density = total_pop/st_area(.)
)

write.csv(chicago %>% st_drop_geometry(),"cluster_data/chicago_lpa_data2.csv",row.names = F)

# add in % impervious and % nvdi
sent_10m <- raster::stack("cluster_data/sentinel/2017/sent_2_2017_10m.tif")
names(sent_10m) <- c("blue","green","red","NIR")
NDVIfun <- function(NIR,RED){
  NDVI <- (NIR - RED)/ (NIR + RED)
  return(NDVI)
}



ndvi_chicago <- NDVIfun(sent_10m$NIR,sent_10m$red)
ndvi_chicago <- raster::projectRaster(ndvi_chicago,crs=st_crs(chicago)$proj4string)
# get some more things to add to clusters
impervious_surface <-  ndvi_chicago %>% raster::reclassify(.,c(-Inf, 0, 1,  0, 1, 0))
chicago_impervious <- raster::extract(impervious_surface,chicago,fun=mean,na.rm=T,df=T)
chicago_impervious<- chicago_impervious %>% dplyr::select(impervious_surface=layer)
chicago <- bind_cols(chicago,chicago_impervious)
rm(chicago_impervious,sent_10m)

high_veg <-  ndvi_chicago %>% raster::reclassify(.,c(-Inf, .25, 0,  .25, 1, 1))
chicago_high_veg <- raster::extract(high_veg,chicago,fun=mean,na.rm=T,df=T)
chicago_high_veg<- chicago_high_veg %>% dplyr::select(high_veg=layer)
chicago <- bind_cols(chicago,chicago_high_veg)
rm(chicago_high_veg)

# 8/10/22 add in railroads
railroads <- st_read("cluster_data/Illinois_Railroads/Illinois_Railroads.shp")

railroads %>% st_geometry() %>% plot()
railroads <- st_transform(railroads, 26916) # convert to NAD83 / UTM zone 16N

# pct of water is pretty low, might need to change to distance to lake michigan
# or contains water feature y/n
railroads_intersect <- st_intersection(chicago, railroads) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  st_drop_geometry()  %>% # drop geometry as we don't need it
  mutate(railroads_pct=intersect_area/block_area) %>% 
  dplyr::select(block_group_id,railroads_pct) %>% group_by(block_group_id) %>% summarise(
    has_railroads=sum(railroads_pct,na.rm=T)
  ) %>% mutate(has_railroads = 1)

chicago <- left_join(chicago,railroads_intersect,by="block_group_id")
chicago$has_railroads <- ifelse(is.na(chicago$has_railroads)==TRUE,0,1)
rm(railroads,railroads_intersect)


lpa_data <- read_csv("cluster_data/chicago_lpa_data2csv")
lpa_data$block_group_id <- as.character(lpa_data$block_group_id)
chicago <- chicago %>% st_drop_geometry() 
chicago <- chicago %>% distinct(block_group_id,.keep_all = T)
lpa_data <- left_join(lpa_data,chicago,by="block_group_id")

write.csv(chicago,"cluster_data/chicago_lpa_data2csv",row.names = F)

##################################################################################
# Perform an LPA
library(tidyverse)
library(tidyLPA)
library(mclust)
lpa_data <- read_csv("cluster_data/chicago_lpa_data.csv")
lpa_data <- lpa_data %>% distinct(geoid10,.keep_all = T) %>% select(-high_veg,-impervious_surface)
block_metrics <- read_csv("data/block_ndvi_metrics.csv")
block_metrics <- block_metrics %>% distinct(geoid10,.keep_all = T)
lpa_data <-left_join(lpa_data,block_metrics,by="geoid10")

lpa_data <- lpa_data %>% select(-geoid10,-plantable_land) %>% scale() %>% as.data.frame()
# remove pct_agriculture_area,pct_planned_development
lpa_data <- lpa_data %>% select(-pct_agriculture_area,-pct_planned_development,-pct_parks, -pct_industrial_area,-pct_open_space_area,-pct_commercial)
lpa_data <- lpa_data %>% filter(is.na(pct_commercial_area)==FALSE)


# Run many different versions, MPLUS is better but each of these generates slightly 
# different clusters
c2_12a <- lpa_data %>% 
  estimate_profiles(2:12)

c2_12b <- lpa_data %>% 
  estimate_profiles(2:12)

c2_12c <- lpa_data %>% 
  estimate_profiles(2:12)

c2_12d <- lpa_data %>% 
  estimate_profiles(2:12)

c2_12e <- lpa_data %>% 
  estimate_profiles(2:12)

c2_12f <- lpa_data %>% 
  estimate_profiles(2:12)

c2_12g <- lpa_data %>% 
  estimate_profiles(2:12)

lpa_data  %>%
  estimate_profiles(3:13,
                    variances = c("equal","equal"),
                    covariances = c("zero","equal")) %>%
  compare_solutions(statistics = c("AIC", "BIC","Entropy","n_min"))

fit_stats <- get_fit(c2_12) %>% 
  select(Model,Classes,AIC,BIC,Entropy,n_min) 



get_fit(c2_12) %>% 
  select(Model,Classes,Entropy,prob_min,prob_max) %>% 
  gather("metric","score",-c(Model,Classes)) %>% 
  ggplot(aes(x=Classes,y=score,color=metric)) +
  geom_point() +
  geom_line() +
  theme_minimal(base_size=14)+
  scale_y_continuous(limits=c(.5,1))+
  labs(title="Evaluating # of Profiles",
       x="# of Profiles",
       y="Value",
       color="Metric",
       caption="Note: The goal is to maximize each of these metrics")

get_fit(c2_12b) %>% 
  select(Model,Classes,AIC,BIC) %>% 
  gather("metric","score",-c(Model,Classes)) %>% 
  ggplot(aes(x=Classes,y=score,color=metric)) +
  geom_point() +
  geom_line() +
  theme_minimal(base_size=14)+
  labs(title="Evaluating # of Profiles",
       x="# of Profiles",
       y="Value",
       color="Metric")

get_fit(c2_12b) %>% 
  select(Model,Classes,n_min,n_max) %>% 
  gather("metric","score",-c(Model,Classes)) %>% 
  ggplot(aes(x=Classes,y=score,color=metric)) +
  geom_point() +
  geom_line() +
  theme_minimal(base_size=14)+
  labs(title="Evaluating # of Profiles",
       x="# of Profiles",
       y="Value",
       color="Metric",
       caption="Note: n_max is proportion of sample in largest profile;
       n_min is proportion of sample in smallest profile")

get_data(c2_12a$model_1_class_6) %>% {prop.table(table(.$Class))}
get_data(c2_12$model_1_class_7) %>% {prop.table(table(.$Class))}
get_data(c2_12$model_1_class_8) %>% {prop.table(table(.$Class))}
get_data(c2_12$model_1_class_9) %>% {prop.table(table(.$Class))}

get_data(c2_12$model_1_class_6) %>% select(Class) %>% table()
get_data(c2_12$model_1_class_7) %>% select(Class) %>% table()
get_data(c2_12$model_1_class_8) %>% select(Class) %>% table()
get_data(c2_12$model_1_class_9) %>% select(Class) %>% table()


list_of_datasets <- list("clust_8f" = get_data(c2_12f$model_1_class_8),
                         "clust_8g" = get_data(c2_12g$model_1_class_8),
                         "clust_9g" = get_data(c2_12g$model_1_class_9)
)
 openxlsx::write.xlsx(list_of_datasets,
                      file="lpa_assignments_8-12-22.xlsx")

################################################################################
# Visually Inspect the different models to compare results
library(ggplot2)
clust_7a <- readxl::read_xlsx("lpa_assignments_8-12-22.xlsx",sheet="clust_8f")
clust_8g <- readxl::read_xlsx("lpa_assignments_8-12-22.xlsx",sheet="clust_8g")
clust_9c <- readxl::read_xlsx("lpa_assignments_8-12-22.xlsx",sheet="clust_9g")

clust_8g <- clust_8g %>% filter(is.na(Class)==FALSE)

dfs <- list(clust_7a,clust_8g,clust_9c)

lapply(dfs, function(x) prop.table(table(x["Class"])))

for(i in 1:length(dfs)){
  print(prop.table(table(dfs[[i]]["Class"])))
}

clust_10 %>% dplyr::select(-model_number,-classes_number,-starts_with("CPROB")) %>% group_by(
  Class) %>% summarise_all(mean,na.rm=T) %>% 
  pivot_longer(
    -names(.)[1],
    names_to="lpa_features",
    values_to="value"
  ) %>% ggplot(aes(y=value,x=lpa_features,fill=factor(Class,labels = c(
    "1 (19.7%)","2 (47.0%)","3 (1.5%)","4 (16.2%)","5 (7.2%)","6 (.%)",'7','8','9','10'
  ))))+
  # scale_x_discrete(breaks= c("urbanicity_classification","num_ks","num_heads","num_dist","num_prog",
  #                            "num_ks_title1"),
  #                  labels=c("Urbanicity","# Ks","# HS","# Dist","# Programs",
  #                           "# Title 1 Ks")) +
  geom_bar(stat="identity",width=.6,position=position_dodge(width=.7)) +
  labs(x="Features",y="Standardized Z value",title="LPA Cluster Averages", 
       fill="Clusters") +
  theme_bw() +
  theme(text=element_text(size=14),plot.subtitle = element_text(size=8))

clust_7a %>% dplyr::select(-model_number,-classes_number,-starts_with("CPROB")) %>% group_by(
  Class) %>% summarise_all(mean,na.rm=T) %>% 
  pivot_longer(
    -names(.)[1],
    names_to="lpa_features",
    values_to="value"
  ) %>% ggplot(aes(y=value,x=lpa_features,fill=factor(Class,labels = c(
    "1 (.%)","2 (.%)","3 (.%)","4 (.%)","5 (.%)","6 (.%)","7 (.%)"
  ))))+
  geom_bar(stat="identity",width=.6,position=position_dodge(width=.7)) +
  labs(x="Features",y="Standardized Z value",title="LPA Cluster Averages", 
       fill="Clusters") +
  theme_bw() +
  theme(text=element_text(size=14),plot.subtitle = element_text(size=8))

clust_8g %>% dplyr::select(-model_number,-classes_number,-starts_with("CPROB")) %>% group_by(
  Class) %>% summarise_all(mean,na.rm=T) %>% 
  pivot_longer(
    -names(.)[1],
    names_to="lpa_features",
    values_to="value"
  ) %>% ggplot(aes(y=value,x=lpa_features,fill=factor(Class,labels = c(
    "Single Family Residential, High Vegetation, Low Accesibility (25.00%)", 
    "Parks and Water features (4.91%)",
    "Mixed Residential, Low Accesibility (11.45%)",
    "High Traffic, Manufacturing, Low Vegetation (18.10%)", 
    "Highest Density, High Accesibility (14.52%)",
    "Lowest Vegetation, High Business/Commerical (9.89%)",
    "Highest Vegetation, Vacant Lots, Some Commercial (4.26%)",
    "High Density Multi-fam residences (11.82%)"
  ))))+
  geom_bar(stat="identity",width=.6,position=position_dodge(width=.7)) +
  scale_fill_brewer(palette = "Set3") +
  labs(x="Features",y="Standardized Z value",title="LPA Cluster Averages", 
       fill="Clusters") +
  theme_bw() +
  theme(text=element_text(size=14),plot.subtitle = element_text(size=8))

clust_8g %>% dplyr::select(-model_number,-classes_number,-starts_with("CPROB"),-starts_with("pct_")) %>% group_by(
  Class) %>% summarise_all(mean,na.rm=T) %>% 
  pivot_longer(
    -names(.)[1],
    names_to="lpa_features",
    values_to="value"
  ) %>% ggplot(aes(y=value,x=lpa_features,fill=factor(Class,labels = c(
    "Single Family Residential, High Vegetation,\n Low Accesibility (25.00%)", 
    "Parks and Water features (4.91%)",
    "Mixed Residential, Low Accesibility\n (11.45%)",
    "High Traffic, Manufacturing, Low Vegetation\n (18.10%)", 
    "Highest Density, High Accesibility\n (14.52%)",
    "Lowest Vegetation, High Business/Commerical\n (9.89%)",
    "Highest Vegetation, Vacant Lots,\n Some Commercial (4.26%)",
    "High Density Multi-fam residences\n (11.82%)"
  ))))+
  geom_bar(stat="identity",width=.6,position=position_dodge(width=.7)) +
  scale_fill_brewer(palette = "Set3") +
  labs(x="Features",y="Standardized Z value", 
       fill="Clusters") +
  theme_bw() +
  theme(text=element_text(size=16),plot.subtitle = element_text(size=8))

clust_8g %>% dplyr::select(Class,starts_with("pct_"),-contains("resid"),-contains("comm")) %>% group_by(
  Class) %>% summarise_all(mean,na.rm=T) %>% 
  pivot_longer(
    -names(.)[1],
    names_to="lpa_features",
    values_to="value"
  ) %>% ggplot(aes(y=value,x=lpa_features,fill=factor(Class,labels = c(
    "Single Family Residential, High Vegetation,\n Low Accesibility (25.00%)", 
    "Parks and Water features (4.91%)",
    "Mixed Residential, Low Accesibility\n (11.45%)",
    "High Traffic, Manufacturing, Low Vegetation\n (18.10%)", 
    "Highest Density, High Accesibility\n (14.52%)",
    "Lowest Vegetation, High Business/Commerical\n (9.89%)",
    "Highest Vegetation, Vacant Lots,\n Some Commercial (4.26%)",
    "High Density Multi-fam residences\n (11.82%)"
  ))))+
  geom_bar(stat="identity",width=.6,position=position_dodge(width=.7)) +
  scale_fill_brewer(palette = "Set3") +
  labs(x="Features",y="Standardized Z value", 
       fill="Clusters") +
  theme_bw() +
  theme(text=element_text(size=16),plot.subtitle = element_text(size=8))

clust_8g %>% dplyr::select(Class,contains("resid"),contains("comm")) %>% group_by(
  Class) %>% summarise_all(mean,na.rm=T) %>% 
  pivot_longer(
    -names(.)[1],
    names_to="lpa_features",
    values_to="value"
  ) %>% ggplot(aes(y=value,x=lpa_features,fill=factor(Class,labels = c(
    "Single Family Residential, High Vegetation,\n Low Accesibility (25.00%)", 
    "Parks and Water features (4.91%)",
    "Mixed Residential, Low Accesibility\n (11.45%)",
    "High Traffic, Manufacturing, Low Vegetation\n (18.10%)", 
    "Highest Density, High Accesibility\n (14.52%)",
    "Lowest Vegetation, High Business/Commerical\n (9.89%)",
    "Highest Vegetation, Vacant Lots,\n Some Commercial (4.26%)",
    "High Density Multi-fam residences\n (11.82%)"
  ))))+
  geom_bar(stat="identity",width=.6,position=position_dodge(width=.7)) +
  scale_fill_brewer(palette = "Set3") +
  labs(x="Features",y="Standardized Z value", 
       fill="Clusters") +
  theme_bw() +
  theme(text=element_text(size=16),plot.subtitle = element_text(size=8))


# naming
# 1.low transit accessibility, high veg single family residential
# 2. parks, water, low pedestrian friendliness
# 3. mixed residential, high veg low transit
# 4. high traffic, high railroads, low veg, high manufacturing/commerical
# 5. highest pop density, high transit accessibility
# 6. lowest vegetation, high business/commercial
# 7. highest veg, high vacant area, some commercial
# 8. residential, multi-fam, high density

