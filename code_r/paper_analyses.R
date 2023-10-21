setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library(tidyverse)
library(sf)
library(spgwr)
library(tmap)
library(ggplot2)

# Pull together final analytic dataset
chicago <- st_read("chicago_boundaries/blocks/geo_export_0a836b4b-86b6-416d-a2a3-4700d035bb01.shp")
chicago <- chicago %>% filter(!tractce10 %in% c("980000","770602")) # not including area by O'hare
chicago <- st_transform(chicago, 26916) # convert to NAD83 / UTM zone 16N
chicago <- chicago %>% mutate(block_area=st_area(.))
chicago <- chicago %>% dplyr::filter(block_area>=units::set_units(100, m^2))
chicago$tract_bloc <- as.numeric(chicago$tract_bloc)
chicago <- chicago %>% distinct(tract_bloc,.keep_all = TRUE)
chicago <- chicago %>% distinct(geoid10,.keep_all = TRUE)

comm_areas <- st_read("chicago_2010_tracts/geo_export_034e8876-472c-4dd0-88b3-4ea893bbd67f.shp")
comm_areas <- comm_areas %>% st_drop_geometry() %>% select(commarea_n,tractce10)
comm_area_labs <- read_csv("CommAreas.csv")
comm_area_labs <- comm_area_labs %>% select(commarea_n=AREA_NUMBE,ca_name=COMMUNITY)
comm_areas <- left_join(comm_areas,comm_area_labs,by="commarea_n")
rm(comm_area_labs)

chicago <- left_join(chicago,comm_areas,by="tractce10")

c_tracts <- st_read("chicago_2010_tracts/geo_export_034e8876-472c-4dd0-88b3-4ea893bbd67f.shp")
c_tracts <- c_tracts %>% filter(!name10 %in% c("9800","7706.02"))
c_tracts <- st_transform(c_tracts, 26916) # convert to NAD83 / UTM zone 16N
c_tracts <- c_tracts %>% st_union()

heat <- read_csv("chicago_block_heat.csv")
heat$geoid10 <- as.character(heat$geoid10)
heat <- heat %>% select(-blockce10,-block_area,-countyfp10,-name10,-statefp10,-tract_bloc,-tractce10)
heat <- heat %>% distinct(geoid10,.keep_all = T)

tree_height <- read_csv("theight_2021.csv")
tree_height <- tree_height %>% filter(!tractce10 %in% c("980000","770602"))
tree_height <-tree_height %>% select(tract_bloc,theight_mean)
tree_height <- tree_height %>% distinct(tract_bloc,.keep_all = TRUE)

tree_binary <- read_csv("tbinary_2021.csv")
tree_binary <- tree_binary %>% filter(!tractce10 %in% c("980000","770602"))
tree_binary <-tree_binary %>% select(tract_bloc,tbinary_mean)
tree_binary <- tree_binary %>% distinct(tract_bloc,.keep_all = TRUE)

block_dems <- read_csv("chicago_block_demographics.csv")
block_dems <- block_dems %>% distinct(geoid10,.keep_all = TRUE)
block_dems$geoid10 <- as.character(block_dems$geoid10)

nvdi_metrics <- read_csv("block_ndvi_metrics.csv")
nvdi_metrics <- nvdi_metrics %>% distinct(geoid10,.keep_all = TRUE)
nvdi_metrics$geoid10 <- as.character(nvdi_metrics$geoid10)


chicago <- chicago %>% select(geoid10,block_area,tract_bloc,ca_name)
chicago <- left_join(chicago,heat,by="geoid10")
chicago <- left_join(chicago,nvdi_metrics,by="geoid10")
chicago <- left_join(chicago,tree_height,by="tract_bloc")
chicago <- left_join(chicago,tree_binary,by="tract_bloc")
chicago <- left_join(chicago,block_dems,by="geoid10")
rm(heat,tree_height,tree_binary,block_dems,nvdi_metrics)

# get rid of observations outside my clip
chicago <- st_intersection(chicago,c_tracts)
rm(c_tracts,comm_areas)

chicago <- chicago %>% mutate(
  treeheight_mean = ifelse(tbinary_mean==0,0,theight_mean)
)

chicago <- chicago %>% filter(is.na(tbinary_mean)==FALSE) %>% filter(is.na(tracts_lst_avg_scaled)==FALSE)

chicago <- chicago %>% rename(pct_tree_cover=tbinary_mean,avg_canopy_height=treeheight_mean,
                              avg_scaled_lst=tracts_lst_avg_scaled)


# scale a couple of the variables that still require cleaning
chicago <- chicago %>% mutate(
  avg_canopy_height_scaled = as.numeric(scale(avg_canopy_height)),
  pct_tree_cover_sq =sqrt(pct_tree_cover)
)

# test two measures of plantable land
chicago <- chicago %>% mutate(
  plantable_land =high_veg-pct_tree_cover,
  plantable_land_area =(high_veg*block_area)-(pct_tree_cover*block_area),
  
)

tree_area = units::set_units(pi*9, m^2)
chicago <- chicago %>% mutate(
  number_potential_trees = as.numeric(floor(plantable_land_area/tree_area)),
  current_tree_area = pct_tree_cover*block_area,
  area_added_by_tree = as.numeric(tree_area/block_area)
)

chicago$number_potential_trees <- ifelse(chicago$number_potential_trees<=0,0,chicago$number_potential_trees)

chicago <- chicago %>% mutate(
  primary_racial_group = case_when(
    pct_white_alone>=.5~"white",
    pct_black_alone>=.5~"black",
    pct_latino>=.5~"latino",
    pct_asian_alone>=.5~"asian",
    TRUE~"no primary race"
  )
)



################################################################################
# Equity
corrplot::corrplot(cor(chicago %>% st_drop_geometry() %>% select(
  avg_canopy_height_scaled,pct_tree_cover,avg_scaled_lst,
  pct_below_pov,pct_white_alone,pct_black_alone,pct_asian_alone,pct_latino),use = "complete.obs"))


cor.test(chicago$pct_tree_cover, chicago$pct_below_pov)
summary(chicago %>%lm(pct_tree_cover ~ pct_black_alone, data=.)) # slight positive correlation
summary(chicago %>%lm(pct_tree_cover ~ pct_latino, data=.)) # slight negative correlation
summary(chicago %>%lm(pct_tree_cover ~ pct_below_pov, data=.)) # slight positive correlation
summary(chicago %>%lm(pct_tree_cover ~ pct_black_alone + pct_latino + pct_asian_alone +pct_white_alone, data=.))


t.test(chicago$pct_tree_cover, mu = 50)

hist(chicago$pct_tree_cover)


summary(aov(chicago$pct_tree_cover ~ chicago$primary_racial_group))
summary(chicago %>%lm(pct_tree_cover ~ as.factor(primary_racial_group), data=.)) # slight positive correlation

pairwise.t.test(chicago$pct_tree_cover,as.factor(chicago$primary_racial_group),
                p.adjust.method = "bonf")
chicago %>% st_drop_geometry() %>% group_by(primary_racial_group) %>% summarise(
  utc = mean(pct_tree_cover)
)


################################################################################
### GWR ###
# Run all of the GWR models -add 2% in each direction to minimize edge effects

chicago_top_left_eighth <- chicago %>% st_crop(.,
                                               xmin=428578.1,
                                               xmax=(456468.9-13945.4+557.816),
                                               ymin=(4610424.0+10532.93+10532.93+10532.93-750),
                                               ymax=4652555.7)

# need to make some assumptions about trees to run simulations
coordsW <- chicago_top_left_eighth %>%
  st_centroid()%>%
  st_geometry()

coordsW2 <- st_coordinates(coordsW)
chicago_top_left_eighth <- cbind(chicago_top_left_eighth,coordsW2)

coords=cbind(chicago_top_left_eighth$X, chicago_top_left_eighth$Y)
chicago_top_left_eighth <- chicago_top_left_eighth %>% st_drop_geometry()
coordinates(chicago_top_left_eighth) <- ~ X + Y

start_time <- Sys.time()
gwr_model_lst <- gwr(avg_scaled_lst ~ pct_tree_cover*avg_canopy_height_scaled,
                     data = chicago_top_left_eighth,
                     coords=cbind(chicago_top_left_eighth$X, chicago_top_left_eighth$Y),
                     adapt=0.02580512, 
                     hatmatrix=T,
                     se.fit=T,
                     predictions=TRUE)
end_time <- Sys.time()
loop_time <- end_time - start_time
print(loop_time)
# Time difference of 2.049124 hours

results_topleft <- as.data.frame(gwr_model_lst$SDF)  
rm(chicago_top_left_eighth)
################################################################################

chicago_top_right_eighth <- chicago %>% st_crop(.,
                                                xmin=(428578.1+13945.4-557.816),
                                                xmax=(456468.9),
                                                ymin=(4610424.0+10532.93+10532.93+10532.93-750),
                                                ymax=4652555.7)

# need to make some assumptions about trees to run simulations
coordsW <- chicago_top_right_eighth %>%
  st_centroid()%>%
  st_geometry()

coordsW2 <- st_coordinates(coordsW)
chicago_top_right_eighth <- cbind(chicago_top_right_eighth,coordsW2)

coords=cbind(chicago_top_right_eighth$X, chicago_top_right_eighth$Y)
chicago_top_right_eighth <- chicago_top_right_eighth %>% st_drop_geometry()
coordinates(chicago_top_right_eighth) <- ~ X + Y

start_time <- Sys.time()
gwr_model_lst <- gwr(avg_scaled_lst ~ pct_tree_cover*avg_canopy_height_scaled,
                     data = chicago_top_right_eighth,
                     coords=cbind(chicago_top_right_eighth$X, chicago_top_right_eighth$Y),
                     adapt=0.05121901, 
                     hatmatrix=T,
                     se.fit=T,
                     predictions=TRUE)
end_time <- Sys.time()
loop_time <- end_time - start_time
print(loop_time)
# Time difference of 2.049124 hours

results_topright <- as.data.frame(gwr_model_lst$SDF)  
rm(chicago_top_right_eighth)
################################################################################
chicago_top_mid_left_eighth <- chicago %>% st_crop(.,
                                                   xmin=428578.1,
                                                   xmax=(456468.9-13945.4+557.816),
                                                   ymin=(4610424.0+10532.93+10532.93-375),
                                                   ymax=(4652555.7-10532.93+375))


# need to make some assumptions about trees to run simulations
coordsW <- chicago_top_mid_left_eighth %>%
  st_centroid()%>%
  st_geometry()

coordsW2 <- st_coordinates(coordsW)
chicago_top_mid_left_eighth <- cbind(chicago_top_mid_left_eighth,coordsW2)

coords=cbind(chicago_top_mid_left_eighth$X, chicago_top_mid_left_eighth$Y)
chicago_top_mid_left_eighth <- chicago_top_mid_left_eighth %>% st_drop_geometry()
coordinates(chicago_top_mid_left_eighth) <- ~ X + Y

start_time <- Sys.time()
gwr_model_lst <- gwr(avg_scaled_lst ~ pct_tree_cover*avg_canopy_height_scaled,
                     data = chicago_top_mid_left_eighth,
                     coords=cbind(chicago_top_mid_left_eighth$X, chicago_top_mid_left_eighth$Y),
                     adapt=0.03320053, 
                     hatmatrix=T,
                     se.fit=T,
                     predictions=TRUE)
end_time <- Sys.time()
loop_time <- end_time - start_time
print(loop_time)
# Time difference of 2.049124 hours

results_topmidleft <- as.data.frame(gwr_model_lst$SDF)  
rm(chicago_top_mid_left_eighth)
################################################################################
chicago_top_mid_right_eighth <- chicago %>% st_crop(.,
                                                    xmin=(428578.1+13945.4-557.816),
                                                    xmax=(456468.9),
                                                    ymin=(4610424.0+10532.93+10532.93-375),
                                                    ymax=(4652555.7-10532.93+375))

# need to make some assumptions about trees to run simulations
coordsW <- chicago_top_mid_right_eighth %>%
  st_centroid()%>%
  st_geometry()

coordsW2 <- st_coordinates(coordsW)
chicago_top_mid_right_eighth <- cbind(chicago_top_mid_right_eighth,coordsW2)

coords=cbind(chicago_top_mid_right_eighth$X, chicago_top_mid_right_eighth$Y)
chicago_top_mid_right_eighth <- chicago_top_mid_right_eighth %>% st_drop_geometry()
coordinates(chicago_top_mid_right_eighth) <- ~ X + Y

start_time <- Sys.time()
gwr_model_lst <- gwr(avg_scaled_lst ~ pct_tree_cover*avg_canopy_height_scaled,
                     data = chicago_top_mid_right_eighth,
                     coords=cbind(chicago_top_mid_right_eighth$X, chicago_top_mid_right_eighth$Y),
                     adapt=0.03288176, 
                     hatmatrix=T,
                     se.fit=T,
                     predictions=TRUE)
end_time <- Sys.time()
loop_time <- end_time - start_time
print(loop_time)
# Time difference of 2.049124 hours

results_topmidright <- as.data.frame(gwr_model_lst$SDF)  
rm(chicago_top_mid_right_eighth)
################################################################################
chicago_bot_mid_left_eighth <- chicago %>% st_crop(.,
                                                   xmin=428578.1,
                                                   xmax=(456468.9-13945.4+557.816),
                                                   ymin=(4610424.0+10532.93-375),
                                                   ymax=(4652555.7-10532.93-10532.93+375))


# need to make some assumptions about trees to run simulations
coordsW <- chicago_bot_mid_left_eighth %>%
  st_centroid()%>%
  st_geometry()

coordsW2 <- st_coordinates(coordsW)
chicago_bot_mid_left_eighth <- cbind(chicago_bot_mid_left_eighth,coordsW2)

coords=cbind(chicago_bot_mid_left_eighth$X, chicago_bot_mid_left_eighth$Y)
chicago_bot_mid_left_eighth <- chicago_bot_mid_left_eighth %>% st_drop_geometry()
coordinates(chicago_bot_mid_left_eighth) <- ~ X + Y

start_time <- Sys.time()
gwr_model_lst <- gwr(avg_scaled_lst ~ pct_tree_cover*avg_canopy_height_scaled,
                     data = chicago_bot_mid_left_eighth,
                     coords=cbind(chicago_bot_mid_left_eighth$X, chicago_bot_mid_left_eighth$Y),
                     adapt=0.0476372, 
                     hatmatrix=T,
                     se.fit=T,
                     predictions=TRUE)
end_time <- Sys.time()
loop_time <- end_time - start_time
print(loop_time)
# Time difference of 2.049124 hours

results_botmidleft <- as.data.frame(gwr_model_lst$SDF)  
rm(chicago_bot_mid_left_eighth)
################################################################################
chicago_bot_mid_right_eighth <- chicago %>% st_crop(.,
                                                    xmin=(428578.1+13945.4-557.816),
                                                    xmax=(456468.9),
                                                    ymin=(4610424.0+10532.93-375),
                                                    ymax=(4652555.7-10532.93-10532.93+375))

# need to make some assumptions about trees to run simulations
coordsW <- chicago_bot_mid_right_eighth %>%
  st_centroid()%>%
  st_geometry()

coordsW2 <- st_coordinates(coordsW)
chicago_bot_mid_right_eighth <- cbind(chicago_bot_mid_right_eighth,coordsW2)

coords=cbind(chicago_bot_mid_right_eighth$X, chicago_bot_mid_right_eighth$Y)
chicago_bot_mid_right_eighth <- chicago_bot_mid_right_eighth %>% st_drop_geometry()
coordinates(chicago_bot_mid_right_eighth) <- ~ X + Y

start_time <- Sys.time()
gwr_model_lst <- gwr(avg_scaled_lst ~ pct_tree_cover*avg_canopy_height_scaled,
                     data = chicago_bot_mid_right_eighth,
                     coords=cbind(chicago_bot_mid_right_eighth$X, chicago_bot_mid_right_eighth$Y),
                     adapt=0.02407318, 
                     hatmatrix=T,
                     se.fit=T,
                     predictions=TRUE)
end_time <- Sys.time()
loop_time <- end_time - start_time
print(loop_time)
# Time difference of 2.049124 hours

results_botmidright <- as.data.frame(gwr_model_lst$SDF)  
rm(chicago_bot_mid_right_eighth)

################################################################################
chicago_bot_left_eighth <- chicago %>% st_crop(.,
                                               xmin=428578.1,
                                               xmax=(456468.9-13945.4+557.816),
                                               ymin=(4610424.0),
                                               ymax=(4652555.7-10532.93-10532.93-10532.93+750))
# 505 final number


# need to make some assumptions about trees to run simulations
coordsW <- chicago_bot_left_eighth %>%
  st_centroid()%>%
  st_geometry()

coordsW2 <- st_coordinates(coordsW)
chicago_bot_left_eighth <- cbind(chicago_bot_left_eighth,coordsW2)

coords=cbind(chicago_bot_left_eighth$X, chicago_bot_left_eighth$Y)
chicago_bot_left_eighth <- chicago_bot_left_eighth %>% st_drop_geometry()
coordinates(chicago_bot_left_eighth) <- ~ X + Y

start_time <- Sys.time()
gwr_model_lst <- gwr(avg_scaled_lst ~ pct_tree_cover*avg_canopy_height_scaled,
                     data = chicago_bot_left_eighth,
                     coords=cbind(chicago_bot_left_eighth$X, chicago_bot_left_eighth$Y),
                     adapt=0.2439024, 
                     hatmatrix=T,
                     se.fit=T,
                     predictions=TRUE)
end_time <- Sys.time()
loop_time <- end_time - start_time
print(loop_time)
# Time difference of 16.12092 secs


results_botleft <- as.data.frame(gwr_model_lst$SDF)  
rm(chicago_bot_left_eighth)

################################################################################
chicago_bot_right_eighth <- chicago %>% st_crop(.,
                                                xmin=(428578.1+13945.4-557.816),
                                                xmax=(456468.9),
                                                ymin=(4610424.0),
                                                ymax=(4652555.7-10532.93-10532.93-10532.93+750))

# need to make some assumptions about trees to run simulations
coordsW <- chicago_bot_right_eighth %>%
  st_centroid()%>%
  st_geometry()

coordsW2 <- st_coordinates(coordsW)
chicago_bot_right_eighth <- cbind(chicago_bot_right_eighth,coordsW2)

coords=cbind(chicago_bot_right_eighth$X, chicago_bot_right_eighth$Y)
chicago_bot_right_eighth <- chicago_bot_right_eighth %>% st_drop_geometry()
coordinates(chicago_bot_right_eighth) <- ~ X + Y

start_time <- Sys.time()
gwr_model_lst <- gwr(avg_scaled_lst ~ pct_tree_cover*avg_canopy_height_scaled,
                     data = chicago_bot_right_eighth,
                     coords=cbind(chicago_bot_right_eighth$X, chicago_bot_right_eighth$Y),
                     adapt=0.0292603, 
                     hatmatrix=T,
                     se.fit=T,
                     predictions=TRUE)
end_time <- Sys.time()
loop_time <- end_time - start_time
print(loop_time)
# Time difference of 2.049124 hours

results_botright <- as.data.frame(gwr_model_lst$SDF)  
rm(chicago_bot_right_eighth)

################################################################################
################################################################################
# max 3 hours each
# compile and save results

chicago_top_left_eighth <- chicago %>% st_crop(.,
                                               xmin=428578.1,
                                               xmax=(456468.9-13945.4+557.816),
                                               ymin=(4610424.0+10532.93+10532.93+10532.93-750),
                                               ymax=4652555.7) %>% select(geoid10)
chicago_top_left_eighth <- bind_cols(chicago_top_left_eighth,results_topleft)
chicago_top_left_eighth <- chicago_top_left_eighth %>% st_crop(.,
                                                               xmin=428578.1,
                                                               xmax=(456468.9-13945.4),
                                                               ymin=(4610424.0+10532.93+10532.93+10532.93),
                                                               ymax=4652555.7) %>% st_drop_geometry()


chicago_top_right_eighth <- chicago %>% st_crop(.,
                                                xmin=(428578.1+13945.4-557.816),
                                                xmax=(456468.9),
                                                ymin=(4610424.0+10532.93+10532.93+10532.93-750),
                                                ymax=4652555.7)%>% select(geoid10)
chicago_top_right_eighth <- bind_cols(chicago_top_right_eighth,results_topright)
chicago_top_right_eighth <- chicago_top_right_eighth %>% st_crop(.,
                                                                 xmin=(428578.1+13945.4),
                                                                 xmax=(456468.9),
                                                                 ymin=(4610424.0+10532.93+10532.93+10532.93),
                                                                 ymax=4652555.7) %>% st_drop_geometry()


chicago_top_mid_left_eighth <- chicago %>% st_crop(.,
                                                   xmin=428578.1,
                                                   xmax=(456468.9-13945.4+557.816),
                                                   ymin=(4610424.0+10532.93+10532.93-375),
                                                   ymax=(4652555.7-10532.93+375)) %>% select(geoid10)
chicago_top_mid_left_eighth <- bind_cols(chicago_top_mid_left_eighth,results_topmidleft)
chicago_top_mid_left_eighth <- chicago_top_mid_left_eighth %>% st_crop(.,
                                                                       xmin=428578.1,
                                                                       xmax=(456468.9-13945.4),
                                                                       ymin=(4610424.0+10532.93+10532.93),
                                                                       ymax=(4652555.7-10532.93))%>% st_drop_geometry()



chicago_top_mid_right_eighth <- chicago %>% st_crop(.,
                                                    xmin=(428578.1+13945.4-557.816),
                                                    xmax=(456468.9),
                                                    ymin=(4610424.0+10532.93+10532.93-375),
                                                    ymax=(4652555.7-10532.93+375)) %>% select(geoid10)
chicago_top_mid_right_eighth <- bind_cols(chicago_top_mid_right_eighth,results_topmidright)
chicago_top_mid_right_eighth <- chicago_top_mid_right_eighth %>% st_crop(.,
                                                                         xmin=(428578.1+13945.4),
                                                                         xmax=(456468.9),
                                                                         ymin=(4610424.0+10532.93+10532.93),
                                                                         ymax=(4652555.7-10532.93))%>% st_drop_geometry()


chicago_bot_mid_left_eighth <- chicago %>% st_crop(.,
                                                   xmin=428578.1,
                                                   xmax=(456468.9-13945.4+557.816),
                                                   ymin=(4610424.0+10532.93-375),
                                                   ymax=(4652555.7-10532.93-10532.93+375)) %>% select(geoid10)
chicago_bot_mid_left_eighth <- bind_cols(chicago_bot_mid_left_eighth,results_botmidleft)
chicago_bot_mid_left_eighth <- chicago_bot_mid_left_eighth %>% st_crop(.,
                                                                       xmin=428578.1,
                                                                       xmax=(456468.9-13945.4),
                                                                       ymin=(4610424.0+10532.93),
                                                                       ymax=(4652555.7-10532.93-10532.93))%>% st_drop_geometry()


chicago_bot_mid_right_eighth <- chicago %>% st_crop(.,
                                                    xmin=(428578.1+13945.4-557.816),
                                                    xmax=(456468.9),
                                                    ymin=(4610424.0+10532.93-375),
                                                    ymax=(4652555.7-10532.93-10532.93+375)) %>% select(geoid10)
chicago_bot_mid_right_eighth <- bind_cols(chicago_bot_mid_right_eighth,results_botmidright)
chicago_bot_mid_right_eighth <- chicago_bot_mid_right_eighth %>% st_crop(.,
                                                                         xmin=(428578.1+13945.4),
                                                                         xmax=(456468.9),
                                                                         ymin=(4610424.0+10532.93),
                                                                         ymax=(4652555.7-10532.93-10532.93))%>% st_drop_geometry()



chicago_bot_left_eighth <- chicago %>% st_crop(.,
                                               xmin=428578.1,
                                               xmax=(456468.9-13945.4+557.816),
                                               ymin=(4610424.0),
                                               ymax=(4652555.7-10532.93-10532.93-10532.93+750)) %>% select(geoid10)
chicago_bot_left_eighth <- bind_cols(chicago_bot_left_eighth,results_botleft)
chicago_bot_left_eighth <- chicago_bot_left_eighth %>% st_crop(.,
                                                               xmin=428578.1,
                                                               xmax=(456468.9-13945.4),
                                                               ymin=(4610424.0),
                                                               ymax=(4652555.7-10532.93-10532.93-10532.93)) %>% st_drop_geometry()


chicago_bot_right_eighth <- chicago %>% st_crop(.,
                                                xmin=(428578.1+13945.4-557.816),
                                                xmax=(456468.9),
                                                ymin=(4610424.0),
                                                ymax=(4652555.7-10532.93-10532.93-10532.93+750)) %>% select(geoid10)
chicago_bot_right_eighth <- bind_cols(chicago_bot_right_eighth,results_botright)
chicago_bot_right_eighth <- chicago_bot_right_eighth %>% st_crop(.,
                                                                 xmin=(428578.1+13945.4),
                                                                 xmax=(456468.9),
                                                                 ymin=(4610424.0),
                                                                 ymax=(4652555.7-10532.93-10532.93-10532.93)) %>% st_drop_geometry()



all_chicago_lst_gwr <- bind_rows(chicago_bot_right_eighth,chicago_bot_left_eighth,
                                 chicago_bot_mid_right_eighth,chicago_bot_mid_left_eighth,
                                 chicago_top_mid_right_eighth,chicago_top_mid_left_eighth,
                                 chicago_top_right_eighth,chicago_top_left_eighth )

all_chicago_lst_gwr <- all_chicago_lst_gwr %>%  distinct(geoid10,.keep_all = TRUE)
# Save Results
#write.csv(all_chicago_lst_gwr,"gwr_lst_coverxheight.csv",row.names = F)

# Create a clean file for the tree placement simulations
# # move over to python for minimization equations
# gwr_coverxheight<- read_csv("gwr_lst_coverxheight.csv")
# gwr_coverxheight <- gwr_coverxheight %>% select(
#   geoid10,xintercept_coverxheight=X.Intercept.,teffect_coverxheight=pct_tree_cover,
#   heighteffect_coverxheight=avg_canopy_height_scaled,
#   interactioneffect_coverxheight=pct_tree_cover.avg_canopy_height_scaled,
# )
# gwr_coveralone<- read_csv("gwr_lst_coveralone.csv")
# gwr_coveralone <- gwr_coveralone %>% select(
#   geoid10,xintercept_cover=X.Intercept.,teffect_cover=pct_tree_cover
# )
# 
# minimize_data <-chicago %>% st_drop_geometry()
# minimize_data$geoid10 <- as.numeric(minimize_data$geoid10)
# minimize_data <- minimize_data %>% select(
#   geoid10,pct_tree_cover,avg_canopy_height_scaled,number_potential_trees,area_added_by_tree,
# 
# )
# 
# minimize_data <- left_join(minimize_data,gwr_coveralone,by="geoid10")
# minimize_data <- left_join(minimize_data,gwr_coverxheight,by="geoid10")
# 
# # write.csv(minimize_data,"minimize_data.csv",row.names = F)


################################################################################
### Figures
################################################################################
#1. 1x3 map of outcome variables at the block level

# extract a basemap


# type = c("osm", "osm-bw",
#          "maptoolkit-topo", "waze", "bing", "stamen-toner", "stamen-terrain",
#          "stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox", "esri",
#          "esri-topo", "nps", "apple-iphoto", "skobbler", "hillshade", "opencyclemap",
#          "osm-transport", "osm-public-transport", "osm-bbike", "osm-bbike-german")

## Map with raster layer to make sure everything looks alright
water <- st_read("Waterways_Chicago/geo_export_110c65a5-f82f-43cd-9b41-9ae24632d43d.shp")
#water <- water %>% filter(!name %in% "LAKE MICHIGAN")
#water %>% st_geometry() %>% plot()
water <- st_transform(water, 26916) # convert to NAD83 / UTM zone 16N

chicago_boundary <- st_read("Boundaries - City/geo_export_5d37251c-7044-4a45-a8f5-a0e58cae7a2b.shp")
chicago_boundary <- st_transform(chicago_boundary, 26916) 

water <- st_crop(water,chicago)
#water %>% st_geometry() %>% plot()

chicago <- st_crop(chicago,chicago_boundary)
chicago2 <- st_intersection(chicago,chicago_boundary)

lake <- water %>% filter(name %in% "LAKE MICHIGAN")
tm_shape(water) +
  tm_polygons(col = "lightblue") +
  tmap::tm_shape(chicago2) +
  tmap::tm_fill("avg_scaled_lst",style = "cont",palette = "seq",
                title = "Std. Land Surface Temperature",) +
  tm_shape(chicago_boundary) +
  tm_polygons(alpha = 0, border.alpha = .8, border.col = "black") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))

p1 <- tm_shape(water) +
  tm_polygons(col = "lightblue",border.alpha=0) +
  tmap::tm_shape(chicago2) +
  tmap::tm_fill("avg_scaled_lst",style = "cont",palette = "seq",
                title = "Std. Land Surface Temperature",) +
  tm_shape(chicago_boundary) +
  tm_polygons(alpha = 0, border.alpha = .5, border.col = "black") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))



chicago2 <- chicago2 %>% mutate(
  pct_below_pov = ifelse(is.na(pct_below_pov)==TRUE,0,pct_below_pov),
  racial_poverty = case_when(
    primary_racial_group=="asian" & pct_below_pov>=.235~"asian high pov",
    primary_racial_group=="asian" & pct_below_pov<.235~"asian reg",
    primary_racial_group=="black" & pct_below_pov>=.235~"black high pov",
    primary_racial_group=="black" & pct_below_pov<.235~"black reg",
    primary_racial_group=="latino" & pct_below_pov>=.235~"latino high pov",
    primary_racial_group=="latino" & pct_below_pov<.235~"latino reg",
    primary_racial_group=="no primary race" & pct_below_pov>=.235~"no primary high pov",
    primary_racial_group=="no primary race" & pct_below_pov<.235~"no primary reg",
    primary_racial_group=="white" & pct_below_pov>=.235~"white high pov",
    primary_racial_group=="white" & pct_below_pov<.235~"white reg",
  )
)

p2 <-  tmap::tm_shape(chicago2) +
  tmap::tm_fill("racial_poverty",style = "cat",
                palette =  c('#7218cc','#975fcf',
                             '#4121de','#6d58d6',
                             '#075718','#6fde8d',
                             '#4a484d','#a59fab',
                             '#e32424','#e68181'),
                title = "Primary Racial Group", ) +
  tm_shape(chicago_boundary) +
  tm_polygons(alpha = 0, border.alpha = .5, border.col = "black") +
  tm_shape(lake) +
  tm_polygons(col = "lightblue",border.alpha=0) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))


tmap::tmap_arrange(p1,p2)


p1 <- tmap::tm_shape(chicago2) +
  tmap::tm_fill("pct_tree_cover",style = "fixed",
                breaks = c(0,.01,.05, .1,.2, .3,.4, .5, 1),
                palette = "Greens",
                title = "Estimated Tree Canopy (%)",) +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)


chicago$avg_canopy_height_scaled2 <- if_else(chicago$avg_canopy_height_scaled>=4,4,chicago$avg_canopy_height_scaled)
chicago2$avg_canopy_height_scaled2 <- if_else(chicago2$avg_canopy_height_scaled>=4,4,chicago2$avg_canopy_height_scaled)
p2 <-tmap::tm_shape(chicago2) +
  tmap::tm_fill("avg_canopy_height_scaled2",style = "cont",
                #breaks = c(0,10,20,30,40,50,100),
                #breaks = c(-4,-3,-2,-1,-.5,.5,1,2,3,4),
                palette = "RdYlGn",
                title = "Average Canopy Height",) +
  tm_compass(position = c("right","top"),size=.8,text.size = .8) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)

tmap::tmap_arrange(p1,p2)
################################################################################
# GWR maps
################################################################################
#write.csv(all_chicago_lst_gwr,"gwr_lst_coverxheight.csv",row.names = F)
gwr_coveralone <- read_csv("gwr_lst_coveralone.csv")
gwr_coverxheight <- read_csv("gwr_lst_coverxheight.csv")

gwr_coveralone <- gwr_coveralone %>% select(geoid10,tree_cover_coef_coveralone=pct_tree_cover)
gwr_coverxheight <- gwr_coverxheight %>% select(
  geoid10,
  tree_cover_coef_coverxheight=pct_tree_cover,
  canopy_height_coef_coverxheight=avg_canopy_height_scaled,
  interaction_coef_coverxheight=pct_tree_cover.avg_canopy_height_scaled)
gwr_coveralone$geoid10 <- as.character(gwr_coveralone$geoid10)
gwr_coverxheight$geoid10 <- as.character(gwr_coverxheight$geoid10)

chicago2 <- left_join(chicago2,gwr_coveralone,by="geoid10" )
chicago2 <- left_join(chicago2,gwr_coverxheight,by="geoid10" )


lake <- water %>% filter(name %in% "LAKE MICHIGAN")
p1<-tm_shape(water) +
  tm_polygons(col = "lightblue",border.alpha = 0) +
  tmap::tm_shape(chicago2) +
  tmap::tm_fill("tree_cover_coef_coveralone",style = "cont",palette = "seq",
                title = "Canopy Cover coef CoverAlone Model",) +
  # tm_shape(chicago_boundary) +
  # tm_polygons(alpha = 0, border.alpha = .8, border.col = "black") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))


p2<-tm_shape(water) +
  tm_polygons(col = "lightblue",border.alpha = 0) +
  tmap::tm_shape(chicago2) +
  tmap::tm_fill("tree_cover_coef_coverxheight",style = "cont",palette = "seq",
                title = "Canopy Cover coef CoverxHeight Model",) +
  # tm_shape(chicago_boundary) +
  # tm_polygons(alpha = 0, border.alpha = .8, border.col = "black") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))



p3<-tm_shape(water) +
  tm_polygons(col = "lightblue",border.alpha = 0) +
  tmap::tm_shape(chicago2) +
  tmap::tm_fill("canopy_height_coef_coverxheight",style = "cont",palette = "seq",
                title = "Canopy Height coef CoverxHeight Model",) +
  # tm_shape(chicago_boundary) +
  # tm_polygons(alpha = 0, border.alpha = .8, border.col = "black") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))


p4<-tm_shape(water) +
  tm_polygons(col = "lightblue",border.alpha = 0) +
  tmap::tm_shape(chicago2) +
  tmap::tm_fill("interaction_coef_coverxheight",style = "cont",palette = "seq",
                title = "Interaction coef CoverxHeight Model",) +
  # tm_shape(chicago_boundary) +
  # tm_polygons(alpha = 0, border.alpha = .8, border.col = "black") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))






tmap::tmap_arrange(p1,p2,p3,p4)

################################################################################
#Final maps
################################################################################
# UNET Image Predictions
library(raster)
imagelist1 <- list.files("2021_predictions_results/") %>% Filter(function(x) {str_detect(x,"tif")}, .)
imagelist1 <- unlist(lapply(imagelist1, function(i){paste0("2021_predictions_results/",i)}))

a <- sample.int(12972, 5)

all1 <- stack(imagelist1[a[1]])
names(all1) <- c("r","g","b","height","cover")
all1$newheight <- mask(all1$height,
                       all1$cover,
                       maskvalue=0) 


all2 <- stack(imagelist1[a[2]])
names(all2) <- c("r","g","b","height","cover")
all2$newheight <- mask(all2$height,
                       all2$cover,
                       maskvalue=0) 


all3 <- stack(imagelist1[a[3]])
names(all3) <- c("r","g","b","height","cover")
all3$newheight <- mask(all3$height,
                       all3$cover,
                       maskvalue=0) 

all4 <- stack(imagelist1[a[4]])
names(all4) <- c("r","g","b","height","cover")
all4$newheight <- mask(all4$height,
                       all4$cover,
                       maskvalue=0) 

all5 <- stack(imagelist1[a[5]])
names(all5) <- c("r","g","b","height","cover")
all5$newheight <- mask(all5$height,
                       all5$cover,
                       maskvalue=0) 

par(mfrow=c(3,5))

plotRGB(all1)
plotRGB(all2)
plotRGB(all3)
plotRGB(all4)
plotRGB(all5)

plotRGB(all1)
plot(all1$newheight,add=T,alpha=.5) # height
plotRGB(all2)
plot(all2$newheight,add=T,alpha=.5) # height
plotRGB(all3)
plot(all3$newheight,add=T,alpha=.5) # height
plotRGB(all4)
plot(all4$newheight,add=T,alpha=.5) # height
plotRGB(all5)
plot(all5$newheight,add=T,alpha=.5) # height

plotRGB(all1)
plot(all1$cover,add=T,alpha=.5,legend=F) # cover
plotRGB(all2)
plot(all2$cover,add=T,alpha=.5,legend=F) # cover
plotRGB(all3)
plot(all3$cover,add=T,alpha=.5,legend=F) # cover
plotRGB(all4)
plot(all4$cover,add=T,alpha=.5,legend=F) # cover
plotRGB(all5)
plot(all5$cover,add=T,alpha=.5,legend=F) # cover



all1rgb <- stack("2017_3/P_1_X0_1_X1_240_Y0_1_Y1_240_1019.tif")
names(all1rgb) <- c("r","g","b","4","5","6","7","8","9","10","11","12","13","14","height","cover","17")
all1rgb$newheight <- raster::mask(all1rgb$height,
                          all1rgb$cover,
                          maskvalue=NA) 
all1rgb$newheight <- all1rgb$newheight/3.28084

all2rgb <- stack("2017_3/P_9_X0_481_X1_720_Y0_481_Y1_720_790.tif")
names(all2rgb) <- c("r","g","b","4","5","6","7","8","9","10","11","12","13","14","height","cover","17")
all2rgb$newheight <- raster::mask(all2rgb$height,
                                  all2rgb$cover,
                                  maskvalue=NA) 
all2rgb$newheight <- all2rgb$newheight/3.28084

all3rgb <- stack("2017_3/P_9_X0_481_X1_720_Y0_481_Y1_720_956.tif")
names(all3rgb) <- c("r","g","b","4","5","6","7","8","9","10","11","12","13","14","height","cover","17")
all3rgb$newheight <- raster::mask(all3rgb$height,
                                  all3rgb$cover,
                                  maskvalue=NA) 
all3rgb$newheight <- all3rgb$newheight/3.28084

all4rgb <- stack("2017_3/P_5_X0_241_X1_480_Y0_241_Y1_480_705.tif")
names(all4rgb) <- c("r","g","b","4","5","6","7","8","9","10","11","12","13","14","height","cover","17")
all4rgb$newheight <- raster::mask(all4rgb$height,
                                  all4rgb$cover,
                                  maskvalue=NA) 
all4rgb$newheight <- all4rgb$newheight/3.28084

all5rgb <- stack("2017_3/P_8_X0_481_X1_720_Y0_241_Y1_480_331.tif")
names(all5rgb) <- c("r","g","b","4","5","6","7","8","9","10","11","12","13","14","height","cover","17")
all5rgb$newheight <- raster::mask(all5rgb$height,
                                  all5rgb$cover,
                                  maskvalue=NA) 
all5rgb$newheight <- all5rgb$newheight/3.28084

all1height <- raster("2017_fig_preds/theight/P_1_X0_1_X1_240_Y0_1_Y1_240_1019.tif")
all1height <- all1height/3.28084
all1cover <- raster("2017_fig_preds/tbinary/P_1_X0_1_X1_240_Y0_1_Y1_240_1019.tif")
all1height[all1height == 0] <- NA
all1cover[all1cover == 0] <- NA


all2height <- raster("2017_fig_preds/theight/P_9_X0_481_X1_720_Y0_481_Y1_720_790.tif")
all2height <- all2height/3.28084
all2cover <- raster("2017_fig_preds/tbinary/P_9_X0_481_X1_720_Y0_481_Y1_720_790.tif")
all2height[all2height == 0] <- NA
all2cover[all2cover == 0] <- NA

all3height <- raster("2017_fig_preds/theight/P_9_X0_481_X1_720_Y0_481_Y1_720_956.tif")
all3height <- all3height/3.28084
all3cover <- raster("2017_fig_preds/tbinary/P_9_X0_481_X1_720_Y0_481_Y1_720_956.tif")
all3height[all3height == 0] <- NA
all3cover[all3cover == 0] <- NA

all4height <- raster("2017_fig_preds/theight/P_5_X0_241_X1_480_Y0_241_Y1_480_705.tif")
all4height <- all4height/3.28084
all4cover <- raster("2017_fig_preds/tbinary/P_5_X0_241_X1_480_Y0_241_Y1_480_705.tif")
all4height[all4height == 0] <- NA
all4cover[all4cover == 0] <- NA

all5height <- raster("2017_fig_preds/theight/P_8_X0_481_X1_720_Y0_241_Y1_480_331.tif")
all5height <- all5height/3.28084
all5cover <- raster("2017_fig_preds/tbinary/P_8_X0_481_X1_720_Y0_241_Y1_480_331.tif")
all5height[all5height == 0] <- NA
all5cover[all5cover == 0] <- NA


par(mfrow=c(5,5))

plotRGB(all1rgb)
plot(all1rgb$cover,add=T,alpha=.9,legend=F) # cover
plotRGB(all2rgb)
plot(all2rgb$cover,add=T,alpha=.9,legend=F) # cover
plotRGB(all3rgb)
plot(all3rgb$cover,add=T,alpha=.9,legend=F) # cover
plotRGB(all4rgb)
plot(all4rgb$cover,add=T,alpha=.9,legend=F) # cover
plotRGB(all5rgb)
plot(all5rgb$cover,add=T,alpha=.9,legend=F) # cover

plotRGB(all1rgb)
plot(all1rgb$newheight,add=T,alpha=.9) # height
plotRGB(all2rgb)
plot(all2rgb$newheight,add=T,alpha=.9) # height
plotRGB(all3rgb)
plot(all3rgb$newheight,add=T,alpha=.9) # height
plotRGB(all4rgb)
plot(all4rgb$newheight,add=T,alpha=.9) # height
plotRGB(all5rgb)
plot(all5rgb$newheight,add=T,alpha=.9) # height


plotRGB(all1rgb)
plotRGB(all2rgb)
plotRGB(all3rgb)
plotRGB(all4rgb)
plotRGB(all5rgb)


plotRGB(all1rgb)
plot(all1height,add=T,alpha=.9) # height
plotRGB(all2rgb)
plot(all2height,add=T,alpha=.9) # height
plotRGB(all3rgb)
plot(all3height,add=T,alpha=.9) # height
plotRGB(all4rgb)
plot(all4height,add=T,alpha=.9) # height
plotRGB(all5rgb)
plot(all5height,add=T,alpha=.9) # height

plotRGB(all1rgb)
plot(all1cover,add=T,alpha=.9,legend=F) # cover
plotRGB(all2rgb)
plot(all2cover,add=T,alpha=.9,legend=F) # cover
plotRGB(all3rgb)
plot(all3cover,add=T,alpha=.9,legend=F) # cover
plotRGB(all4rgb)
plot(all4cover,add=T,alpha=.9,legend=F) # cover
plotRGB(all5rgb)
plot(all5cover,add=T,alpha=.9,legend=F) # cover


################################################################################
################################################################################
#Final maps
temp_scenario1 <- read_csv("paper_allocated_trees_coveralone.csv")
temp_scenario1 <- temp_scenario1 %>% select(geoid10,allocated_trees_coveralone=allocated_trees) # 2658 blocks
temp_scenario2 <- read_csv("paper_allocated_trees_coverheight.csv")
temp_scenario2 <- temp_scenario2 %>% select(geoid10,allocated_trees_coverheight=allocated_trees) # 2631 blocks



eq_scenario1 <- read_csv("paper_allocated_trees_equity.csv")
eq_scenario1 <- eq_scenario1 %>% select(geoid10,allocated_trees_eqcover=allocated_trees) # 3549
eq_scenario2 <- read_csv("paper_allocated_trees_equityheight.csv")
eq_scenario2 <- eq_scenario2 %>% select(geoid10,allocated_trees_eqcoverheight=allocated_trees) # 2860



chicago$geoid10 <- as.numeric(chicago$geoid10)
chicago <- left_join(chicago,temp_scenario1,by="geoid10")
chicago <- left_join(chicago,temp_scenario2,by="geoid10")
chicago <- left_join(chicago,eq_scenario1,by="geoid10")
chicago <- left_join(chicago,eq_scenario2,by="geoid10")

chicago <- chicago %>% mutate(
  allocated_temp_diff=allocated_trees_coverheight-allocated_trees_coveralone,
  allocated_equity_diff=allocated_trees_eqcoverheight-allocated_trees_eqcover
)


comm_area_1 <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_temp1 = sum(allocated_trees_coveralone,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE) %>% mutate(ca_num=row_number())

comm_area_2 <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_temp2 = sum(allocated_trees_coverheight,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)%>% mutate(ca_num=row_number())

comm_area_2a <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_tempdiff = sum(allocated_temp_diff,na.rm=T),
  added_trees_temp2 = sum(allocated_trees_coverheight,na.rm=T),
  added_trees_temp1 = sum(allocated_trees_coveralone,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)%>% mutate(
    ca_num=row_number(),
    tdiff=added_trees_temp2-added_trees_temp1)



comm_area_3 <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_eq1 = sum(allocated_trees_eqcover,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)%>% mutate(ca_num=row_number())

comm_area_4 <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_eq2 = sum(allocated_trees_eqcoverheight,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)%>% mutate(ca_num=row_number())

comm_area_4a <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_eqdiff = sum(allocated_equity_diff,na.rm=T),
  added_trees_eq2 = sum(allocated_trees_eqcoverheight,na.rm=T),
  added_trees_eq1 = sum(allocated_trees_eqcover,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)%>% mutate(
    ca_num=row_number(),
    tdiff=added_trees_eq2-added_trees_eq1)



p1 <- tmap::tm_shape(comm_area_1) +
  tmap::tm_fill("added_trees_temp1",style = "cont",
                breaks=c(seq(0,5000,500),7500,9000),
                palette = "Greens") +
  tmap::tm_shape(comm_area_1) +
  tmap::tm_polygons(alpha = 0,border.alpha = .3,border.col = "grey") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
#  tm_text("ca_num",size=.5) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)
p2 <- tmap::tm_shape(comm_area_2a) +
  tmap::tm_fill("tdiff",style = "cont",
                breaks= c(-1000,-750,-500,-250,0,250,500,750,1000),
                palette = "RdBu") +
  tmap::tm_shape(comm_area_1) +
  tmap::tm_polygons(alpha = 0,border.alpha = .3,border.col = "grey") +
#  tm_text("ca_num",size=.5) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)
p3 <- tmap::tm_shape(comm_area_3) +
  tmap::tm_fill("added_trees_eq1",style = "cont",
                breaks=c(seq(0,5000,500),7500,9000),
                palette = "Greens") +
  tmap::tm_shape(comm_area_1) +
  tmap::tm_polygons(alpha = 0,border.alpha = .3,border.col = "grey") +
#  tm_text("ca_num",size=.5) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)
p4 <- tmap::tm_shape(comm_area_4a) +
  tmap::tm_fill("tdiff",style = "cont",
                breaks= c(-1100,-750,-500,-250,0,250,500,750,1000),
                palette = "RdBu") +
  tmap::tm_shape(comm_area_1) +
  tmap::tm_polygons(alpha = 0,border.alpha = .3,border.col = "grey") +
#  tm_text("ca_num",size=.5) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)


tmap::tmap_arrange(p1,p2,p3,p4,ncol = 2)


################################################################################
chicago <- chicago %>% mutate(
  primary_racial_group = case_when(
    pct_white_alone>=.5~"white",
    pct_black_alone>=.5~"black",
    pct_latino>=.5~"latino",
    pct_asian_alone>=.5~"asian",
    TRUE~"no primary race"
  )
)
summary(aov(chicago$pct_tree_cover ~ chicago$primary_racial_group))
summary(chicago %>%lm(pct_tree_cover ~ as.factor(primary_racial_group), data=.)) # slight positive correlation

pairwise.t.test(chicago$pct_tree_cover,as.factor(chicago$primary_racial_group),
                p.adjust.method = "bonf")

chicago %>% st_drop_geometry()  %>%  mutate(
  high_poverty=if_else(pct_below_pov>=.235,1,0))%>% group_by(high_poverty) %>% summarise(
  utc = mean(avg_scaled_lst),
  a= mean(pct_tree_cover)
)

chicago %>% st_drop_geometry() %>%  mutate(
  number_potential_trees=if_else(number_potential_trees<0,0,number_potential_trees),
  high_poverty=if_else(pct_below_pov>=.235,1,0)
) %>% group_by(high_poverty) %>% summarise(
  number_potential_trees= mean(number_potential_trees,na.rm=T)
)
chicago %>% st_drop_geometry() %>%  mutate(
  number_potential_trees=if_else(number_potential_trees<0,0,number_potential_trees)
) %>% group_by(primary_racial_group) %>% summarise(
  number_potential_trees= mean(number_potential_trees,na.rm=T)
)




chicago %>% st_drop_geometry() %>%  mutate(
  number_potential_trees=if_else(number_potential_trees<0,0,number_potential_trees),
  high_poverty=if_else(pct_below_pov>=.235,1,0)
) %>% group_by(high_poverty) %>% summarise(
  trees_lst1 = sum(allocated_trees_coveralone,na.rm=T), #/75000,
  trees_lst2 = sum(allocated_trees_coverheight,na.rm=T), #/75000,
  trees_eq1 = sum(allocated_trees_eqcover,na.rm=T), #/75000,
  trees_eq2 = sum(allocated_trees_eqcoverheight,na.rm=T) #/75000
  )

chicago %>% st_drop_geometry() %>%  mutate(
  number_potential_trees=if_else(number_potential_trees<0,0,number_potential_trees),
  high_poverty=if_else(pct_below_pov>=.235,1,0)
) %>% group_by(high_poverty) %>% summarise(
  trees_lst1 = sum(allocated_trees_coveralone,na.rm=T), #/75000,
  trees_lst2 = sum(allocated_trees_coverheight,na.rm=T), #/75000,
  trees_eq1 = sum(allocated_trees_eqcover,na.rm=T), #/75000,
  trees_eq2 = sum(allocated_trees_eqcoverheight,na.rm=T) #/75000
)

chicago <- left_join(chicago,minimize_data,by="geoid10")


chicago <- chicago %>% mutate(
  temp_height0= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(0)+interactioneffect_coverxheight*(pct_tree_cover.x)*(0),
  temp_height.5= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(.5)+interactioneffect_coverxheight*(pct_tree_cover.x)*(.5),
  temp_height1= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(1)+interactioneffect_coverxheight*(pct_tree_cover.x)*(1),
  temp_height1.5= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(1.5)+interactioneffect_coverxheight*(pct_tree_cover.x)*(1.5),
  temp_height2= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(2)+interactioneffect_coverxheight*(pct_tree_cover.x)*(2),
  temp_height2.5= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(2.5)+interactioneffect_coverxheight*(pct_tree_cover.x)*(2.5),
  temp_height3= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(3)+interactioneffect_coverxheight*(pct_tree_cover.x)*(3),
  temp_height3.5= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(3.5)+interactioneffect_coverxheight*(pct_tree_cover.x)*(3.5),
  temp_height4= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(4)+interactioneffect_coverxheight*(pct_tree_cover.x)*(4),
  temp_height4.5= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(4.5)+interactioneffect_coverxheight*(pct_tree_cover.x)*(4.5),
  temp_height5= xintercept_coverxheight + teffect_coverxheight*(pct_tree_cover.x)+ avg_canopy_height_scaled.x*(5)+interactioneffect_coverxheight*(pct_tree_cover.x)*(5)
)

