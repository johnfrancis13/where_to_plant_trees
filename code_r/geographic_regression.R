setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library(tidyverse)
library(sf)
library(spgwr)
library(tmap)
library(ggplot2)
#https://andrewmaclachlan.github.io/CASA0005repo/explaining-spatial-patterns.html

# Pull together final analytic dataset

chicago <- st_read("../data/cluster_data/chicago_boundaries/blocks/geo_export_0a836b4b-86b6-416d-a2a3-4700d035bb01.shp")
chicago <- chicago %>% filter(!tractce10 %in% c("980000","770602")) # not including area by O'hare
chicago <- st_transform(chicago, 26916) # convert to NAD83 / UTM zone 16N
chicago <- chicago %>% mutate(block_area=st_area(.))
chicago <- chicago %>% dplyr::filter(block_area>=units::set_units(100, m^2))
chicago$tract_bloc <- as.numeric(chicago$tract_bloc)
chicago <- chicago %>% distinct(tract_bloc,.keep_all = TRUE)
chicago <- chicago %>% distinct(geoid10,.keep_all = TRUE)

comm_areas <- st_read("C:/Users/johnf/Box/UCL/thesis/cluster_data/chicago_2010_tracts/geo_export_034e8876-472c-4dd0-88b3-4ea893bbd67f.shp")
comm_areas <- comm_areas %>% st_drop_geometry() %>% select(commarea_n,tractce10)
comm_area_labs <- read_csv("D:/final_data/CommAreas.csv")
comm_area_labs <- comm_area_labs %>% select(commarea_n=AREA_NUMBE,ca_name=COMMUNITY)
comm_areas <- left_join(comm_areas,comm_area_labs,by="commarea_n")
rm(comm_area_labs)

chicago <- left_join(chicago,comm_areas,by="tractce10")

c_tracts <- st_read("../data/cluster_data/chicago_2010_tracts/geo_export_034e8876-472c-4dd0-88b3-4ea893bbd67f.shp")
# drop the ohare tracts
c_tracts <- c_tracts %>% filter(!name10 %in% c("9800","7706.02"))
# make sure it looks correct
#c_tracts %>% st_geometry() %>% plot()
c_tracts <- st_transform(c_tracts, 26916) # convert to NAD83 / UTM zone 16N
c_tracts <- c_tracts %>% st_union()


heat <- read_csv("D:/final_data/chicago_block_heat.csv")
heat$geoid10 <- as.character(heat$geoid10)
heat <- heat %>% select(-blockce10,-block_area,-countyfp10,-name10,-statefp10,-tract_bloc,-tractce10)
heat <- heat %>% distinct(geoid10,.keep_all = T)
aq <- read_csv("D:/final_data/block_airquality.csv")
aq$geoid10 <- as.character(aq$geoid10)
aq <- aq %>% distinct(geoid10,.keep_all = T)

lpa_data <- read_csv("D:/final_data/chicago_lpa_data.csv") %>% select(geoid10) #,impervious_surface,high_veg)
#clust_7 <- readxl::read_xlsx("D:/final_data/lpa_assignments_8-10-22.xlsx",sheet="clust_7a") %>% select(lpa_7=Class)
clust_8 <- readxl::read_xlsx("D:/final_data/lpa_assignments_8-10-22.xlsx",sheet="clust_8g") %>% select(lpa_8=Class)
#clust_9 <- readxl::read_xlsx("D:/final_data/lpa_assignments_8-10-22.xlsx",sheet="clust_9c") %>% select(lpa_9=Class)
lpa_data <- bind_cols(lpa_data,clust_8)
lpa_data$geoid10 <- as.character(lpa_data$geoid10)
lpa_data <- lpa_data %>% distinct(geoid10,.keep_all = T)

tree_height1 <- read_csv("D:/final_data/tree_canopy_height_with0s.csv")
tree_height1 <- tree_height1 %>% filter(!tractce10 %in% c("980000","770602"))
tree_height1 <-tree_height1 %>% select(tract_bloc,canopy_height_with0s)
tree_height1 <- tree_height1 %>% distinct(tract_bloc,.keep_all = TRUE)

tree_height2 <- read_csv("D:/final_data/tree_canopy_height_without0s.csv")
tree_height2 <- tree_height2 %>% filter(!tractce10 %in% c("980000","770602"))
tree_height2 <-tree_height2 %>% select(tract_bloc,canopy_height_no0s)
tree_height2 <- tree_height2 %>% distinct(tract_bloc,.keep_all = TRUE)

tree_binary <- read_csv("D:/final_data/tree_binary.csv")
tree_binary <- tree_binary %>% filter(!tractce10 %in% c("980000","770602"))
tree_binary <-tree_binary %>% select(tract_bloc,tree_binary)
tree_binary <- tree_binary %>% distinct(tract_bloc,.keep_all = TRUE)

block_dems <- read_csv("D:/final_data/chicago_block_demographics.csv")
block_dems <- block_dems %>% distinct(geoid10,.keep_all = TRUE)
block_dems$geoid10 <- as.character(block_dems$geoid10)

nvdi_metrics <- read_csv("D:/final_data/block_ndvi_metrics.csv")
nvdi_metrics <- nvdi_metrics %>% distinct(geoid10,.keep_all = TRUE)
nvdi_metrics$geoid10 <- as.character(nvdi_metrics$geoid10)

chicago <- chicago %>% select(geoid10,block_area,tract_bloc,ca_name)
chicago <- left_join(chicago,aq,by="geoid10")
chicago <- left_join(chicago,heat,by="geoid10")
chicago <- left_join(chicago,lpa_data,by="geoid10")
chicago <- left_join(chicago,nvdi_metrics,by="geoid10")
chicago <- left_join(chicago,tree_height1,by="tract_bloc")
chicago <- left_join(chicago,tree_height2,by="tract_bloc")
chicago <- left_join(chicago,tree_binary,by="tract_bloc")
chicago <- left_join(chicago,block_dems,by="geoid10")
rm(aq,clust_8,lpa_data,heat,tree_height1,tree_height2,tree_binary,block_dems,nvdi_metrics)

# get rid of observations outside my clip
chicago <- st_intersection(chicago,c_tracts)
rm(c_tracts,comm_areas)

chicago <- chicago %>% mutate(
  canopy_height_no0s = ifelse(tree_binary==0,0,canopy_height_no0s)
)
chicago <- chicago %>% filter(is.na(lpa_8)==FALSE) %>% filter(is.na(tree_binary)==FALSE) %>% filter(
  is.na(mean_pm25)==FALSE) %>% filter(is.na(tracts_lst_avg_scaled)==FALSE)


# name the profiles
# naming
# 1.low transit accessibility, high veg single family residential
# 2. parks, water, low pedestrian friendliness
# 3. mixed residential, high veg low transit
# 4. high traffic, high railroads, low veg, high manufacturing/commerical
# 5. highest pop density, high transit accessibility
# 6. lowest vegetation, high business/commercial
# 7. highest veg, high vacant area, some commercial
# 8. residential, multi-fam, high density
chicago$lpa_8 <- as.factor(chicago$lpa_8)
levels(chicago$lpa_8) <- c("Single Family Residential, High Vegetation, Low Accesibility", 
                           "Parks and Water features",
                           "Mixed Residential, Low Accesibility",
                           "High Traffic, Manufacturing, Low Vegetation", 
                           "Highest Density, High Accesibility",
                           "Lowest Vegetation, High Business/Commerical",
                           "Highest Vegetation, Vacant Lots, Some Commercial",
                           "High Density Multi-fam residences")

chicago <- chicago %>% rename(pct_tree_cover=tree_binary,avg_canopy_height=canopy_height_no0s,
                              avg_scaled_lst=tracts_lst_avg_scaled)

# scale a couple of the variables that still require cleaning
chicago <- chicago %>% mutate(
  mean_pm25_scaled = as.numeric(scale(mean_pm25)),
  avg_canopy_height_scaled = as.numeric(scale(avg_canopy_height)),
  pct_tree_cover_sq =sqrt(pct_tree_cover)
)


# test two measures of plantable land
chicago <- chicago %>% mutate(
  plantable_land =high_veg-pct_tree_cover,
  plantable_land_area =(high_veg*block_area)-(pct_tree_cover*block_area),
  
)
# drop some excess variables
#chicago <- chicago %>% select(-starts_with("tracts"),-canopy_height_with0s)


# try and group by block and pick the cluster with the largest representation...
# chicago <- chicago %>% mutate(
#  tree_area= pct_tree_cover*block_area,
#  mean_pm25_raw=scales::rescale(chicago$mean_pm25, to=c(0,1))
# )
# 
# block_clusters <- chicago %>% st_drop_geometry() %>%  
#   group_by(block_group_id) %>% 
#   count(lpa_8) %>% 
#   slice(which.max(n)) %>% 
#   select(-n)
# 
# chicago_block <- chicago %>% group_by(block_group_id) %>%  summarise(
#   tracts_lst_avg = mean(tracts_lst_avg,na.rm=T),
#   tree_area = sum(tree_area,na.rm=T),
#   avg_canopy_height = mean(avg_canopy_height,na.rm=T),
#   mean_pm25 = mean(mean_pm25_raw,na.rm=T),
#   bg_area= sum(block_area, na.rm=T),
#   do_union=T)
# 
# chicago_block <- left_join(chicago_block,block_clusters,by="block_group_id")
# 
# chicago_block <- chicago_block %>% mutate(
#   pct_tree_cover=tree_area/bg_area
# )
# 
# chicago_block <- chicago_block %>% mutate(
#   avg_scaled_lst = as.numeric(scale(tracts_lst_avg)),
#   avg_canopy_height_scaled = as.numeric(scale(avg_canopy_height)),
#   pct_tree_cover = as.numeric(pct_tree_cover )
# )

# exploratory maps
tmap::tm_shape(chicago) +
  tmap::tm_fill("pct_tree_cover",style = "cont")

tmap::tm_shape(chicago) +
  tmap::tm_fill("lpa_8",style = "cat",palette = "Set3") +
  tmap::tm_layout(legend.text.size = .6,
                  legend.outside = T)



################################################################################
# Put together some descriptives
summary(chicago$avg_canopy_height_scaled)
summary(chicago$pct_tree_cover)
chicago %>% st_drop_geometry() %>% group_by(lpa_8) %>%  summarise(
  across(all_of(c("avg_canopy_height_scaled","pct_tree_cover","avg_scaled_lst","mean_pm25_scaled")), 
         list(mean = ~ mean(.x, na.rm = T), sd =  ~ sd(.x, na.rm = T), n=~ n())))

chicago %>% st_drop_geometry() %>% group_by(lpa_8) %>%  summarise(
  across(all_of(c("avg_scaled_lst","mean_pm25_scaled")), 
         list(mean = ~ mean(.x, na.rm = T), sd =  ~ sd(.x, na.rm = T), n=~ n())))

################################################################################
# Regression models
# start with ols, look at spatial autocorrelation

# Good paper example on how to write up show results
#https://academic.oup.com/jue/article/2/1/juw006/2875730#53974559



# outcome_vars: mean_pm25,tracts_lst_avg_scaled
corrplot::corrplot(cor(chicago %>% st_drop_geometry() %>% select(
  avg_canopy_height_scaled,mean_pm25_scaled,pct_tree_cover,avg_scaled_lst,
  pct_below_pov,pct_white_alone,pct_black_alone,pct_asian_alone,pct_latino),use = "complete.obs"))

# car::symbox(~pct_tree_cover,
#        chicago,
#        na.rm=T,
#        power=seq(-3,3,by=.5))

model0_aq <- chicago %>%
  lm(mean_pm25_scaled ~
       lpa_8,
     data=.)
summary(model0_aq)
broom::glance(model0_aq)


model1_aq <- chicago %>%
  lm(mean_pm25_scaled ~
       pct_tree_cover,
     data=.)
summary(model1_aq)
broom::glance(model1_aq)


model2_aq <- chicago %>%
  lm(mean_pm25_scaled ~
       avg_canopy_height_scaled,
     data=.)
summary(model2_aq)
broom::glance(model2_aq)

model3_aq <- chicago %>%
  lm(mean_pm25_scaled ~
       lpa_8*pct_tree_cover,
     data=.)
summary(model3_aq)
broom::glance(model3_aq)

model4_aq <- chicago %>%
  lm(mean_pm25_scaled ~
       lpa_8*avg_canopy_height_scaled,
     data=.)
summary(model4_aq)
broom::glance(model4_aq)

model5_aq <- chicago %>%
  lm(mean_pm25_scaled ~
       lpa_8*pct_tree_cover+lpa_8*avg_canopy_height_scaled,
     data=.)
summary(model5_aq)
broom::glance(model5_aq)

model6_aq <- chicago %>%
  lm(mean_pm25_scaled ~
       lpa_8*pct_tree_cover*avg_canopy_height_scaled,
     data=.)
summary(model6_aq)
broom::glance(model6_aq)

model7_aq <- chicago %>%
  lm(mean_pm25_scaled ~
       pct_tree_cover*avg_canopy_height_scaled,
     data=.)
summary(model7_aq)
broom::glance(model7_aq)


sjPlot::plot_model(model3_aq,type="pred",terms = c("pct_tree_cover","lpa_8"))
sjPlot::plot_model(model3_aq,type="pred",terms = c("lpa_8","pct_tree_cover [.025,.05,.075,.1]"))

sjPlot::plot_model(model4_aq,type="pred",terms = c("avg_canopy_height_scaled","lpa_8"))
sjPlot::plot_model(model4_aq,type="pred",terms = c("lpa_8","avg_canopy_height_scaled"))

sjPlot::plot_model(model5_aq,type="pred",terms = c("avg_canopy_height_scaled","lpa_8"))
sjPlot::plot_model(model5_aq,type="pred",terms = c("pct_tree_cover","lpa_8"))
sjPlot::plot_model(model5_aq,type="pred",terms = c("pct_tree_cover","avg_canopy_height_scaled","lpa_8"))

sjPlot::plot_model(model6_aq,type="pred",terms = c("pct_tree_cover","avg_canopy_height_scaled [-2, -1,-.5,0,.5,1,2]","lpa_8"))
sjPlot::plot_model(model7_aq,type="pred",terms = c("pct_tree_cover","avg_canopy_height_scaled [-2, -1,-.5,0,.5,1,2]"))


model0_lst <- chicago %>%
  lm(avg_scaled_lst ~
       lpa_8,
     data=.)
summary(model0_lst)
broom::glance(model0_lst)


model1_lst <- chicago %>%
  lm(avg_scaled_lst ~
       pct_tree_cover,
     data=.)
summary(model1_lst)
broom::glance(model1_lst)


model2_lst <- chicago %>%
  lm(avg_scaled_lst ~
       avg_canopy_height_scaled,
     data=.)
summary(model2_lst)
broom::glance(model2_lst)

model3_lst <- chicago %>%
  lm(avg_scaled_lst ~
       lpa_8*pct_tree_cover,
     data=.)
summary(model3_lst)
broom::glance(model3_lst)

model4_lst <- chicago %>%
  lm(avg_scaled_lst ~
       lpa_8*avg_canopy_height_scaled,
     data=.)
summary(model4_lst)
broom::glance(model4_lst)

model5_lst <- chicago %>%
  lm(avg_scaled_lst ~
       lpa_8*pct_tree_cover+lpa_8*avg_canopy_height_scaled,
     data=.)
summary(model5_lst)
broom::glance(model5_lst)

model6_lst <- chicago %>%
  lm(avg_scaled_lst ~
       lpa_8*pct_tree_cover*avg_canopy_height_scaled,
     data=.)
summary(model6_lst)
broom::glance(model6_lst)

sjPlot::plot_model(model3_lst,type="pred",terms = c("pct_tree_cover","lpa_8"))#,axis.lim=list(c(0,.5),c(-5,3)))

sjPlot::plot_model(model4_lst,type="pred",terms = c("avg_canopy_height_scaled","lpa_8"))

sjPlot::plot_model(model5_lst,type="pred",terms = c("avg_canopy_height_scaled","lpa_8"))
sjPlot::plot_model(model5_lst,type="pred",terms = c("pct_tree_cover","lpa_8"))
sjPlot::plot_model(model5_lst,type="pred",terms = c("pct_tree_cover","avg_canopy_height_scaled","lpa_8"))

sjPlot::plot_model(model6_lst,type="pred",terms = c("pct_tree_cover","avg_canopy_height_scaled [-2,-1,-.5,0,.5,1,2]","lpa_8"))

# check for spatial autorcorrelation
model6_lst_data <- model6_aq %>% broom::augment() 

chicago <- chicago %>% mutate(
  model_residuals = residuals(model6_aq)
)
 
# definitely have spatial autocorrelation
tmap::tm_shape(chicago) +
  tmap::tm_polygons("model_residuals",palette = "RdYlBu",border.alpha = 0) +
  tmap::tm_layout(legend.text.size = .6,
                  legend.outside = T)


chicago %>%
  tidypredict::tidypredict_to_column(model6_aq) %>% st_drop_geometry %>% select(fit) %>% head()

# Moran's I value: 0.956
# coordsW <- chicago%>%
#   st_centroid()%>%
#   st_geometry()
# 
# chicago_nb <- chicago %>%
#   spdep::poly2nb(., queen=T)
# 
# # Queens matrix
# chicao.queens_weight <- chicago_nb %>%
#   spdep::nb2listw(., style="W", zero.policy = T)
# 
# 
# Queen <- chicago %>%
#   st_drop_geometry()%>%
#   dplyr::select(model_residuals)%>%
#   pull()%>%
#   spdep::moran.test(., chicao.queens_weight,zero.policy = T)%>%
#   broom::tidy()

# manually compute marginal predictions 
# p <- predict(object = model1,
#              newdata = chicago,
#              type = "response",
#              se.fit = TRUE)
# mult <- qnorm(0.5*(1-0.95))
# out <- cbind(p$fit,
#              p$se.fit,
#              p$fit+p$se.fit*mult,
#              p$fit-p$se.fit*mult)
# rownames(out) <- levels(chicago$lpa_9)[chicago$lpa_9]
# colnames(out) <- c("margin", "Std.Err", "lower 95% conf", "upper 95% conf")


#If this occurs, then we have 'non-stationarity' - this is when the global model does not
#represent the relationships between variables that might vary locally.

# split the extent into thirds, see if that works
# > st_bbox(chicago)
# xmin      ymin      xmax      ymax 
# 428578.1 4610424.0  456468.9 4652555.7 

# maybe try ninths?
chicago_top_third <- chicago %>% st_crop(.,
                                         xmin=428578.1,
                                         xmax=(456468.9-9296.933-9296.933),
                                         ymin=(4610424.0+14043.9+14043.9),
                                         ymax=4652555.7)

chicago_bot_third <- chicago %>% st_crop(.,
                                         xmin=428578.1,
                                         xmax=456468.9,
                                         ymin=(4610424.0+14043.9),
                                         ymax=(4652555.7-14043.9))

chicago_mid_third <- chicago %>% st_crop(.,
                                         xmin=428578.1,
                                         xmax=456468.9,
                                         ymin=4610424.0,
                                         ymax=(4652555.7-14043.9-14043.9))



# need to make some assumptions about trees to run simulations
coordsW <- chicago_top_third %>%
  st_centroid()%>%
  st_geometry()

coordsW2 <- st_coordinates(coordsW)

chicago_top_third <- cbind(chicago_top_third,coordsW2)

# chicago_write <- chicago %>% st_drop_geometry() %>% select(
#   geoid10,avg_scaled_lst,avg_canopy_height_scaled,pct_tree_cover,mean_pm25_scaled,lpa_8,X,Y)
# 
# levels(chicago_write$lpa_8) <- c("g1","g2","g3","g4","g5","g6","g7","g8")
# write.csv(chicago_write,"D:/final_data/chicago_gwr_data.csv",row.names = F)
# chicago_nb <- chicago %>%
#   spdep::poly2nb(., queen=T)
# 
chicago_knn <- coordsW2 %>%
  spdep::knearneigh(., k=500)

chicago_knn<- chicago_knn %>% spdep::knn2nb()

chicago_knn_weight <- chicago_knn %>% spdep::nb2listw(., style="C", zero.policy = T)
# Queens matrix
# chicao.queens_weight <- chicago_nb %>%
#   spdep::nb2listw(., style="C", zero.policy = T)



################################################################################
# run a spatial lag model - these are a bit sketchy, too large to do properly
# 
# spat_mod_lst_1<- spatialreg::lagsarlm(avg_scaled_lst ~
#                                         lpa_8*pct_tree_cover*avg_canopy_height_scaled,
#                                       data = chicago_top_third,
#                                       spdep::nb2listw(chicago_nb,style="C"),
#                                       method = "eigen",
#                                       zero.policy = T)
# summary(spat_mod_lst_1)
# 
# # extract the residuals for modelSLY object and dump back to original sf spatialdatafile object
# chicago$RESID_lag <- spat_mod_lst_1$residuals
# # use Moran's I test using moran.mc() function
# spdep::moran.mc(chicago$RESID_lag, spdep::nb2listw(chicago_nb,style="W"), 1000, zero.policy = T)
# 
# # still spatial autocorrelation but much less worse
# # statistic = 0.03044, observed rank = 1001, p-value = 0.000999
# # alternative hypothesis: greater
# 
# # generate the residual map
# tmap::tm_shape(chicago) + 
#   tmap::tm_fill("RESID_lag", style = "cont", midpoint = 0, palette = "-RdBu",
#           title='Spatial Lag Resiudals') +
#   # tm_shape(chicago_tracts) + 
#   # tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
#   tmap::tm_compass(position = c("right","top"),size=.8,text.size = .8) +
#   tmap::tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
#   tmap::tm_layout(frame=FALSE, legend.outside = TRUE,legend.text.size = .5,legend.title.size = .8,
#             inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)
# 
# 
# # Interpretation of results using impacts
# # impacts
# Weights_2.0 <- as(spdep::nb2listw(chicago_nb,style="W"), "CsparseMatrix")
# trMC <- spatialreg::trW(Weights_2.0, type="MC") # or moments
# summary(spatialreg::impacts(spat_mod_lst_1, tr = trMC, R=100), zstats=TRUE)

################################################################################
# run a spatial error  model
start_time <- Sys.time()
spat_mod_lst_1<- spatialreg::errorsarlm(avg_scaled_lst ~
                                          lpa_8*pct_tree_cover*avg_canopy_height_scaled,
                                        data = chicago,
                                       chicago_knn_weight,
                                        method = "MC",
                                        zero.policy = T)
end_time <- Sys.time()
loop_time <- end_time - start_time

summary(spat_mod_lst_1)

# extract the residuals for modelSLY object and dump back to original sf spatialdatafile object
chicago$RESID_lag <- spat_mod_lst_1$residuals
# use Moran's I test using moran.mc() function
spdep::moran.mc(chicago$RESID_lag, chicago_knn_weight, 1000, zero.policy = T)

# little remaining spatial autocorrelation
#statistic = 0.084595, observed rank = 1001, p-value = 0.000999
#alternative hypothesis: greater

# generate the residual map
tmap::tm_shape(chicago) + 
  tmap::tm_fill("RESID_lag", style = "cont", midpoint = 0, palette = "-RdBu",
                title='Spatial Lag Resiudals') +
  # tm_shape(chicago_tracts) + 
  # tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tmap::tm_compass(position = c("right","top"),size=.8,text.size = .8) +
  tmap::tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tmap::tm_layout(frame=FALSE, legend.outside = TRUE,legend.text.size = .5,legend.title.size = .8,
                  inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)



sjPlot::plot_model(spat_mod_aq_1$fitted.values,type="pred",terms = c("pct_tree_cover","avg_canopy_height_scaled [-2,-1,-.5,0,.5,1,2]","lpa_8"))

# create a new data frame with the values we want to predict
predicted_data <- data.frame(lpa_8=rep(levels(chicago$lpa_8),88),
                  pct_tree_cover=rep(rep(c(0,.5,.1,.15,.2,.25,.3,.35,.4,.45,.5),each=8),8), 
                  avg_canopy_height_scaled=rep(c(-2,-1,-.5,0,.5,1,1.5,2),each=88))


predicted_results <- as.data.frame(predict(spat_mod_lst_1,newdata=predicted_data,se.fit=T))

predicted_data <- cbind(predicted_data,predicted_results)

ggplot(predicted_data, aes(x=pct_tree_cover, y=fit, colour=as.factor(avg_canopy_height_scaled))) + 
  geom_line() +
  geom_smooth() +
  facet_wrap(. ~ lpa_8) +
  ggtitle("IVContinuousA vs. DV, by Level of IVContinousB") +
  labs(colour="")
###############################################################################
# run a spatial error  model
start_time <- Sys.time()
spat_mod_aq_1<- spatialreg::errorsarlm(mean_pm25_scaled ~
                                         lpa_8*pct_tree_cover*avg_canopy_height_scaled,
                                       data = chicago,
                                       (spdep::nb2listw(chicago_knn, style="W", zero.policy = T)),
                                       method = "LU",
                                       zero.policy = T)
end_time <- Sys.time()
loop_time <- end_time - start_time

summary(spat_mod_aq_1)

# extract the residuals for modelSLY object and dump back to original sf spatialdatafile object
chicago$RESID_lag <- spat_mod_aq_1$residuals
# use Moran's I test using moran.mc() function
spdep::moran.mc(chicago$RESID_lag, chicago_knn_weight, 1000, zero.policy = T)

# little remaining spatial autocorrelation
# statistic = 0.058516, observed rank = 1001, p-value = 0.000999
# alternative hypothesis: greate

# generate the residual map
tmap::tm_shape(chicago) + 
  tmap::tm_fill("RESID_lag", style = "cont", midpoint = 0, palette = "-RdBu",
                title='Spatial Lag Resiudals') +
  # tm_shape(chicago_tracts) + 
  # tm_polygons(alpha = 0, border.alpha = 1, border.col = "black") +
  tmap::tm_compass(position = c("right","top"),size=.8,text.size = .8) +
  tmap::tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tmap::tm_layout(frame=FALSE, legend.outside = TRUE,legend.text.size = .5,legend.title.size = .8,
                  inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)



sjPlot::plot_model(spat_mod_aq_1$fitted.values,type="pred",terms = c("pct_tree_cover","avg_canopy_height_scaled [-2,-1,-.5,0,.5,1,2]","lpa_8"))

# create a new data frame with the values we want to predict
predicted_data <- data.frame(lpa_8=rep(levels(chicago$lpa_8),88),
                             pct_tree_cover=rep(rep(c(0,.5,.1,.15,.2,.25,.3,.35,.4,.45,.5),each=8),8), 
                             avg_canopy_height_scaled=rep(c(-2,-1,-.5,0,.5,1,1.5,2),each=88))


predicted_results <- as.data.frame(predict(spat_mod_aq_1,newdata=predicted_data,se.fit=T))

predicted_data <- cbind(predicted_data,predicted_results)

ggplot(predicted_data, aes(x=pct_tree_cover, y=fit, colour=as.factor(avg_canopy_height_scaled))) + 
  geom_line() +
  geom_smooth() +
  facet_wrap(. ~ lpa_8) +
  ggtitle("IVContinuousA vs. DV, by Level of IVContinousB") +
  labs(colour="")

################################################################################
# display coefficient results from all models
stargazer::stargazer(model6_aq, spat_mod_aq_1,type = "text", # type="html",
                     title="Title: Regression Results")

# plot the residuals
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,3)))
print(resid_map1, vp=viewport(layout.pos.col = 1,layout.pos.row = 1))
print(resid_map2, vp=viewport(layout.pos.col = 2,layout.pos.row = 1))
print(resid_map3, vp=viewport(layout.pos.col = 3,layout.pos.row = 1))

# export map

# only going to interpret results of the spatial lag model... need impacts
summary(impacts(model1_lag, tr = trMC, R=100), zstats=TRUE)


################################################################################
# GWR seems most relevant, however the vector space is too large, would need to use
# block groups or tracts

# # run once to get the bandwidth that makes most sense for this data
# # TAKES A LONG TIME TO RUN!

chicago_top_third <- as(chicago_top_third, "Spatial")

# GWRbandwidth <- gwr.sel(avg_scaled_lst ~ lpa_8*pct_tree_cover*avg_canopy_height_scaled,
#                         data = chicago_top_third,
#                         coords=cbind(chicago_top_third$X, chicago_top_third$Y),
#                         adapt=T)

# GWRbandwidth2 <- gwr.sel(mean_pm25_scaled ~ lpa_8*pct_tree_cover*avg_canopy_height_scaled,
#                         data = chicago, 
#                         coords=cbind(chicago$X, chicago$Y),
#                         adapt=T)
# 
# # results pulled from the singular run above
# GWRbandwidth <-0.01287558
# GWRbandwidth2 <-0.01287558
# 
# 
# # The optimal bandwidth is about 0.015 meaning 1.5% of all the total spatial units should be used for the local regression 
# # based on k-nearest neighbours. Which is about 9 of the 626 wards.
# 

coords=cbind(chicago_top_third$X, chicago_top_third$Y)
chicago_top_third <- chicago_top_third %>% st_drop_geometry()
coordinates(chicago_top_third) <- ~ X + Y

start_time <- Sys.time()
gwr_model_lst <- gwr(avg_scaled_lst ~ lpa_8*pct_tree_cover*avg_canopy_height_scaled,
                     data = chicago_top_third,
                     coords=cbind(chicago_top_third$X, chicago_top_third$Y),
                     adapt=0.01287558, 
                     hatmatrix=T,
                     se.fit=T,
                     predictions=TRUE)
end_time <- Sys.time()
loop_time <- end_time - start_time

# Time difference of 2.049124 hours

results <- as.data.frame(gwr_model_lst$SDF)  
results <- results %>% select(-contains("_se"))
chicago_top_third$pct_tree_cover_coef <- results$pct_tree_cover

tmap::tm_shape(chicago_top_third) +
  tmap::tm_dots("pct_tree_cover_coef",style = "cont")

# start_time <- Sys.time()
# gwr.pred<-GWmodel::gwr.predict(avg_scaled_lst ~ pct_tree_cover,
#                       data = chicago_top_third, bw=5,kernel = "gaussian")
# end_time <- Sys.time()
# loop_time <- end_time - start_time



start_time <- Sys.time()
gwr_model_lst <- GWmodel::gwr.basic(avg_scaled_lst ~ pct_tree_cover,
                             chicago_top_third, bw = 0.01287558, adaptive = F, parallel.method = F)#, parallel.arg = 1)
end_time <- Sys.time()
loop_time <- end_time - start_time

# 
# gwr_model_aq <- gwr(mean_pm25_scaled ~ lpa_8*pct_tree_cover*avg_canopy_height_scaled,
#                      data = chicago, 
#                      coords=cbind(chicago$X, chicago$Y),
#                      adapt=0.0002165721, # 10 closest blocks
#                      hatmatrix=T,
#                      se.fit=T,
#                     predictions=TRUE
#                     )
# 
# results <- as.data.frame(gwr_model_lst$SDF)
################################################################################
# Equity
cor.test(chicago$pct_tree_cover, chicago$pct_below_pov)
summary(chicago %>%lm(pct_tree_cover ~ pct_black_alone, data=.)) # slight positive correlation
summary(chicago %>%lm(pct_tree_cover ~ pct_latino, data=.)) # slight negative correlation
summary(chicago %>%lm(pct_tree_cover ~ pct_below_pov, data=.)) # slight positive correlation
summary(chicago %>%lm(pct_tree_cover ~ pct_black_alone + pct_latino + pct_asian_alone +pct_white_alone, data=.))

summary(chicago %>% lm(pct_black_alone ~ lpa_8, data=.))

t.test(chicago$pct_tree_cover, mu = 50)

hist(chicago$pct_tree_cover)

boxplot(pct_tree_cover~lpa_8,data=chicago)
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
chicago %>% st_drop_geometry() %>% group_by(primary_racial_group) %>% summarise(
  utc = mean(pct_tree_cover)
)
################################################################################
# To minimize
chicago <- chicago %>% mutate(
  pct_plantable_land_planted = tree_area/plantable_land_area
)
DescTools::Gini(chicago$pct_tree_cover)

DescTools::Gini(chicago$tracts_lst_avg)
DescTools::Gini(chicago$mean_pm25_raw) # dont really care about equity...


# create simulations such that temperature. aq, and pct_tree equity are minimized
chicago$plantable_land


# # need to calculate the maximum urban tree canopy coverage for each block
# > summary(chicago$plantable_land)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.43919 -0.04508 -0.01296  0.07110  0.09221  1.00000 

# > summary(chicago$plantable_land_area)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -17945.6    -349.4     -85.8    1845.8     831.6 2445718.6

# can calculate the maximum number of trees to be added to an area
#each tree = (3.141453) *( 3 ^2)

# 4 maps... if we just wanted to target individual vars, vs a method for combining all 3

##### 1 - Equity #####
# starting with the blocks with the smallest pct_tree cover, target the blocks that
# have plantable land



##### 2 - LST #####
##### 3 - AQ #####
##### 4 - ALL #####


#################################################################################
# Run all of the GWR models -8/20/22, add 2% in each direction i can, reduce points to 250

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
# 
# write.csv(all_chicago_lst_gwr,"D:/final_data/gwr_lst_results_interaction2.csv",row.names = F)
 

###################################################################################
lst_gwr <- read_csv("D:/final_data/gwr_lst_results_interaction2.csv")
aq_gwr <- read_csv("D:/final_data/gwr_aq_results_interaction2.csv")

lst_gwr <- lst_gwr %>% select(geoid10,lst_xintercept=X.Intercept.,lst_utc_coef=pct_tree_cover,
                              lst_ach_coef=avg_canopy_height_scaled,
                              lst_interaction=pct_tree_cover.avg_canopy_height_scaled,
                              lst_pred=pred)
lst_gwr$geoid10 <- as.character(lst_gwr$geoid10)
aq_gwr <- aq_gwr %>% select(geoid10,aq_xintercept=X.Intercept.,aq_utc_coef=pct_tree_cover,
                              aq_ach_coef=avg_canopy_height_scaled,
                              aq_interaction=pct_tree_cover.avg_canopy_height_scaled,
                              aq_pred=pred)
aq_gwr$geoid10 <- as.character(aq_gwr$geoid10)
# aq_gwr <- aq_gwr %>% select(-geoid10)
# gwr <- bind_cols(lst_gwr,aq_gwr)

chicago <- left_join(chicago,lst_gwr,by="geoid10")
chicago <- left_join(chicago,aq_gwr,by="geoid10")

tmap::tm_shape(chicago) +
  tmap::tm_fill("lst_pred",style = "cont")


### AQ
aq_prediction <- chicago %>% st_drop_geometry() %>% select(geoid10,lpa_8,starts_with("aq"))

tree_cover_vals <- c(0,.025,.05,.075,.1,.2,.3)
canopy_height_vals <- c(-2,-1,0,1,2,3)

for(i in tree_cover_vals){
  for(v in canopy_height_vals){
    print(paste0(i," + ", v))
    aq_prediction <- aq_prediction %>% mutate(
      "aqval_{i}_{v}" := aq_xintercept + aq_utc_coef*i + aq_ach_coef*v + aq_interaction*v*i
    )
  }
}

aq_prediction_short <- aq_prediction %>% select(
  -geoid10,-aq_xintercept,-aq_utc_coef,-aq_ach_coef,-aq_pred,-aq_interaction) %>% group_by(lpa_8) %>% summarise(
    across(everything(),list(mean=mean, sd=sd,n=~ n()))
)

aq_prediction_short <- aq_prediction_short %>% pivot_longer(
  cols = -c(lpa_8),
  names_to = c("var","pct_tree_cover","avg_canopy_height_scaled","z"),
  names_sep = "_") %>% select(-var)

aq_prediction_short$pct_tree_cover <- as.numeric(aq_prediction_short$pct_tree_cover)
aq_prediction_short$avg_canopy_height_scaled <- as.numeric(aq_prediction_short$avg_canopy_height_scaled)

aq_prediction_short <- aq_prediction_short %>% pivot_wider(
  names_from = z,
  values_from = value) 

aq_prediction_short <- aq_prediction_short %>% mutate(
  mymin=mean - (1.96*(sd/(n^.5))),
  mymax=mean + (1.96*(sd/(n^.5)))
)


ggplot(aq_prediction_short, aes(x=pct_tree_cover, y=mean, colour=as.factor(avg_canopy_height_scaled))) + 
  geom_line(lwd = 1) +
  # geom_ribbon(aes(ymin=mymin,ymax=mymax,fill=as.factor(avg_canopy_height_scaled)),alpha=.1) +
  # scale_colour_manual(values = c("black", "red", "magenta", "green","blue","grey")) +
  # scale_fill_manual(values = c("black", "red", "magenta", "green","blue","grey")) +
  facet_wrap(. ~ lpa_8) +
  ggtitle("Air Quality") +
  labs(colour="")

### LST
lst_prediction <- chicago %>% st_drop_geometry() %>% select(geoid10,lpa_8,starts_with("lst"))

tree_cover_vals <- c(0,.025,.05,.075,.1,.2,.3)
canopy_height_vals <- c(-2,-1,0,1,2,3)

for(i in tree_cover_vals){
  for(v in canopy_height_vals){
    print(paste0(i," + ", v))
    lst_prediction <- lst_prediction %>% mutate(
      "lstval_{i}_{v}" := lst_xintercept + lst_utc_coef*i + lst_ach_coef*v + lst_interaction*v*i
    )
  }
}

lst_prediction_short <- lst_prediction %>% select(
  -geoid10,-lst_xintercept,-lst_utc_coef,-lst_ach_coef,-lst_pred,-lst_interaction) %>% group_by(lpa_8) %>% summarise(
    across(everything(),list(mean))
  )

lst_prediction_short <- lst_prediction_short %>% pivot_longer(
  cols = -c(lpa_8),
  names_to = c("var","pct_tree_cover","avg_canopy_height_scaled","idk"),
  names_sep = "_") %>% select(-idk,-var)

lst_prediction_short$pct_tree_cover <- as.numeric(lst_prediction_short$pct_tree_cover)
lst_prediction_short$avg_canopy_height_scaled <- as.numeric(lst_prediction_short$avg_canopy_height_scaled)


ggplot(lst_prediction_short, aes(x=pct_tree_cover, y=value, colour=as.factor(avg_canopy_height_scaled))) + 
  geom_line() +
  geom_smooth() +
  facet_wrap(. ~ lpa_8) +
  ggtitle("Land Surface Temperature") +
  labs(colour="")

rm(aq_prediction_short,lst_prediction_short,aq_prediction,lst_prediction,aq_gwr,lst_gwr,gwr)

################################################################################
tree_area = units::set_units(pi*9, m^2)
chicago <- chicago %>% mutate(
  number_potential_trees = as.numeric(floor(plantable_land_area/tree_area)),
  current_tree_area = pct_tree_cover*block_area
)
################################################################################
# Equity (True Equity)
################################################################################
# add trees to the communities with the lowest pct_tree_cover that have room for trees

# Want to minimize this value
DescTools::Gini(chicago$pct_tree_cover)

#sum(chicago$number_potential_trees[which(chicago$number_potential_trees>0)])
# 3,364,319 estimated room for number of trees
#test <- chicago %>% st_drop_geometry() %>% select(geoid10,pct_tree_cover,number_potential_trees)
test <- chicago %>% st_drop_geometry() %>% select(geoid10,pct_tree_cover,number_potential_trees)
test <- test[order(test$pct_tree_cover),]
#test <- test[order(test$pct_below_pov),]
test$number_potential_trees<- ifelse(test$number_potential_trees<0,0,test$number_potential_trees)
test$added_trees=0
trees=75000
i=1
while(trees>0){
  test[i,"added_trees"] <- test[i,"number_potential_trees"]
  test[i,"added_trees"] <-if_else(test[i,"added_trees"]>11,11,test[i,"added_trees"])
  trees=trees-test[i,"added_trees"]
  i=i+1
}
test <- test %>% select(geoid10,added_trees_equity=added_trees)
chicago <- left_join(chicago,test,by="geoid10")
chicago <- chicago %>% mutate(
  new_pct_tree_cover=as.numeric(((added_trees_equity*tree_area)+current_tree_area)/block_area)
)
DescTools::Gini(chicago$pct_tree_cover)
DescTools::Gini(chicago$new_pct_tree_cover) # 11 =0.3993328
chicago <- chicago %>% select(-added_trees_equity)



comm_area <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_equity = sum(added_trees_equity,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)


#chicago <- chicago %>% select(-contains("added_trees"))
tmap::tm_shape(comm_area) +
  tmap::tm_fill("added_trees",style = "cont") +
  tm_text("ca_name",size=.4)



################################################################################
# Equity (Underresourced Communities)
################################################################################
# > summary(aov(chicago$pct_tree_cover ~ chicago$primary_racial_group))
# 
# Df Sum Sq Mean Sq F value Pr(>F)    
# chicago$primary_racial_group     4   1.00 0.25022   139.8 <2e-16 ***
#   Residuals                    46144  82.57 0.00179                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# summary(chicago %>%lm(pct_tree_cover ~ primary_racial_group, data=.)) # slight positive correlation
# 
# Call:
#   lm(formula = pct_tree_cover ~ primary_racial_group, data = .)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.05908 -0.03091 -0.00565  0.02243  0.45818 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         0.026567   0.002014   13.19   <2e-16 ***
#   primary_racial_groupblack           0.032514   0.002040   15.94   <2e-16 ***
#   primary_racial_grouplatino          0.022603   0.002057   10.99   <2e-16 ***
#   primary_racial_groupno primary race 0.027744   0.002102   13.20   <2e-16 ***
#   primary_racial_groupwhite           0.029082   0.002048   14.20   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0423 on 46144 degrees of freedom
# Multiple R-squared:  0.01198,	Adjusted R-squared:  0.01189 
# F-statistic: 139.8 on 4 and 46144 DF,  p-value: < 2.2e-16
# 
# 
# Pairwise comparisons using t tests with pooled SD 
# 
# data:  chicago$pct_tree_cover and as.factor(chicago$primary_racial_group) 
# 
# asian   black   latino  no primary race
# black           < 2e-16 -       -       -              
#   latino          < 2e-16 < 2e-16 -       -              
#   no primary race < 2e-16 2.7e-11 2.5e-11 -              
#   white           < 2e-16 2.2e-11 < 2e-16 0.58           
# 
# P value adjustment method: bonferroni 
# 
# # A tibble: 5 x 2
# primary_racial_group    utc
# <chr>                 <dbl>
#   1 asian                0.0266
# 2 black                0.0591
# 3 latino               0.0492
# 4 no primary race      0.0543
# 5 white                0.0556




################################################################################
################################# Air Quality ##################################
################################################################################
mean(chicago$mean_pm25)
#sum(chicago$number_potential_trees[which(chicago$number_potential_trees>0)])
# 3,364,319 estimated room for number of trees
test <- chicago %>% st_drop_geometry() %>% select(
  geoid10,mean_pm25,pct_tree_cover,number_potential_trees,aq_xintercept,aq_utc_coef,
  aq_ach_coef,aq_interaction,aq_pred,current_tree_area,block_area,avg_canopy_height_scaled)

test <- test[order(test$aq_utc_coef),]
#test <- test[order(test$mean_pm25,decreasing = T),]
test$number_potential_trees<- ifelse(test$number_potential_trees<0,0,test$number_potential_trees)
test$added_trees=0
trees=75000
i=1
while(trees>0){
  test[i,"added_trees"] <- test[i,"number_potential_trees"]
# test[i,"added_trees"] <- if_else(test[i,"aq_utc_coef"]< 0, test[i,"number_potential_trees"],0) # -0.02845771; adding max values to the highest value areas (158 max)
  test[i,"added_trees"] <-if_else(test[i,"added_trees"]>142,142,test[i,"added_trees"])
  trees=trees-test[i,"added_trees"]
  i=i+1
}
test <- test %>% mutate(
  new_pct_tree_cover=as.numeric(((added_trees*tree_area)+current_tree_area)/block_area)
)
test <- test %>% mutate(
  predicted_aq = aq_xintercept + aq_utc_coef*(new_pct_tree_cover) + aq_ach_coef*(avg_canopy_height_scaled) + (aq_interaction*new_pct_tree_cover*avg_canopy_height_scaled),
  predicted_aq = if_else(predicted_aq < -4.08269, -4.08269, predicted_aq) # don't let values go below min possible
)
mean(test$aq_pred)
mean(test$predicted_aq) # -0.02962394; adding max values to the highest value areas (142 max)



test <- test %>% select(geoid10,added_trees_aq=added_trees)

chicago <- left_join(chicago,test,by="geoid10")


comm_area <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_aq = sum(added_trees_aq,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)


#chicago <- chicago %>% select(-contains("added_trees"))
tmap::tm_shape(comm_area) +
  tmap::tm_fill("added_trees_aq",style = "cont") +
  tm_text("ca_name",size=.4)

################################################################################
# UHI
################################################################################
mean(chicago$avg_scaled_lst)
#sum(chicago$number_potential_trees[which(chicago$number_potential_trees>0)])
# 3,364,319 estimated room for number of trees
test <- chicago %>% st_drop_geometry() %>% select(
  geoid10,avg_scaled_lst,pct_tree_cover,number_potential_trees,lst_xintercept,lst_utc_coef,
  lst_ach_coef,lst_interaction,lst_pred,current_tree_area,block_area,avg_canopy_height_scaled)

test <- test[order(test$lst_utc_coef),]
#test <- test[order(test$avg_scaled_lst,decreasing = T),]
test$number_potential_trees<- ifelse(test$number_potential_trees<0,0,test$number_potential_trees)
test$added_trees=0
trees=75000
i=1
while(trees>0){
#  test[i,"added_trees"] <- test[i,"number_potential_trees"]
    test[i,"added_trees"] <- if_else(test[i,"lst_utc_coef"]< 0, test[i,"number_potential_trees"],0) # 0.1256661; adding max values to the highest value areas (66 max)
  test[i,"added_trees"] <-if_else(test[i,"added_trees"]>38,38,test[i,"added_trees"])
  trees=trees-test[i,"added_trees"]
  i=i+1
}
test <- test %>% mutate(
  new_pct_tree_cover=as.numeric(((added_trees*tree_area)+current_tree_area)/block_area)
)
test <- test %>% mutate(
  predicted_lst = lst_xintercept + lst_utc_coef*(new_pct_tree_cover) + lst_ach_coef*(avg_canopy_height_scaled) + (lst_interaction*new_pct_tree_cover*avg_canopy_height_scaled),
  predicted_lst = if_else(predicted_lst < -3.6470, -3.6470, predicted_lst) # don't let values go below min possible
)
mean(test$lst_pred)
mean(test$predicted_lst) # 0.1253389; adding max values to the highest value areas (38 max)



#########
test <- test %>% select(geoid10,added_trees_lst=added_trees)

chicago <- left_join(chicago,test,by="geoid10")


comm_area <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_lst = sum(added_trees_lst,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)


#chicago <- chicago %>% select(-contains("added_trees"))
tmap::tm_shape(comm_area) +
  tmap::tm_fill("added_trees_lst",style = "cont") +
  tm_text("ca_name",size=.4)

################################################################################
# Combo
################################################################################
test <- chicago %>% st_drop_geometry() %>% select(
  geoid10,avg_scaled_lst,pct_tree_cover,number_potential_trees,lst_xintercept,lst_utc_coef,
  lst_ach_coef,lst_interaction,lst_pred,current_tree_area,block_area,avg_canopy_height_scaled,
  mean_pm25,aq_xintercept,aq_utc_coef,aq_ach_coef,aq_interaction,aq_pred)
test$number_potential_trees<- ifelse(test$number_potential_trees<0,0,test$number_potential_trees)
test <- test %>% mutate(
  total_lst_effect = lst_utc_coef + lst_ach_coef + lst_interaction,
  total_aq_effect = aq_utc_coef + aq_ach_coef + aq_interaction,
  total_tree_effect = lst_utc_coef + aq_utc_coef #lst_ach_coef + lst_interaction+aq_utc_coef + aq_ach_coef + aq_interaction,
  
)
test <- test %>% arrange(pct_tree_cover,total_tree_effect)
#test <- test %>% arrange(total_tree_effect)
test$added_trees=0
trees=75000
i=1
while(trees>0){
  test[i,"added_trees"] <- test[i,"number_potential_trees"]
     # test[i,"added_trees"] <- if_else((test[i,"total_lst_effect"]< -.5) |
     #                                    (test[i,"total_aq_effect"]< -1), test[i,"number_potential_trees"],0)
     test[i,"added_trees"] <- if_else((test[i,"total_tree_effect"]< 0), test[i,"number_potential_trees"],0)
  test[i,"added_trees"] <-if_else(test[i,"added_trees"]>13,13,test[i,"added_trees"])
  trees=trees-test[i,"added_trees"]
  i=i+1
}
test <- test %>% mutate(
  new_pct_tree_cover=as.numeric(((added_trees*tree_area)+current_tree_area)/block_area)
)
test <- test %>% mutate(
  predicted_lst = lst_xintercept + lst_utc_coef*(new_pct_tree_cover) + lst_ach_coef*(avg_canopy_height_scaled) + (new_pct_tree_cover*avg_canopy_height_scaled*lst_interaction),
  predicted_lst = if_else(predicted_lst < -3.6470, -3.6470, predicted_lst), # don't let values go below min possible
  predicted_aq = aq_xintercept + aq_utc_coef*(new_pct_tree_cover) + aq_ach_coef*(avg_canopy_height_scaled) + (aq_interaction*new_pct_tree_cover*avg_canopy_height_scaled),
  predicted_aq = if_else(predicted_aq < -4.08269, -4.08269, predicted_aq) # don't let values go below min possible
)
aq_pctile_test<- ecdf(test$mean_pm25)
aq_score <- aq_pctile_test(mean(test$predicted_aq))
aq_score # 0.3507335
lst_pctile_test<- ecdf(test$avg_scaled_lst)
lst_score <- lst_pctile_test(mean(test$predicted_lst))
lst_score # 0.4699343
equity_score <- DescTools::Gini(test$new_pct_tree_cover) # 11 =0.3993328
equity_score # 0.3994143
total_score  = sum(aq_score,lst_score,equity_score)
total_score # 1.220082 (NA threshold, 13 per)



# COuld plot values of index score based on factors to show minimization
#########
test <- test %>% select(geoid10,added_trees_combo=added_trees)

chicago <- left_join(chicago,test,by="geoid10")


comm_area <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_combo = sum(added_trees_combo,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)


#chicago <- chicago %>% select(-contains("added_trees"))
tmap::tm_shape(comm_area) +
  tmap::tm_fill("added_trees_combo",style = "cont") +
  tm_text("ca_name",size=.4)


################################################################################
### Figures
################################################################################
#1. 1x3 map of outcome variables at the block level

# extract a basemap
chicago_osm <- chicago %>% st_bbox() %>% tmaptools::read_osm(type="stamen-terrain")

# type = c("osm", "osm-bw",
#          "maptoolkit-topo", "waze", "bing", "stamen-toner", "stamen-terrain",
#          "stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox", "esri",
#          "esri-topo", "nps", "apple-iphoto", "skobbler", "hillshade", "opencyclemap",
#          "osm-transport", "osm-public-transport", "osm-bbike", "osm-bbike-german")

## Map with raster layer to make sure everything looks alright
water <- st_read("C:/Users/johnf/Box/UCL/thesis/cluster_data/Waterways_Chicago/geo_export_110c65a5-f82f-43cd-9b41-9ae24632d43d.shp")
#water <- water %>% filter(!name %in% "LAKE MICHIGAN")
#water %>% st_geometry() %>% plot()
water <- st_transform(water, 26916) # convert to NAD83 / UTM zone 16N

chicago_boundary <- chicago %>% st_union()
water <- st_crop(water,chicago)
water %>% st_geometry() %>% plot()

tm_shape(water) +
  tm_polygons(col = "lightblue") +
  tmap::tm_shape(chicago) +
  tmap::tm_fill("avg_scaled_lst",style = "cont",palette = "seq",
                title = "Std. Land Surface Temperature",) +
  tm_shape(chicago_boundary) +
  tm_polygons(alpha = 0, border.alpha = .8, border.col = "black") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))
  
p1 <- tmap::tm_shape(chicago) +
  tmap::tm_fill("avg_scaled_lst",style = "cont",palette = "seq",
                title = "Std. Land Surface Temperature",) +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))

p2 <- tmap::tm_shape(chicago) +
  tmap::tm_fill("mean_pm25",style = "cont",palette = "seq",
                title = "Std. Mean PM2.5") +
   # tm_shape(chicago) +
   # tm_polygons(alpha = 0, border.col = "grey75",border.alpha = .1) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02,aes.palette = list(seq = "-RdYlGn"))


p3 <-tmap::tm_shape(chicago) +
  tmap::tm_fill("pct_tree_cover",style = "cont",palette = "Greens",
                title = "Estimate Tree Canopy (%)",) +
  tm_compass(position = c("right","top"),size=.8,text.size = .8) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)

tmap::tmap_arrange(p1,p2,p3)


p1 <- tmap::tm_shape(chicago) +
  tmap::tm_fill("pct_tree_cover",style = "fixed",
                breaks = c(0,.01, .025 ,.05, .075,.1, .2, .52),
                palette = "Greens",
                title = "Estimated Tree Canopy (%)",) +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)


p2 <-tmap::tm_shape(chicago) +
  tmap::tm_fill("avg_canopy_height_scaled",style = "cont",
    #           breaks = c(0,5,7.5,10,12.5,15,36),
                palette = "RdYlGn",
                title = "Average Canopy Height",) +
  tm_compass(position = c("right","top"),size=.8,text.size = .8) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)

tmap::tmap_arrange(p1,p2)

tmap::tm_shape(chicago) +
  tmap::tm_fill("lpa_8",style = "cat",palette = "Set3") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_compass(position = c("right","top"),size=.8,text.size = .8) +
  tm_layout(frame=FALSE, legend.outside = TRUE,
            legend.text.size = .6,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)

################################################################################
#Final maps
comm_area_1 <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_equity = sum(added_trees_equity,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE) %>% mutate(ca_num=row_number())
comm_area_2 <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_aq = sum(added_trees_aq,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)%>% mutate(ca_num=row_number())
comm_area_3 <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_lst = sum(added_trees_lst,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)%>% mutate(ca_num=row_number())
comm_area_4 <- chicago %>% group_by(ca_name) %>% summarise(
  added_trees_combo = sum(added_trees_combo,na.rm=T),
  do_union=T) %>% filter(is.na(ca_name)==FALSE)%>% mutate(ca_num=row_number())


p1 <- tmap::tm_shape(comm_area_1) +
  tmap::tm_fill("added_trees_equity",style = "fixed",
                breaks= c(0,500,1000,2000,5000,10000,22000),
                palette = "Greens") +
  tmap::tm_shape(comm_area_1) +
  tmap::tm_polygons(alpha = 0,border.alpha = .3,border.col = "grey") +
  tm_scale_bar(position = c("left","bottom"),lwd=.5) + 
  tm_text("ca_num",size=.5) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)
p2 <- tmap::tm_shape(comm_area_2) +
  tmap::tm_fill("added_trees_aq",style = "fixed",
                breaks= c(0,500,1000,2000,5000,10000,22000),
                palette = "Greens") +
  tmap::tm_shape(comm_area_1) +
  tmap::tm_polygons(alpha = 0,border.alpha = .3,border.col = "grey") +
  tm_text("ca_num",size=.5) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)
p3 <- tmap::tm_shape(comm_area_3) +
  tmap::tm_fill("added_trees_lst",style = "fixed",
                breaks= c(0,500,1000,2000,5000,10000,22000),
                palette = "Greens") +
  tmap::tm_shape(comm_area_1) +
  tmap::tm_polygons(alpha = 0,border.alpha = .3,border.col = "grey") +
  tm_text("ca_num",size=.5) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)
p4 <- tmap::tm_shape(comm_area_4) +
  tmap::tm_fill("added_trees_combo",style = "fixed",
                breaks= c(0,500,1000,2000,5000,10000,22000),
                palette = "Greens") +
  tmap::tm_shape(comm_area_1) +
  tmap::tm_polygons(alpha = 0,border.alpha = .3,border.col = "grey") +
  tm_text("ca_num",size=.5) +
  tm_layout(frame=FALSE, #legend.outside = TRUE,
            legend.text.size = .5,legend.title.size = .8,
            inner.margins = 0,outer.margins = 0.01,between.margin = 0.02)


tmap::tmap_arrange(p1,p2,p3,p4,ncol = 2)

# some calcualtions for the paper
chicago %>% st_drop_geometry() %>%  mutate(
  number_potential_trees=if_else(number_potential_trees<0,0,number_potential_trees)
  ) %>% group_by(primary_racial_group) %>% summarise(
    trees_equity = sum(added_trees_equity)/75000,
    trees_aq = sum(added_trees_aq)/75000,
    trees_lst = sum(added_trees_lst)/75000,
    trees_combo = sum(added_trees_combo)/75000)

chicago %>% st_drop_geometry() %>%  mutate(
  high_poverty=if_else(pct_below_pov>=.235,1,0),
  number_potential_trees=if_else(number_potential_trees<0,0,number_potential_trees)) %>% group_by(
    high_poverty) %>% summarise(plant_space = mean(number_potential_trees),
                                trees_equity = sum(added_trees_equity)/75000,
                                trees_aq = sum(added_trees_aq)/75000,
                                trees_lst = sum(added_trees_lst)/75000,
                                trees_combo = sum(added_trees_combo)/75000)

chicago %>% st_drop_geometry() %>%  mutate(
  number_potential_trees=if_else(number_potential_trees<0,0,number_potential_trees)
) %>% group_by(lpa_8) %>% summarise(
  trees_equity = sum(added_trees_equity),
  trees_aq = sum(added_trees_aq),
  trees_lst = sum(added_trees_lst),
  trees_combo = sum(added_trees_combo))
