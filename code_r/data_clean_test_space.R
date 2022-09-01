setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library("tidyverse")
library(sf)
library(tmap)
library(lidR)
library(raster)

################################################################################
# THIS FILE CONTAINS LOTS OF LITTLE SNIPS OF CODE FOR VARIOUS PART OF THE CLEANING
# PROCESS
################################################################################

# in QGIS I clipped the grid tiles to the chicago boundary so i could get a list of 
# grid tiles to unzip


chicago_outline <- st_read("../../term one/Principles of Spatial Analysis/data/raw/assessment/chicago_boundaries/geo_export_1889da91-0253-48fe-bbfd-92c0593875b4.shp")
other_tiles <- sf::st_read("D:/Cook_tile_index/Cook_County_Tile_Grid.shp")
proj_tiles <- sf::st_read("D:/Cook_tile_index/tiles_in_chicago.shp")
proj_tiles %>% st_geometry() %>% plot()

tiles <- other_tiles$las_id
tiles <- proj_tiles$las_id

tiles <- unlist(lapply(tiles, paste0, ".las"))

# about 1200 tiles, this is out of the 1549 tiles i downloaded

#unzip("D:/cook-las3.zip",files = tiles,exdir = "D:/chicago_lidar_tiles")

table(tiles %in% poss_files$Name)
poss_files <- unzip("D:/cook-las3.zip",list=TRUE)
poss_files2 <- unzip("D:/cook-las2.zip",list=TRUE)

poss_files <- bind_rows(poss_files,poss_files2)
other_tiles$ctown <- if_else(tiles %in% poss_files$Name,1,0)

tm_shape(other_tiles) +
  tm_fill(col="ctown",alpha=1,border.col="black") +
  tm_shape(chicago_outline) +
  tm_polygons(alpha = 0, border.col = "black")

other_tiles <- other_tiles %>% filter(ctown==1)

# together 2 and 3 have basically all of chicago... going to just ignore ohare...

# need to get a list of all the tiles that are in Chicago
chicago_outline <- st_transform(chicago_outline,crs=st_crs(other_tiles))

lidar_list <- st_join(other_tiles,chicago_outline)


tm_shape(lidar_list) +
  tm_fill(col="shape_area",alpha=1,border.col="black") +
  tm_shape(chicago_outline) +
  tm_polygons(alpha = 0, border.col = "black")


lidar_list <- lidar_list %>% filter(is.na(shape_area)==FALSE)
final_lidar_tiles <- lidar_list$las_id
final_lidar_tiles <- unlist(lapply(final_lidar_tiles, paste0, ".las")) # 1135 LiDAR tiles

# 4 tiles = 1.78 GB
# 1135/4 = 283.75 tile sets
# 284*1.78 = 505.52 GB

# need a separate list for each
unzip("D:/cook-las3.zip",files = final_lidar_tiles,exdir = "D:/chicago_lidar_tiles_all")

# separately go through this process for each list
unzip("D:/cook-las2.zip",files = final_lidar_tiles,exdir = "D:/chicago_lidar_tiles_all")


################################################################################
# find a few LAS tiles within my RGB test image 
#RGB test image = m_4108711_ne_16_1_20170903 South Side has White- Sox stadium
proof_concept_set <- c("LAS_17258800.las",
                       "LAS_17508800.las",
                       "LAS_17008800.las",
                       "LAS_16758800.las",
                       "LAS_17008825.las",
                       "LAS_17258825.las")

unzip("D:/cook-las3.zip",files = proof_concept_set ,exdir = "D:/chicago_lidar_tiles")
################################################################################

# working with point cloud data
library(lidR)
las <- readLAS("D:/chicago_lidar_tiles/LAS_15759000.las")
las <- filter_duplicates(las) # remove 3 duplicated points
las_check(las)
plot(las)

# get a smaller area to work with
las_subset <- clip_rectangle(las = las, xleft =(1157500+750) ,ybottom =(1900000 +750) ,xright = (1160000-750),ytop = (1902500 -750))
las_subset <- clip_rectangle(las = las, xleft =(1157500+900) ,ybottom =(1900000 +900) ,xright = (1160000-900),ytop = (1902500 -900))

las_subset <- clip_rectangle(las = las, xleft =(1157500+250) ,ybottom =(1900000 +500) ,xright = (1160000-1250),ytop = (1902500 -500))


##### filter to only relevant points of interest
# 2-5 is ground plus vegetation (low, med, high)
las_subset <- filter_poi(las_subset, Classification == 2L | Classification == 3L | Classification == 4L  | Classification == 5L)

# other plotting options
plot(las_subset, color = "Classification", bg = "white", axis = TRUE, legend = TRUE) 
plot(las_subset, color = "Intensity", breaks = "quantile", bg = "white")


#### DTM normalization ####
dtm <- rasterize_terrain(las_subset, 1, knnidw())
plot(dtm, col = gray(1:50/50))

nlas <- las_subset - dtm
plot(nlas, bg = "white")
# not perfect but looks nice
hist(filter_ground(nlas)$Z, breaks = seq(-2, 2, .1), main = "", xlab = "Elevation")

summary(nlas$Z)
nlas <- filter_poi(nlas, Z >= 0 & Z<75)

# aonther option
#nlas <- normalize_height(las, knnidw())
#plot(nlas, bg = "white")

#All the ground points should be exactly 0. Let check it:
#hist(filter_ground(nlas)$Z, breaks = seq(-0.6, 0.6, 0.01), main = "", xlab = "Elevation")


# Khosravipour et al. pitfree algorithm
thr <- c(0,6,15,30,45)
edg <- c(0, 1.5)
chm <- rasterize_canopy(nlas, 1, pitfree(thr, edg))

plot(chm)

# locate trees
ttops <- locate_trees(nlas, lmf(ws = 30)) 

# visualize 1
plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

# visualize 2
x <- plot(nlas, bg = "white", size = 2)
add_treetops3d(x, ttops)

#rgl::rgl.postscript("persp3d.pdf", "pdf")
rgl::rgl.snapshot('3dplot.png', fmt = 'png')


# Another way to generate the tree locations
# change window size based on pixel height - likley overestimates small trees...
# could potentially revamp this a bit
# f <- function(x) {x * 0.4 + 10}
# heights <- seq(0,50,5)
# ws <- f(heights)
# plot(heights, ws, type = "l", ylim = c(0,30))
# 
# ttops <- locate_trees(nlas, lmf(f))

plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)


x <- plot(nlas, bg = "white", size = 2)
add_treetops3d(x, ttops)

# tree segmentation - pretty meh
algo <- dalponte2016(chm, ttops)
nlas2 <- segment_trees(nlas, algo) # segment point cloud
plot(nlas2, bg = "white", size = 4, color = "treeID") # visualize trees

# this take substantially longer to run but might be better
nlas2 <- segment_trees(nlas, li2012())
col <- random.colors(200)
plot(nlas2, color = "treeID", colorPalette = col)

m <- ~list(avgI = mean(Intensity),avgZ=mean(Z))
crowns_li <- crown_metrics(nlas2, m, attribute = "treeID", geom = "concave")

trees_li <- tree_metrics(nlas2, func = .stdmetrics)

# plot the SF objects
par(mfrow=c(1,2),mar=rep(0,4))
plot(crowns_li["avgZ"], pal = heat.colors, axes = TRUE, key.pos = NULL, reset = FALSE)
plot(crowns_li["avgI"], pal = heat.colors, axes = TRUE, key.pos = NULL, reset = FALSE)

# this is a spatial points df, a bit more annoying to plot
par(mfrow = c(1,1))
plot(trees_li["zmax"]) # plot using z_max


## what does it look like if we remove lower intenstiy trees?
crowns_li_2 <- crowns_li[crowns_li$avgI > 7000,] # filter high intensity trees

nlas2_subset <- filter_poi(nlas2, treeID %in% crowns_li_2$treeID)
x <- plot(nlas2, bg = "white", size = 4)
plot(nlas2_subset, add = x + c(-100, 0), size = 5) # some plotting

plot(nlas2, bg = "white", size = 3)
plot(nlas2_subset, bg = "white", size = 3)
#### plot the point cloud ####
offsets <- plot(nlas2, bg = "white", size = 3)
add_treetops3d(offsets, ttops)

# # extract the coordinates of the trees and
# # apply the shift to display the lines
# # in the rendering coordinate system
# x <- sf::st_coordinates(ttops)[,1] - offsets[1] 
# y <- sf::st_coordinates(ttops)[,2] - offsets[2] 
# z <- ttops$Z
# 
# # Build a GL_LINES matrix for fast rendering
# x <- rep(x, each = 2)
# y <- rep(y, each = 2)
# tmp <- numeric(2*length(z)) 
# tmp[2*1:length(z)] <- z
# z <- tmp
# M <- cbind(x,y,z)
# 
# # Display lines
# rgl::segments3d(M, col = "black", lwd = 2)
# 
# 
# # segment trees
# las2 <- segment_trees(las, li2012())
# col <- random.colors(200)
# plot(las2, color = "treeID", colorPalette = col)
# 
# lidR::writeLAS(las2,"D:/chicago_lidar_tiles/LAS_15759000_treeid.las")


################################################################################
# 6/30/22 
# New Scratch Space for data cleaning work

# so i need to be able to associate each LiDAR file with a satellite image
#### read in the 6 point clouds as a LAS catalog
las_ctg <- readLAScatalog("D:/chicago_lidar_tiles_all/")

# plot the bounding boxes
plot(las_ctg)

# checks validity of each tile
#las_check(las_ctg)

# for each image:
# 1. check to make sure it is within chicago bounday...
# 2. convert to NAD83(2011) / Illinois East (ftUS)
# 3. see which Lidar tiles overlap with it
# 4. create a dataframe 

lidar_files <- as.data.frame(las_ctg$filename)

lidar_files <- lidar_files %>% mutate(
  lidar_file_name = gsub("D:/chicago_lidar_tiles_all/","",`las_ctg$filename`)
) %>% dplyr::select(lidar_file_name)

#lidar_files$naip_tile <- NA_character_


# filelist <- list.files("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP") %>% Filter(function(x) {str_detect(x,"ZIP")}, .)
# 
# filelist <- unlist(lapply(filelist, function(i){paste0("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP/",i)}))
# 
# 
# for(i in 1:length(filelist)){
#   unzip(filelist[i] ,exdir = "D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP")  
#   print(i)
# }



imagelist <- list.files("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP") %>% Filter(function(x) {str_detect(x,"tif")}, .)
imagelist <- unlist(lapply(imagelist, function(i){paste0("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP/",i)}))



img_ras <- stack("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP/m_4108711_ne_16_1_20170903.tif")
#img_ras <- projectRaster(img_ras,crs="+init=epsg:6455")

chicago_outline <- st_read("../../term one/Principles of Spatial Analysis/data/raw/assessment/chicago_boundaries/geo_export_1889da91-0253-48fe-bbfd-92c0593875b4.shp")
chicago_outline <- chicago_outline %>% st_transform(.,crs=st_crs(img_ras))


ctg_df <- las_ctg@data
ctg_df <- ctg_df %>% dplyr::select(tile_file=filename)
ctg_df <- ctg_df %>% st_transform(.,crs=st_crs(img_ras))



#ctg_df_all <- ctg_df %>%  summarise()

#r <- extent( c(1091131, 1207500, 1812500, 1952500) )
r <- extent( c(422130.9, 457130.6, 4609946, 4652857) )
r <- as(r, 'SpatialPolygons')
r <- sf::st_as_sf(r)
st_crs(r) <- st_crs(chicago_outline)



# Loop over NAIP tiles and retrieve the info about which LiDAR tiles are associated with them
start_time <- Sys.time()
for(i in 1:length(imagelist)){
  
  skip_to_next <- FALSE
  print(imagelist[i])
  temp_img <- stack(imagelist[i]) # load in the imagelist
#  temp_img <- projectRaster(temp_img,crs="+init=epsg:6455") # project it
  tryCatch(
  temp_img <- temp_img %>% crop(r) %>% mask(r), # crop/mask to Chicago 
  error = function(e) { skip_to_next <<- TRUE})
  if(skip_to_next) { next }  
  
  temp_r <- terra::as.polygons(terra::rast(temp_img) > -Inf) # need to get the outline of the raster
  temp_r <- sf::st_as_sf(temp_r)
  st_crs(temp_r) <- st_crs(temp_img) # add projection back in
  temp_intersect <- st_intersects(ctg_df,temp_r) # get a list of 1135 tiles and if they intersect
  temp_intersect <- mapply(function(x,y) c(x,y)[1], temp_intersect, 0) # convert to usable list
  temp_intersect <- ifelse(temp_intersect ==1,imagelist[i],NA_character_) # replace 1 with filename
  lidar_files <- bind_cols(lidar_files,temp_intersect) # append to final file
  
  #temp_img <- terra::as.polygons(terra::ext(terra::rast(temp_img))) # create a polygon of the outline
  #temp_img <- m  # join with the lidar data
  # create a 1/0 column for each NAIP tile (maybe NA/filename)
  # append that column to the lidar name file
}

end_time <- Sys.time()
loop_time <- end_time - start_time
# Time difference of 3.171765 hours - projecting the raster
# Time difference of 27.41148 mins - not projecting the raster...


# img_ras <- stack("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP/m_4108711_ne_16_1_20170903.tif")
# img_ras <- stack(imagelist[9])
# img_ras <- projectRaster(img_ras,crs="+init=epsg:6455")

plotRGB(img_ras, 
        r = 1, g = 2, b = 3,add=T)

plot(img_ras, add=T)

#img_ras2 <- st_join(img_ras,las_ctg)



tm_shape(chicago_outline) +
  tm_polygons(alpha = 0, border.col = "black") +
  tm_shape(img_ras) +
  tm_rgb()


# img_ras <- img_ras %>% crop(chicago_outline) %>% mask(chicago_outline)
# 
# plotRGB(img_ras, 
#         r = 1, g = 2, b = 3,add=T)
# 
# 
# 
# # THIS MAKES A POLYGON (KINDA)
# #r <- terra::as.polygons(terra::ext(terra::rast(img_ras))) # This will create a sfc polygon dataframe from your cells with values
# r2 <- terra::as.polygons(terra::rast(img_ras) > -Inf)
# r2 <- sf::st_as_sf(r2)
# st_crs(r2) <- st_crs(img_ras)
# 
# 
# 
# 
# test <- st_intersects(ctg_df,r2) # list of 1135
# test <- mapply(function(x,y) c(x,y)[1], test, 0)
# test <- ifelse(test ==1,imagelist[9],NA_character_)
# lidar_files <- bind_cols(lidar_files,test)

################################################################################
count_na <- function(x) sum(is.na(x)) 

lidar_files <- lidar_files %>% mutate(
  count_na = apply(., 1, count_na)
)

table(lidar_files$count_na)
#37/1135 = 3.3% with no tile

lidar_files <- lidar_files %>% mutate(
  problem_cases = if_else(count_na==36,1,0)
)

lidar_merge <- lidar_files %>% dplyr::select(lidar_file_name,problem_cases)
ctg_df$tile_file <- gsub("D:/chicago_lidar_tiles_all/","",ctg_df$tile_file)

ctg_df <- ctg_df %>% rename(lidar_file_name=tile_file)
ctg_df <- left_join(ctg_df,lidar_merge, by="lidar_file_name")

tm_shape(ctg_df) +
  tm_polygons("problem_cases", palette="Blues", style="cont",) 

# so we are good now
lidar_files %>% mutate(mycol = coalesce(x,y,z))
a <- lidar_files %>% unite("all",dplyr::contains(".."),sep = ",,,", na.rm = TRUE, remove = FALSE)

a <- a %>% dplyr::select(lidar_file_name,all)

a <- a %>% separate("all",c("naip_1","naip_2","naip_3","naip_4"),sep = ",,,") 

write.csv(a,"D:/chicago_rgb/lidar_naip_2017_match.csv",row.names = FALSE)

################################################################################
lidarlist <- list.files("D:/chicago_lidar_tiles_all/") %>% Filter(function(x) {str_detect(x,"las")}, .)
lidarlist_path <- unlist(lapply(lidarlist, function(i){paste0("D:/chicago_lidar_tiles_all/",i)}))

rgb_match <- read.csv("D:/chicago_rgb/lidar_naip_2017_match.csv", header = T,stringsAsFactors = F)

lidarlist_2 <- rgb_match %>% filter(is.na(naip_2)==FALSE) %>% dplyr::select(lidar_file_name) %>% list() %>% unlist()
lidarlist_path2 <- unlist(lapply(lidarlist_2, function(i){paste0("D:/chicago_lidar_tiles_all/",i)}))


lidarlist_3 <- rgb_match %>% filter(is.na(naip_4)==FALSE) %>% dplyr::select(lidar_file_name) %>% list() %>% unlist()
lidarlist_path3 <- unlist(lapply(lidarlist_4, function(i){paste0("D:/chicago_lidar_tiles_all/",i)}))


lidarlist_4 <- rgb_match %>% filter(is.na(naip_4)==FALSE) %>% dplyr::select(lidar_file_name) %>% list() %>% unlist()
lidarlist_path4 <- unlist(lapply(lidarlist_4, function(i){paste0("D:/chicago_lidar_tiles_all/",i)}))


r <- readLAS(lidarlist_path2[1])
chicago_outline <- st_read("../../term one/Principles of Spatial Analysis/data/raw/assessment/chicago_boundaries/geo_export_1889da91-0253-48fe-bbfd-92c0593875b4.shp")
chicago_outline <- chicago_outline %>% st_transform(.,crs=st_crs(r))
rm(r)

newlist <- list()
start_time <- Sys.time()
for (i in 1:length(lidarlist_3)) {
  print(i)
  las1 <- readLAS(lidarlist_path3[i])
  #naip_count <- rgb_match[rgb_match$lidar_file_name == lidarlist_2[i], "naip_count"]
  naip_image_path <- rgb_match[rgb_match$lidar_file_name == lidarlist_3[i], "naip_3"]
  img_ras <- stack(naip_image_path)
  
  # create a bounding box from the NAIP image
  r <- terra::as.polygons(terra::ext(terra::rast(img_ras))) # This will create a sfc polygon dataframe from your cells with values
  r <- sf::st_as_sf(r)
  st_crs(r) <- st_crs(img_ras)
  r <- st_transform(r,crs=st_crs(las1))
  
  l <- extent(las1)
  l <- as(l, 'SpatialPolygons')
  l <- sf::st_as_sf(l)
  st_crs(l) <- st_crs(r)
  
  l <- l$geometry %>% st_intersection(chicago_outline) # clip the lidar file to Chicago
  a <- st_contains_properly(r,l)
  newlist <- append(newlist,a)
}
end_time <- Sys.time()
loop_time <- end_time - start_time
loop_time

# 
# chicago_outline %>% st_geometry() %>% plot()
# plot(r,add=T)
# plot(l,add=T)

unlist(newlist)


newlist_2 <- mapply(function(x,y) c(x,y)[1], newlist, 0)

newlist_2 <- unlist(newlist_2)

newlist_2 <- data.frame(lidarlist_3,newlist_2)

newlist_2 <- newlist_2 %>% rename(lidar_file_name=lidarlist_3) 

rgb_match <- left_join(rgb_match,newlist_2,by="lidar_file_name")

rgb_match <- rgb_match %>% rename(naip_3_completely_contains=newlist_2)
# if not, going to need to crop the image and then mosaic them together....

rgb_match <- rgb_match %>% mutate(
  all_check = ifelse(naip_1_completely_contains==1 | naip_2_completely_contains==1 |
                       naip_3_completely_contains==1 | naip_4_completely_contains==1,1,0)
)

table(rgb_match$naip_count,rgb_match$all_check,useNA = "ifany")

rgb_match <- rgb_match %>% mutate(
  final_path = case_when(
    naip_1_completely_contains==1~naip_1,
    naip_2_completely_contains==1~naip_2,
    naip_3_completely_contains==1~naip_3,
    naip_4_completely_contains==1~naip_4,
    NA~"naip_1",
  )
)



#### NOTE can just drop the missing from all_check
###################################################################################
# need to create patches for my prediction data, but first need to unzip and project? all the lidar tiles
filelist <- list.files("D:/chicago_rgb/2019/") %>% Filter(function(x) {str_detect(x,"ZIP")}, .)

filelist <- unlist(lapply(filelist, function(i){paste0("D:/chicago_rgb/2019/",i)}))


for(i in 1:length(filelist)){
  unzip(filelist[i] ,exdir = "D:/chicago_rgb/2019/NAIP")
  print(i)
}


# next we need to check if they are in the outline of chicago or not, can delete the rest

imagelist <- list.files("D:/chicago_rgb/2019/NAIP") %>% Filter(function(x) {str_detect(x,"tif")}, .)
imagelist <- unlist(lapply(imagelist, function(i){paste0("D:/chicago_rgb/2019/NAIP/",i)}))



img_ras <- stack(imagelist[i])
#img_ras <- projectRaster(img_ras,crs="+init=epsg:6455")

chicago_outline <- st_read("../../term one/Principles of Spatial Analysis/data/raw/assessment/chicago_boundaries/geo_export_1889da91-0253-48fe-bbfd-92c0593875b4.shp")
chicago_outline <- chicago_outline %>% st_transform(.,crs=st_crs(img_ras))


# Loop over NAIP tiles and retrieve the info about which LiDAR tiles are associated with them
start_time <- Sys.time()
newlist <- list()
for(i in 35:length(imagelist)){
  print(imagelist[i])
  temp_img <- stack(imagelist[i]) # load in the imagelist
  temp_r <- terra::as.polygons(terra::rast(temp_img) > -Inf) # need to get the outline of the raster
  temp_r <- sf::st_as_sf(temp_r)
  st_crs(temp_r) <- st_crs(temp_img) # add projection back in
  temp_intersect <- st_intersects(chicago_outline,temp_r) # get a list of 1135 tiles and if they intersect
  temp_intersect <- mapply(function(x,y) c(x,y)[1], temp_intersect, 0) # convert to usable list
  temp_intersect <- ifelse(temp_intersect ==1,imagelist[i],NA_character_) # replace 1 with filename

  newlist <- append(newlist,temp_intersect)
  do.call(file.remove, list(list.files("C:/Users/johnf/AppData/Local/Temp/Rtmpk3amHc", full.names = TRUE)))
}

end_time <- Sys.time()
loop_time <- end_time - start_time

df <- as.data.frame(unlist(newlist),unlist(imagelist))
rm(df)

# can go delete all the images not in Chicago....

# next we want to take the images that ARE in Chicago, stack them with their sentinel 2 counterparts
# and then chop them up into evenly 240x240 images at a 1m resolution..

# # first project the sentinel two images into their two files.
# sentstack <- fs::dir_info("D:/L1C_T16TDM_A012307_20190715T164859/S2B_MSIL1C_20190715T163849_N0208_R126_T16TDM_20190715T214622.SAFE/GRANULE/L1C_T16TDM_A012307_20190715T164859/IMG_DATA/")%>%
#   dplyr::select(path)%>%
#   pull()%>%
#   as.character()
# 
# # https://gisgeography.com/sentinel-2-bands-combinations/
# # https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-2-msi/processing-levels/level-2
# sent_2_2019_10m <- stack(raster(rgdal::readGDAL(sentstack[2])),
#                          raster(rgdal::readGDAL(sentstack[3])),
#                          raster(rgdal::readGDAL(sentstack[4])),
#                          raster(rgdal::readGDAL(sentstack[8])))
# 
# names(sent_2_2019_10m) <- c("blue", 'green', 'red', 'B8')
# 
# sent_2_2019_20m <- stack(raster(rgdal::readGDAL(sentstack[5])),
#                          raster(rgdal::readGDAL(sentstack[6])),
#                          raster(rgdal::readGDAL(sentstack[7])),
#                          raster(rgdal::readGDAL(sentstack[11])),
#                          raster(rgdal::readGDAL(sentstack[12])),
#                          raster(rgdal::readGDAL(sentstack[13])))
# 
# names(sent_2_2019_20m) <- c("B5", 'B6', 'B7', 'B11','B12',"B8a")
# 
# 
# # project raster to be same as the point cloud - this take a bit to run....
# 
# extent(412130.9,
#       466395.2, 
#       4600445, 
#       4672556)
# 
# sent_2_2019_10m <- crop(sent_2_2019_10m, extent(412130.9, 466395.2,4600445, 4672556))
# sent_2_2019_20m <- crop(sent_2_2019_20m,extent(412130.9, 466395.2,4600445, 4672556))
# 
# 
# 
# 
# sent_2_2019_10m <- projectRaster(from = sent_2_2019_10m, to = raster(
#   extent(sent_2_2019_10m), resolution = 1, crs = st_crs(naip_image)$proj4string)
#   , method="ngb")
#   
# sent_2_2019_20m <- projectRaster(from = sent_2_2019_20m, to = raster(
#   extent(sent_2_2019_20m), resolution = 1, crs = st_crs(naip_image)$proj4string)
#   , method="ngb")
# 
# # save to external drive
# writeRaster(sent_2_2019_10m, paste0("D:/chicago_sentinel_images/2019/", "sent_2_2019_10m", ".tif"), drivername="Gtiff", overwrite=TRUE)
# writeRaster(sent_2_2019_20m, paste0("D:/chicago_sentinel_images/2019/", "sent_2_2019_20m", ".tif"), drivername="Gtiff", overwrite=TRUE)

# Need a function to split the large rasters into  pieces
patchifyR <- function(img, patch_size){
  # load raster 
  if (!require("raster")) install.packages("raster")
  suppressPackageStartupMessages({library(raster)})
  # create image divisible by the patch_size
  message(paste0("Cropping original image. ", "Making it divisible by ", patch_size, "."))
  x_max <- patch_size*trunc(nrow(img)/patch_size)
  y_max <- patch_size*trunc(ncol(img)/patch_size)
  img <- crop(img, extent(img, 1, x_max, 1, y_max))
  # initializers
  lx = 1; ly = 1; p = 1
  ls.patches <- list()
  ls.coordinates <- list()
  # extract patches
  for(i in 1:(nrow(img)/patch_size)){
    for(j in 1:(ncol(img)/patch_size)){
      ls.patches[[p]] <- crop(img, extent(img, lx, (lx+patch_size)-1, ly, (ly+patch_size)-1))
      ls.coordinates[[p]] <- as.character(paste0("P_", p, "_X0_", lx, "_X1_", (lx+patch_size)-1, "_Y0_", ly, "_Y1_", (ly+patch_size)-1))
      message(paste0("Patch ", p, " created. Coordinates: ", "X0_", lx, "_X1_", (lx+patch_size)-1, "_Y0_", ly, "_Y1_", (ly+patch_size)-1))
      p = p + 1
      ly = ly + patch_size
    }
    ly = 1
    lx = lx + patch_size
  }
  # merge results: $patches and $names
  message("Matching results ... ")
  patchify <- list("patches"=ls.patches, "names"=ls.coordinates)
  # return
  message("Successfully completed.")
  return(patchify)
}


imagelist <- list.files("D:/chicago_rgb/2019/NAIP") %>% Filter(function(x) {str_detect(x,"tif")}, .)
imagelist <- unlist(lapply(imagelist, function(i){paste0("D:/chicago_rgb/2019/NAIP/",i)}))


# ideall i leave the sentinel images in memory the whole time and just cut out the pieces i want, not too much extra time to reread them in though...
sent_2_2019_10m <- stack("D:/chicago_sentinel_images/2019/sent_2_2019_10m.tif")
names(sent_2_2019_10m) <- c("blue", 'green', 'red', 'B8')
# sent_2_2019_10m <- projectRaster(from = sent_2_2019_10m, to = raster(
#   extent(sent_2_2019_10m), resolution = 1, crs = st_crs(naip_image)$proj4string)
#   , method="ngb")


sent_2_2019_20m <- stack("D:/chicago_sentinel_images/2019/sent_2_2019_20m.tif")
names(sent_2_2019_20m) <- c("B5", 'B6', 'B7', 'B11','B12',"B8a")
# sent_2_2019_20m <- projectRaster(from = sent_2_2019_20m, to = raster(
#   extent(sent_2_2019_20m), resolution = 1, crs = st_crs(naip_image)$proj4string)
#   , method="ngb")




start_time <- Sys.time()
for(i in 27:length(imagelist)){
  print(i)
  naip_image <- stack(imagelist[i]) # load in the imagelist
  
  naip_image <- projectRaster(naip_image,crs=st_crs(sent_2_2019_10m)$proj4string)
  
  naip_image <- projectRaster(from = naip_image, to = raster(
  extent(naip_image),resolution = 1, crs = st_crs(naip_image)$proj4string), method="bilinear")
  
  # bring in the sentinel 2 images and put those on the correct resolution
  sent_2_2019_10m_temp <- crop(sent_2_2019_10m, extent(naip_image)) # for speed
  sent_2_2019_10m_temp <- disaggregate(sent_2_2019_10m_temp, fact=10) # for res

  
  sent_2_2019_20m_temp <- crop(sent_2_2019_20m, extent(naip_image))
  sent_2_2019_20m_temp <- disaggregate(sent_2_2019_20m_temp, fact=20)
  
  naip_image <- projectRaster(from = naip_image, to = raster(
    extent(naip_image), resolution = 1, crs = st_crs(naip_image)$proj4string)
    , method="bilinear")
  
  sent_2_2019_20m_temp <- projectRaster(from = sent_2_2019_20m_temp, to = raster(
    extent(naip_image), resolution = 1, crs = st_crs(naip_image)$proj4string)
    , method="bilinear")
  
  sent_2_2019_10m_temp <- projectRaster(from = sent_2_2019_10m_temp, to = raster(
    extent(naip_image), resolution = 1, crs = st_crs(naip_image)$proj4string)
    , method="bilinear")

  # stack all of the layers together (naip_image, sent2, lidar)
  final_stack <- raster::stack(naip_image,sent_2_2019_10m_temp,sent_2_2019_20m_temp)
  
  # create patches from the larger raster layers
  my_patches <- patchifyR(img=final_stack, patch_size=240)
  
  # save patches to file
  output_directory <- "D:/final_data/2019_predictions/"
  
  for(f in 1:length(my_patches$patches)){
    writeRaster(my_patches$patches[[f]], paste0(output_directory, my_patches$names[[f]],"_",i, ".tif"), drivername="Gtiff", overwrite=TRUE)
  }
#  tmpDir()
#  tempdir()
  do.call(file.remove, list(list.files("C:/Users/johnf/AppData/Local/Temp/RtmpiOf7dz/", full.names = TRUE)))
  do.call(file.remove, list(list.files("C:/Users/johnf/AppData/Local/Temp/RtmpiOf7dz/raster/", full.names = TRUE)))
  gc()
}

end_time <- Sys.time()
loop_time <- end_time - start_time


#  code for clearing out the tmp files, ran into some issues there...
# 
# tmp <- tempfile()
# 
# do.call(file.remove, list(list.files("C:/Users/johnf/AppData/Local/Temp/Rtmpk3amHc", full.names = TRUE)))



# make one more loop, i think i have waaaaay to many tif files, need to go through
# and delete any that dont at least touch the chicago boundary.



# next we need to check if they are in the outline of chicago or not, can delete the rest
imagelist <- list.files("D:/final_data/2019_predictions/") %>% Filter(function(x) {str_detect(x,"tif")}, .)
imagelist <- unlist(lapply(imagelist, function(i){paste0("D:/final_data/2019_predictions/",i)}))


img_ras <- stack(imagelist[i])
#img_ras <- projectRaster(img_ras,crs="+init=epsg:6455")

chicago_outline <- st_read("../../term one/Principles of Spatial Analysis/data/raw/assessment/chicago_boundaries/geo_export_1889da91-0253-48fe-bbfd-92c0593875b4.shp")
chicago_outline <- chicago_outline %>% st_transform(.,crs=st_crs(img_ras))


# Loop over patches and delete files that aren't in Chicago
start_time <- Sys.time()
files_removed = 0
for(i in 1:length(imagelist)){
  print(i)
  temp_img <- stack(imagelist[i]) # load in the imagelist
  temp_r <- terra::as.polygons(terra::rast(temp_img) > -Inf) # need to get the outline of the raster
  temp_r <- sf::st_as_sf(temp_r)
  st_crs(temp_r) <- st_crs(temp_img) # add projection back in
  temp_intersect <- st_intersects(chicago_outline,temp_r) # get a list of the tile and if they intersect
  temp_intersect <- mapply(function(x,y) c(x,y)[1], temp_intersect, 0) # convert to usable list
#  temp_intersect <- ifelse(temp_intersect ==1,imagelist[i],NA_character_) # replace 1 with filename
  
  if (temp_intersect==0) {
    #Delete file if it exists
    file.remove(imagelist[i])
    files_removed = files_removed + 1
    print(paste0(files_removed, " files have been removed."))
  }
  # tmp <- tempfile()
  do.call(file.remove, list(list.files("C:/Users/johnf/AppData/Local/Temp/RtmpkBIZtX", full.names = TRUE)))
}

end_time <- Sys.time()
loop_time <- end_time - start_time


#################################################################################
# try and create data to generate an estimate of what the ground truth 2017 data says
imagelist <- list.files("D:/final_data/2017_2/") %>% Filter(function(x) {str_detect(x,"tif")}, .)
imagelist <- unlist(lapply(imagelist, function(i){paste0("D:/final_data/2017_2/",i)}))

output_directory <- "D:/final_data/2017_ground_truth_tree/"
# Loop over patches and delete files that aren't in Chicago
start_time <- Sys.time()

for(i in 1:length(imagelist)){
  print(i)
  temp_img <- stack(imagelist[i]) # load in the imagelist
  names(temp_img) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)
  temp_img <- temp_img$X16
  crs(temp_img) <- "+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs"
  # get the raster
  temp_img[is.na(temp_img[])] <- 0 
  new_path <- gsub("D:/final_data/2017_2/","",imagelist[i])
  writeRaster(temp_img, paste0(output_directory, new_path), drivername="Gtiff", overwrite=TRUE)
}

end_time <- Sys.time()
loop_time <- end_time - start_time

