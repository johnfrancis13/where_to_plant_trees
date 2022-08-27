setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library("tidyverse")
library(sf)
library(raster)
library(lidR)

# Steps
# 1. Unzip RGB and LiDAR tiles
# 2. Read in LiDAR tiles and paste together 
# 3. Read in RGB and create a vegetation mask
# 4. Mask out the non-vegetation pixels
# 5. Of the vegetation pixels, identify pixels containing trees, along with their heights
# 6. finish cleaning and prepping the data necessary for ML
# 7. Use these two rasters in python to predict a model that can predict these
# 8. predict rasters on unseen data in the future for which LiDAR does not exist

################################################################################
# 1. Unzip RGB and LiDAR tiles
# pull the lidar data that we need
# find a few LAS tiles within my RGB test image 
#RGB test image = m_4108711_ne_16_1_20170903 South Side has White- Sox stadium
# only need to run this once....
# proof_concept_set <- c("LAS_17258800.las",
#                        "LAS_17508800.las",
#                        "LAS_17008800.las",
#                        "LAS_16758800.las",
#                        "LAS_17008825.las",
#                        "LAS_17258825.las")
# 
# unzip("D:/cook-las3.zip",files = proof_concept_set ,exdir = "D:/chicago_lidar_tiles")

################################################################################
# 2. Read in LiDAR tiles and paste together 
################################################################################
# Try and avoid reading all the files in at once
las1 <- readLAS("D:/chicago_lidar_tiles/proof_concept/LAS_17258800.las")
# las2 <- readLAS("D:/chicago_lidar_tiles/proof_concept/LAS_17508800.las")
las3 <- readLAS("D:/chicago_lidar_tiles/proof_concept/LAS_17008800.las")
# las4 <- readLAS("D:/chicago_lidar_tiles/proof_concept/LAS_16758800.las")
las5 <- readLAS("D:/chicago_lidar_tiles/proof_concept/LAS_17008825.las")
las6 <- readLAS("D:/chicago_lidar_tiles/proof_concept/LAS_17258825.las")

#### read in the 6 point clouds as a LAS catalog
# las_ctg <- readLAScatalog("D:/chicago_lidar_tiles/proof_concept/")
# las_ctg
# # plot the bounding boxes
# plot(las_ctg)
# 
# # checks validity of each tile
# las_check(las_ctg)


### should be able to use classify_poi
# opt_output_files(las_ctg) <-"D:/chicago_lidar_tiles/proof_concept/veg_cls_{ID}_1"
# 
# output <- classify_poi(las_ctg, class=3, roi = veg2)
# 
# plot(output)
# las = readLAS(output)
# plot(las, color = "Classification")

################################################################################
# 3. Read in RGB and create a vegetation mask
img_ras <- stack("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP/m_4108711_ne_16_1_20170903/m_4108711_ne_16_1_20170903.tif")


# Name the Bands based on where they sample the electromagentic spectrum
names(img_ras) <- c("red", 'green', 'blue', 'NIR')


# Short function to calculate NVDI
NDVIfun <- function(NIR, Red) {
  NDVI <- (NIR - Red) / (NIR + Red)
  return(NDVI)
}


ndvi <- NDVIfun(img_ras$NIR, img_ras$red)

# ndvi %>%
#    plot(.,col = rev(terrain.colors(10)), main = "RGB-NDVI")
# 
# # Let's look at the histogram for this dataset
# ndvi %>%
#  hist(., breaks = 40, main = "NDVI Histogram", xlim = c(-.3,.8))


# We can reclassify to the raster to show use what is most likely going to vegetation
# based on the histogram using ***some sort of justification from literature***

# don't actively need unless i decide to look at vegetation separately
veg <- ndvi %>%
reclassify(., cbind(-Inf, .2, NA)) # sets values below 0 (or some other value, .2?) to NA, and retains others values

# plotRGB(img_ras, 
#         r = 1, g = 2, b = 3)
# 
# veg %>%
#   plot(.,main = 'Possible Veg cover', add=T)

rm(img_ras)
# project raster to be same as the point cloud - this take a bit to run....
veg <- projectRaster(veg,crs="+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")

# crop to the extent of the las catalog
veg <- crop(veg, extent(las1))

# reclassify veg values to 1
veg <- veg %>%
  reclassify(., cbind(.001, .8, 1L)) # sets values below 0 (or some other value, .2?) to NA, and retains others values

#veg3 <- rasterToPolygons(veg2)
rm(ndvi)


################################################################################
# 4. Mask out the non-vegetation pixels 
################################################################################
# not sure if this is sustainable for working with many 
# merge spatial only works with las not las catalog
las1 <- merge_spatial(las1, veg, "nvdi_values")

# how many pixels are veg?
table(las1$nvdi_values) # 3435028/13.2 m = ~26%

# replace NA values so we can plot
a <- las1$nvdi_values
a <- if_else(is.na(a)==FALSE,1,0)
las1 <- add_attribute(las1, a, "nvdi2")
rm(a)

# plot, does it look ok? i think so
plot(las1, color="nvdi2")

# subset the point cloud to only values with 
las1_subset <- filter_poi(las1, nvdi2 == 1)

plot(las1_subset)

rm(las1, veg)

################################################################################
#  5. Of the vegetation pixels, identify pixels containing trees, along with their heights
################################################################################
#### DTM normalization ####
dtm <- rasterize_terrain(las1_subset, 1, knnidw(k = 15, p = 2, rmax = 60))
plot(dtm, col = gray(1:50/50))

nlas <- las1_subset - dtm
plot(nlas, bg = "white")
# not perfect but looks nice
hist(filter_ground(nlas)$Z, breaks = seq(-3, 3, .1), main = "", xlab = "Elevation")

summary(nlas$Z)
# how low am i allowing trees to be? don't want to get bushes and tall grass
nlas <- filter_poi(nlas, Z >= 6.56 & Z<80)

# Canopy Height Model
# Khosravipour et al. pitfree algorithm
thr <- c(0,6,15,30,45)
edg <- c(0, 3)
chm <- rasterize_canopy(nlas, 1, pitfree(thr, edg))

plot(chm)

# locate trees, was 1313 when making cutoff 0
f <- function(x) {x * 0.35 + 9}
heights <- seq(0,50,5)
ws <- f(heights)
plot(heights, ws, type = "l", ylim = c(0,30))

ttops <- locate_trees(nlas, lmf(f))
ttops <- locate_trees(nlas, lmf(ws = 16.4042)) 

# visualize 1
plot(chm, col = height.colors(50))
plot(sf::st_geometry(ttops), add = TRUE, pch = 3)

# visualize 2
x <- plot(nlas, bg = "white", size = 2)
add_treetops3d(x, ttops)

# tree segmentation - pretty meh
algo <- dalponte2016(chm, ttops)
nlas2 <- segment_trees(nlas, algo) # segment point cloud
plot(nlas2, bg = "white", size = 4, color = "treeID") # visualize trees

# this take substantially longer to run but might be better
# nlas2 <- segment_trees(nlas, li2012())
# col <- random.colors(200)
# plot(nlas2, color = "treeID", colorPalette = col)

m <- ~list(avgI = mean(Intensity),avgZ=mean(Z))
crowns_li <- crown_metrics(nlas2, m, attribute = "treeID", geom = "concave")


# plot the SF objects
par(mfrow=c(1,2),mar=rep(0,4))
plot(crowns_li["avgZ"], pal = heat.colors, axes = TRUE, key.pos = NULL, reset = FALSE)
plot(crowns_li["avgI"], pal = heat.colors, axes = TRUE, key.pos = NULL, reset = FALSE)
     
# rasterize the tree crown data
# Generate empty raster layer and rasterize points
treecrown_raster <- raster(crs = crs(crowns_li), vals = 0, resolution = c(1, 1),
                         ext = extent(crowns_li)) %>% rasterize(crowns_li, ., field="avgZ")

rm(chm,dtm, las1_subset,nlas,nlas2,ttops, algo, edg, m, thr, x, NDVIfun,crowns_li)

################################################################################
#  5b. Processs Multiple LiDAR images
################################################################################
start_time <- Sys.time()
lidr_files <- c("D:/chicago_lidar_tiles/proof_concept/LAS_17258800.las",
                "D:/chicago_lidar_tiles/proof_concept/LAS_17008800.las",
                "D:/chicago_lidar_tiles/proof_concept/LAS_17008825.las",
                "D:/chicago_lidar_tiles/proof_concept/LAS_17258825.las")

img_ras <- stack("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP/m_4108711_ne_16_1_20170903/m_4108711_ne_16_1_20170903.tif")

# Name the Bands based on where they sample the electromagentic spectrum
names(img_ras) <- c("red", 'green', 'blue', 'NIR')

# Short function to calculate NVDI
NDVIfun <- function(NIR, Red) {
  NDVI <- (NIR - Red) / (NIR + Red)
  return(NDVI)
}

ndvi <- NDVIfun(img_ras$NIR, img_ras$red)

# don't actively need unless i decide to look at vegetation separately
veg <- ndvi %>%
  reclassify(., cbind(-Inf, 0.1, NA)) # sets values below 0 (or some other value, .2?) to NA, and retains others values

# plotRGB(img_ras, 
#         r = 1, g = 2, b = 3)
# 
# veg %>%
#   plot(.,main = 'Possible Veg cover', add=T)

rm(img_ras,ndvi)
gc()
veg <- crop(veg, extent(c(xmin=444595 ,ymin=4629846  ,xmax=447439 ,ymax=4634427)))

# project raster to be same as the point cloud - this take a bit to run....
veg <- projectRaster(veg,crs="+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")

# reclassify veg values to 1
veg <- veg %>%
  reclassify(., cbind(.001, 10, 1L)) # sets values below 0 (or some other value, .2?) to NA, and retains others values

# crop to keep small
veg <- crop(veg, extent(c(xmin=1168500  ,ymin=1878000   ,xmax=1176000 ,ymax=1886000)))


process_lidar <-  function(lasfile=lidar_path, vegfile=veg){

### read in las
temp_las <- readLAS(lasfile)

# crop veg mask to the extent of the las file
veg_temp <- crop(vegfile, extent(temp_las))

### Mask out the non-vegetation pixels 

# not sure if this is sustainable for working with many 
# merge spatial only works with las not las catalog
temp_las <- merge_spatial(temp_las, veg_temp, "nvdi_values")

# replace NA values so we can plot
a <- temp_las$nvdi_values
a <- if_else(is.na(a)==FALSE,1,0)
temp_las <- add_attribute(temp_las, a, "nvdi2")
rm(a)

# subset the point cloud to only values with 
temp_las <- filter_poi(temp_las, nvdi2 == 1)

rm(veg_temp)
###  Identify pixels containing trees, along with their heights

#### DTM normalization ####
dtm <- rasterize_terrain(temp_las, 1, knnidw(k = 15, p = 2, rmax = 60))

temp_las <- temp_las - dtm
# how low am i allowing trees to be? don't want to get bushes and tall grass
temp_las <- filter_poi(temp_las, Z >= 6.56 & Z<80)

# Canopy Height Model
# Khosravipour et al. pitfree algorithm
thr <- c(0,6,15,30,45)
edg <- c(0, 1.5)
chm <- rasterize_canopy(temp_las, 1, pitfree(thr, edg))

# locate trees, was 1313 when making cutoff 0
f <- function(x) {x * 0.35 + 9}
heights <- seq(0,50,5)
ws <- f(heights)
#plot(heights, ws, type = "l", ylim = c(0,30))

ttops <- locate_trees(temp_las, lmf(f))

# tree segmentation - pretty meh
algo <- dalponte2016(chm, ttops)
temp_las <- segment_trees(temp_las, algo) # segment point cloud

# this take substantially longer to run but might be better
# nlas2 <- segment_trees(nlas, li2012())
# col <- random.colors(200)
# plot(nlas2, color = "treeID", colorPalette = col)

m <- ~list(avgI = mean(Intensity),avgZ=mean(Z))
crowns_li_temp <- crown_metrics(temp_las, m, attribute = "treeID", geom = "concave")


# rasterize the tree crown data
# Generate empty raster layer and rasterize points
treecrown_raster <- raster(crs = crs(crowns_li_temp), vals = 0, resolution = c(1, 1),
                           ext = extent(crowns_li_temp)) %>% rasterize(crowns_li_temp, ., field="avgZ")

return(treecrown_raster)
}


tc_1 <- process_lidar(lasfile = lidr_files[1],vegfile = veg)
tc_2 <- process_lidar(lasfile = lidr_files[2],vegfile = veg)
tc_3 <- process_lidar(lasfile = lidr_files[3],vegfile = veg)
tc_4 <- process_lidar(lasfile = lidr_files[4],vegfile = veg)

#template is an empty raster that has the projected extent of r2 but is aligned with r1 (i.e. same resolution, origin, and crs of r1)
tc_2<- projectRaster(from = tc_2, to= projectRaster(from = tc_2, to= tc_1, alignOnly=TRUE))
tc_3<- projectRaster(from = tc_3, to= projectRaster(from = tc_3, to= tc_1, alignOnly=TRUE))
tc_4<- projectRaster(from = tc_4, to= projectRaster(from = tc_4, to= tc_1, alignOnly=TRUE))

tc_all <- raster::mosaic(tc_1,tc_2,tc_3,tc_4,fun=mean)


tc_all<- projectRaster(from = tc_all, to= raster(
  extent(c(xmin=1170000 ,ymin=1175000  ,xmax=1880000 ,ymax=1885000)), resolution = 1,
         crs = st_crs(tc_all)$proj4string))
rm(tc_1,tc_2,tc_3,tc_4,veg)
end_time <- Sys.time()
time_p1 <- end_time - start_time
# Time difference of 10.81248 mins
writeRaster(tc_all, paste0("D:/proof_concept_3/tc_all" , ".tif"), drivername="Gtiff", overwrite=TRUE)
################################################################################
#  6. prep data for python ML
################################################################################
start_time <- Sys.time()
# need to create a raster stack with all of the variables needed for ML at the same resolution
img_ras <- stack("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP/m_4108711_ne_16_1_20170903/m_4108711_ne_16_1_20170903.tif")

# Name the Bands based on where they sample the electromagentic spectrum
names(img_ras) <- c("red", 'green', 'blue', 'NIR')

img_ras <- crop(img_ras, extent(c(xmin=444595 ,ymin=4629846  ,xmax=447439 ,ymax=4634427)))

# project raster to be same as the point cloud - this take a bit to run....
img_ras <- projectRaster(img_ras,crs="+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")

# crop to the extent of the las catalog
img_ras <- crop(img_ras, extent(tc_all))

# Short function to calculate NVDI
NDVIfun <- function(NIR, Red) {
  NDVI <- (NIR - Red) / (NIR + Red)
  return(NDVI)
}

ndvi <- NDVIfun(img_ras$NIR, img_ras$red)

# not bad but probably need a bit of work here, underestimating tree cover a bit it seems
# perhaps need to change the tree segmentation algorithm..
plotRGB(img_ras,
        r = 1, g = 2, b = 3)
tc_all %>%
  plot(.,main = 'Tree Canopy Height', add=T)

# need to get everything at the same resolution
# Going to make a template and project everything to it
# TEMPLATE.RASTER <- raster(extent(treecrown_raster), resolution = 1, 
#                           crs = st_crs(treecrown_raster)$proj4string)

# # Project the original raster to the new resolution
# lidar_3_projected <- projectRaster(from = lidar_3, to = TEMPLATE.RASTER)
img_ras <- projectRaster(from = img_ras, to = raster(
  extent(tc_all), resolution = 3.28, crs = st_crs(tc_all)$proj4string)
                                     , method="ngb")
ndvi <- projectRaster(from = ndvi, to = raster(
  extent(tc_all), resolution = 3.28, crs = st_crs(tc_all)$proj4string)
  , method="ngb")
tc_all <- projectRaster(from = tc_all, to = raster(
  extent(tc_all), resolution = 3.28, crs = st_crs(tc_all)$proj4string)
  , method="ngb")

tc_all_2 <- tc_all %>%
  reclassify(., cbind( 6.58, 100, 1L)) # sets values below 0 (or some other value, .2?) to NA, and retains others values

# sentstackmask_projected <- raster::stack(img_ras_projected,treecrown_raster)
# 
# 
# plotRGB(img_ras_projected,
#         r = 1, g = 2, b = 3)
# treecrown_raster %>%
#   plot(.,main = 'Tree Canopy Height', add=T)


sentstack <- fs::dir_info("D:/L1C_T16TDM_A011706_20170918T164733/S2A_MSIL1C_20170918T163941_N0205_R126_T16TDM_20170918T164733.SAFE/GRANULE/L1C_T16TDM_A011706_20170918T164733/IMG_DATA/")%>%
  dplyr::select(path)%>%
  pull()%>%
  as.character()

#sent_2_2017 <- raster(rgdal::readGDAL("D:/L1C_T16TDM_A011706_20170918T164733/S2A_MSIL1C_20170918T163941_N0205_R126_T16TDM_20170918T164733.SAFE/GRANULE/L1C_T16TDM_A011706_20170918T164733/IMG_DATA/T16TDM_20170918T163941_B01.jp2"))

# https://gisgeography.com/sentinel-2-bands-combinations/
# https://sentinels.copernicus.eu/web/sentinel/user-guides/sentinel-2-msi/processing-levels/level-2
sent_2_2017_10m <- stack(raster(rgdal::readGDAL(sentstack[2])),
                     raster(rgdal::readGDAL(sentstack[3])),
                     raster(rgdal::readGDAL(sentstack[4])),
                     raster(rgdal::readGDAL(sentstack[8])))

names(sent_2_2017_10m) <- c("blue", 'green', 'red', 'B8')

sent_2_2017_20m <- stack(raster(rgdal::readGDAL(sentstack[5])),
                         raster(rgdal::readGDAL(sentstack[6])),
                         raster(rgdal::readGDAL(sentstack[7])),
                         raster(rgdal::readGDAL(sentstack[11])),
                         raster(rgdal::readGDAL(sentstack[12])),
                         raster(rgdal::readGDAL(sentstack[13])))

names(sent_2_2017_20m) <- c("B5", 'B6', 'B7', 'B11','B12',"B8a")

# # is this a valid way of doing this? not sure, and not sure it matters... could just 
# # keep the raw values
# sent_2_2017_10m <- RStoolbox::rescaleImage(x= sent_2_2017_10m, ymin=0, ymax=255)

# project and crop to the proper extent
sent_2_2017_10m <- projectRaster(sent_2_2017_10m,crs="+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")
sent_2_2017_10m <- crop(sent_2_2017_10m, extent(tc_all))
sent_2_2017_10m <- projectRaster(from = sent_2_2017_10m, to = raster(
  extent(tc_all), resolution = 3.28, crs = st_crs(tc_all)$proj4string)
  , method="ngb")

# project and crop to the proper extent
sent_2_2017_20m <- projectRaster(sent_2_2017_20m,crs="+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")
sent_2_2017_20m <- crop(sent_2_2017_20m, extent(tc_all))
sent_2_2017_20m <- projectRaster(from = sent_2_2017_20m, to = raster(
  extent(tc_all), resolution = 3.28, crs = st_crs(tc_all)$proj4string)
  , method="ngb")

# stack all of the layers together
final_stack <- raster::stack(img_ras,sent_2_2017_10m,sent_2_2017_20m,tc_all,tc_all_2,ndvi)

rm(img_ras,sent_2_2017_10m,sent_2_2017_20m,tc_all)

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

# create patches from the larger raster layers
my_patches <- patchifyR(img=final_stack, patch_size=240)

#### DONT RUN, IMAGES HAVE ALREADY BEEN CREATED ####

output_directory <- "D:/proof_concept_3/"

for(i in 1:length(my_patches$patches)){
  writeRaster(my_patches$patches[[i]], paste0(output_directory, my_patches$names[[i]], ".tif"), drivername="Gtiff", overwrite=TRUE)
}

end_time <- Sys.time()
time_p2 <- end_time - start_time
#7.323803 mins
plotRGB(final_stack,
        r = 1, g = 2, b = 3)
plot(final_stack$layer.1,main = 'Tree Canopy Height', add=T)
