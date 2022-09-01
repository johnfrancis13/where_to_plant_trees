setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
library("tidyverse")
library(sf)
library(raster)
library(lidR)
library(tmap)

# THE FIRST VERSION OF THIS FILE ONLY INCLUDED TREE HEIGHT INSTEAD OF THE HEIGHT 
# OF EACH PIXEL, SO THIS VERSION ADD THAT LAYER IN

lidarlist <- list.files("D:/chicago_lidar_tiles_all/") %>% Filter(function(x) {str_detect(x,"las")}, .)
lidarlist_path <- unlist(lapply(lidarlist, function(i){paste0("D:/chicago_lidar_tiles_all/",i)}))

rgb_match <- read.csv("D:/chicago_rgb/lidar_naip_2017_match.csv", header = T,stringsAsFactors = F)

# ideall i leave the sentinel images in memory the whole time and just cut out the pieces i want, not too much extra time to reread them in though...
sent_2_2017_10m <- stack("D:/chicago_sentinel_images/2017/sent_2_2017_10m.tif")
names(sent_2_2017_10m) <- c("blue", 'green', 'red', 'B8')

sent_2_2017_20m <- stack("D:/chicago_sentinel_images/2017/sent_2_2017_20m.tif")
names(sent_2_2017_20m) <- c("B5", 'B6', 'B7', 'B11','B12',"B8a")


# Short function to calculate NVDI
NDVIfun <- function(NIR, Red) {
  NDVI <- (NIR - Red) / (NIR + Red)
  return(NDVI)
}

# function to process the lidar - outputting 1 raster
process_lidar_height <-  function(lasfile=lidar_path){
  
  ### Dont need to mask out the non-vegetation pixels 
  
  ###  Dont need to Identify pixels containing trees, just need the average height at all pixels
  
  #### DTM normalization ####
  dtm <- rasterize_terrain(temp_las, 1, knnidw(k = 15, p = 2, rmax = 60))
  
  temp_las <- temp_las - dtm
  
  # Canopy Height Model
  # Khosravipour et al. pitfree algorithm
  thr <- c(0,6,15,30,45)
  edg <- c(0, 1.5)
  chm <- rasterize_canopy(temp_las, 1, pitfree(thr, edg))
  
  tree_height_raster <- raster(chm)
  
  return(tree_height_raster)
}


process_lidar <-  function(lasfile=lidar_path, vegfile=veg){
  
  ### Mask out the non-vegetation pixels 
  # not sure if this is sustainable for working with many 
  # merge spatial only works with las not las catalog
  temp_las <- merge_spatial(temp_las, veg, "nvdi_values")
  
  # replace NA values so we can plot
  a <- temp_las$nvdi_values
  a <- if_else(is.na(a)==FALSE,1,0)
  temp_las <- add_attribute(temp_las, a, "nvdi2")
  rm(a)
  
  # subset the point cloud to only values with 
  temp_las <- filter_poi(temp_las, nvdi2 == 1)
  
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
  
  # account for an odd situation where an invalid tree geometry would be generated
  crowns_li_temp <- crowns_li_temp %>% dplyr::filter(is.na(st_is_valid(crowns_li_temp))==FALSE)
  # rasterize the tree crown data
  # Generate empty raster layer and rasterize points
  treecrown_raster <- raster(crs = crs(crowns_li_temp), vals = 0, resolution = c(1, 1),
                             ext = extent(crowns_li_temp)) %>% rasterize(crowns_li_temp, ., field="avgZ")
  
  return(treecrown_raster)
}


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



start_time <- Sys.time()
for(i in  1:length(lidarlist)){
  print(i)
  #### Read in the LAS File
  temp_las <- readLAS(lidarlist_path[i])
  
  naip_image_path <- rgb_match[rgb_match$lidar_file_name == lidarlist[i], "final_path"]
  naip_image_path <- gsub("D:/chicago_rgb/2017/Bulk Order naip_chicago_2017_sep_9/NAIP/",
                          "D:/chicago_rgb/2017/naip_projected/",
                          naip_image_path)
  naip_image <- stack(naip_image_path)
  #naip_image <- projectRaster(naip_image,crs="+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=us-ft +no_defs")
  naip_image <- naip_image %>% crop(extent(temp_las)) # crop/mask to the LiDAR tile 
  # Name the Bands based on where they sample the electromagentic spectrum
  names(naip_image) <- c("red", 'green', 'blue', 'NIR')
  ndvi <- NDVIfun(naip_image$NIR, naip_image$red)
  
  veg <- ndvi %>%
    reclassify(., cbind(-Inf, 0.05, NA)) # sets values below 0 (or some other value, .2?) to NA, and retains others values
  
  # reclassify veg values to 1
  veg <- veg %>%
    reclassify(., cbind(.001, 10, 1L)) # sets values below 0 (or some other value, .2?) to NA, and retains others values
  
  skip_to_next <- FALSE
  
  tryCatch( # need to skip to next image in scenario where there are no trees...
    # Process the LiDAR, outputting the raster layer for tree crown height
    tree_crown_raster <- process_lidar(lasfile = temp_las, vegfile = veg),
    
    error = function(e) { 
      message(paste("An error occurred for item", i, ":\n"), e)
      skip_to_next <<- TRUE})
  if(skip_to_next) { next }  
  
  # get the canopy height model for the full area
  tree_height_raster <- process_lidar_height(lasfile = temp_las)
  
  # need to reproject everything to the new resolution
  
  naip_image <- projectRaster(from = naip_image, to = raster(
    extent(tree_crown_raster), resolution = 3.28, crs = st_crs(tree_crown_raster)$proj4string)
    , method="ngb")
  ndvi <- projectRaster(from = ndvi, to = raster(
    extent(tree_crown_raster), resolution = 3.28, crs = st_crs(tree_crown_raster)$proj4string)
    , method="ngb")
  tree_crown_raster <- projectRaster(from = tree_crown_raster, to = raster(
    extent(tree_crown_raster), resolution = 3.28, crs = st_crs(tree_crown_raster)$proj4string)
    , method="ngb")
  
  tree_crown_raster <- tree_crown_raster %>%
    reclassify(., cbind( 6.58, 100, 1L)) # sets values below 0 (or some other value, .2?) to NA, and retains others values
  
  tree_height_raster <- projectRaster(from = tree_height_raster, to = raster(
    extent(tree_crown_raster), resolution = 3.28, crs = st_crs(tree_crown_raster)$proj4string)
    , method="ngb")
  
  
  # bring in the sentinel 2 images and put those on the correct resolution
  sent_2_2017_10m_temp <- projectRaster(from = sent_2_2017_10m, to = raster(
    extent(tree_crown_raster), resolution = 3.28, crs = st_crs(tree_crown_raster)$proj4string)
    , method="ngb")
  
  sent_2_2017_20m_temp <- projectRaster(from = sent_2_2017_20m, to = raster(
    extent(tree_crown_raster), resolution = 3.28, crs = st_crs(tree_crown_raster)$proj4string)
    , method="ngb")
  
  # stack all of the layers together (naip_image, sent2, lidar)
  final_stack <- raster::stack(naip_image,sent_2_2017_10m_temp,sent_2_2017_20m_temp,tree_height_raster,tree_crown_raster,ndvi)
  
  
  # create patches from the larger raster layers
  my_patches <- patchifyR(img=final_stack, patch_size=240)
  
  # save patches to file
  output_directory <- "D:/final_data/2017_2/"
  
  for(f in 1:length(my_patches$patches)){
    writeRaster(my_patches$patches[[f]], paste0(output_directory, my_patches$names[[f]],"_",i, ".tif"), drivername="Gtiff", overwrite=TRUE)
  }
  
  rm(tree_crown_raster,tree_height_raster,sent_2_2017_10m_temp,sent_2_2017_20m_temp,naip_image,my_patches,final_stack,temp_las,veg)
  gc()
  
}
end_time <- Sys.time()
loop_time <- end_time - start_time




# Some issues with LiDAR files that were different sizes, missing data, etc.
# Removing those before moving to the CNN
test <- stack(paste0("D:/final_data/2017_2/",testlist[3432]))

test <- stack("D:/final_data/2017_2/P_9_X0_481_X1_720_Y0_481_Y1_720_1122.tif")
names(test) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17")
plot(test)

all(is.na(values(test$X12)))

testlist <- list.files("D:/final_data/2017_2/") %>% Filter(function(x) {str_detect(x,"tif")}, .)
# how big is my issue?
for(i in 1:length(testlist)){
  print(i)
  test <- stack(paste0("D:/final_data/2017_2/",testlist[i]))
  names(test) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17")
  # if data is completely missing, move to the missing folder
  if (  all(is.na(values(test$X12))) | all(is.na(values(test$X16)))  ) {
    file.rename( from = file.path(paste0("D:/final_data/2017_2/",testlist[i])) ,
                 to = file.path(paste0("D:/final_data/2017_2/missing/",testlist[i])) )
    print(paste0("file ",i, " copied to missing folder"))
  }
  
}

# check to make sure it looks alright

test <- stack("D:/final_data/2017_2/P_1_X0_1_X1_240_Y0_1_Y1_240_109.tif")
plot(test)

