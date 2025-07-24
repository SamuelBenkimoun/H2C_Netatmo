library(lidR)
library(terra)
library(sf)
library(dplyr)
library(horizon)

setwd("~/H2C_Netatmo") #setting the working directory

urls <- readLines("../H2C/LIDAR/Liens/liste_dalles_auto_salve1.txt") # list to be collected from the IGN website: https://geoservices.ign.fr/lidarhd#telechargementclassifiees
iris <- st_read("../INSEE/iris.gpkg")

filter_class_veg <- function(chunk) { # Can take only one parameter apparently ?
  las <- readLAS(chunk)
  if (npoints(las) == 0) return(NULL)  # Must check if las is empty, pattern to respect for the function to work
  
  las <- filter_poi(las, Classification %in% c(3, 4, 5)) # 5 instead of 8 ? points classification corresponding the low/mid/high vegetation
  return(las)
  
  # remove the buffer of the output
  bbox <- bbox(chunk)
  output <- remove_buffer(output, bbox)
  return(output)
} # function to filter the vegetation points
filter_class_water <- function(chunk) {
  las <- readLAS(chunk)
  if (npoints(las) == 0) return(NULL)
  
  las <- filter_poi(las, Classification == 9)
  if (is.empty(las)) return(NULL)
  return(las)
  
  bbox <- bbox(chunk)
  output <- remove_buffer(output, bbox)
  return(output)
} # function to filter the water points
zones_list = list()

i = 1 #initializing the index

#while (i <= length(urls)) { 
while (i <= 32) { #32 done
  
  print(urls[i:min(i + 15, length(urls))])
  tiles <- urls[i:min(i + 15, length(urls))]  #screening the tileset by subsets of 16 tiles
  
  for (tile in tiles) {
    filename <- basename(tile) #extracting name the file
    dest <- file.path("../H2C/LIDAR/Subset/", filename) #storing the file in a folder
    download.file(tile, dest, mode = "wb") #write binary
    cat(filename, "\n")
  }
  # Reading the collection of LIDAR files
  ctg <- readLAScatalog("../H2C/LIDAR/Subset/")
  # Checking the validity of the files
  las_check(ctg)
  
  # Creating a digital terrain model, and computing the height, slope and aspect
  ## Creating the dtm
  dtm <- rasterize_terrain(ctg, res = 1, tin(), pkg = "terra") # resolution 1m
  ## Locating the centroid of the zone, to create a comprehensive filename
  centroid <- st_as_sfc(st_bbox(dtm), crs = crs(dtm)) %>%
    st_centroid() %>%
    st_as_sf()
  zone_name <- st_join(centroid, iris["nom_iris"])$nom_iris
  zone_name <- gsub(zone_name, pattern = " ", replacement = "_")
  
  if (zone_name %in% zones_list) {
    zone_name = paste(zone_name,"_",i, sep="")
  }
  zones_list[i] <- zone_name
  
  path <- paste("./Outputs_auto/dtm_", zone_name, "_1m.tif", sep = "")
  ## Writing the dtm file
  terra::writeRaster(dtm, filename = path)
  ## Computing the slope and aspect
  dtm_prod <- terra::terrain(dtm, v = c("slope", "aspect"), unit = "radians") # Computing the slope and aspect of each cell, can be computed in degrees
  ## Writing the slope/aspect file
  path <- paste("./Outputs_auto/dtm_prod_", zone_name, "_1m.tif", sep = "")
  terra::writeRaster(dtm_prod, filename = path)
  
  # Creating normalized/not normalized heights from the dtm
  opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm") # designating a temp output file
  ctg_norm <- normalize_height(ctg, dtm) 
  opt_select(ctg_norm) <- "xyzci" # selecting only the coordinates (xyz) + class and intensity attributes to make the object lighter
  
  opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_hmean_norm_1m") # providing another output filepath template
  hmean_norm <- pixel_metrics(
    ctg_norm, # normalized version
    ~mean(Z), 
    res=1) # resolution in meters
  path <- paste("./Outputs_auto/hmean_norm_", zone_name, "_1m.tif", sep = "")
  terra::writeRaster(hmean_norm, filename = path)
  
  opt_output_files(ctg) <- paste0(tempdir(), "/{*}_hmean_not_norm_1m") # providing another output filepath template
  hmean <- pixel_metrics(
    ctg, # not normalized version, e.g. for SVF use 
    ~mean(Z), 
    res=1) 
  path <- paste("./Outputs_auto/hmean_not_norm_", zone_name, "_1m.tif", sep = "")
  terra::writeRaster(hmean, filename = path)
  
## Filtering the vegetation points
  
  opt_chunk_size(ctg_norm) <- 0  #processing the files entirely
  opt_chunk_buffer(ctg_norm) <- 0 #ok because not depending on a neighbouring effect
  opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_filtered_veg")
  ctg_veg <- catalog_apply(ctg_norm, filter_class_veg) # Filtering the whole catalog with the defined function
  # Merging the outputs into a single LAS file
  las_list <- lapply(ctg_veg, readLAS)
  las_merged <- do.call(rbind, las_list)
  rm(las_list)
  grid_veg <- las_merged %>% 
    grid_density(1) #resolution 1m
  rm(las_merged)
  
  path <- paste("./Outputs_auto/veg_density_", zone_name, "_1m.tif", sep = "")
  terra::writeRaster(grid_veg, filename = path)

## Filtering the water points
  
  opt_chunk_size(ctg_norm) <- 0  #processing the files entirely
  opt_chunk_buffer(ctg_norm) <- 0 #ok because not depending on a neighbouring effect
  opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_filtered_water")
  ctg_water <- catalog_apply(ctg_norm, filter_class_water)
  
  # Merging the outputs into a single LAS file
  las_list <- lapply(ctg_water, readLAS)
  las_merged <- do.call(rbind, las_list)
  rm(las_list)
  grid_water <- las_merged %>% grid_density(1)
  rm(las_merged)

  path = paste("./Outputs_auto/water_density_", zone_name, "_1m.tif", sep = "")
  terra::writeRaster(grid_water, filename = path)

  ## Sky View Factor
  hmean_raster <- raster::raster(hmean) #hmean => altitude, no the normalized version
  gc() #freeing the unused memory bc of the important needs of the 
  path = paste("./Outputs_auto/svf_", zone_name, "_1m.tif", sep = "")
  system.time({ #keeping track 
    svf_raster <- horizon::svf(x = hmean_raster, 
                               nAngles = 16, #number of angles, can be slightly reduced for faster computation
                               maxDist = 250, #distance max to be inspected, can also be adapted for faster computation
                               ll = FALSE,
                               blockSize = NULL,
                               verbose = TRUE,
                               filename = path)
  })
  
  ## Roughness index
  roughness <- terrain(hmean, 
                       v="roughness", # fonction roughness pre-existente
                       neighbors = 8 # can only be 4 or 8
  ) 
  
  path <- paste("./Outputs_auto/roughness_", zone_name, "_1m.tif", sep = "")
  terra::writeRaster(roughness, filename = path)
  
  # Removing the downloaded files in order to save some disk space
  folder_path <- "../H2C/LIDAR/Subset/"
  files_to_remove <- list.files(folder_path, full.names = TRUE)
  unlink(files_to_remove) # removing the listed files
  
  # Appending the the log the tiles that were treated
  # Append the current timestamp
  write(paste("Timestamp:", Sys.time()), file = "./Outputs_auto/tiles_treated.txt", append = TRUE)
  write("", file = "./Outputs_auto/tiles_treated.txt", append = TRUE)
  write(files_to_remove, file = "./Outputs_auto/tiles_treated.txt", append = TRUE)
  write("", file = "./Outputs_auto/tiles_treated.txt", append = TRUE)
  
  i <- i + 16 # incrementing for the next set of 16 tiles
}
