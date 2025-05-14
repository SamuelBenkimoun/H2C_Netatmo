library(dplyr)
library(lidR)
library(terra)
library(sf)

##IN CASE OF A QUICK RESTART NEEDED
#ctg <- readLAScatalog("../H2C/LIDAR/")
#dtm <- terra::rast("./Outputs/dtm_Paris13_1m.tif")
#opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm")
#ctg_norm <- normalize_height(ctg, dtm) 

# Reading the collection of LIDAR files
ctg <- readLAScatalog("../H2C/LIDAR/")
ctg
plot(ctg)

# Checking the validity of the files
las_check(ctg)

# Plotting a region of interest
#roi <- clip_rectangle(ctg, xleft = 651000, xright = 655000, ybottom = 6858000, ytop=6859000)
#plot(roi, bg = "black", size = 4)
# Creating a digital terrain model, and computing the height, slope and aspect

dtm <- rasterize_terrain(ctg, res = 1, tin(), pkg = "terra") # resolution 1m
terra::writeRaster(dtm, filename = "./Outputs/dtm_Paris13_1m.tif")

dtm_prod <- terra::terrain(dtm, v = c("slope", "aspect"), unit = "radians") # Computing the slope and aspect of each cell, can be computed in degrees
plot(dtm_prod$slope, main = "Slope (in radians)", col = terrain.colors(100))
plot(dtm_prod$aspect, main = "Aspect (in radians)", col = terrain.colors(100))
terra::writeRaster(dtm_prod, filename = "./Outputs/dtm_prod_Paris13_1m.tif")

dtm_hillshade <- terra::shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect) # Adding the shades to represent the slope and aspect
terra::writeRaster(dtm_hillshade, filename = "./Outputs/dtm_shade_Paris13_1m.tif")
plot(dtm_hillshade, col = gray(0:50/50), legend = TRUE)

# Normalizing the heights, based on the terrain 
opt_output_files(ctg) <- paste0(tempdir(), "/{*}_norm")
#opt_output_files(ctg) <-  "./Outputs/Heights_Normalized/normalized_{XLEFT}_{YBOTTOM}" # providing an output filepath to store it
ctg_norm <- normalize_height(ctg, dtm) 
#ctg_norm2 <- normalize_height(ctg, tin()) # or normalizing using the points cloud
opt_select(ctg_norm) <- "xyzci" # selecting only the coordinates (xyz) + class and intensity attributes to make the object lighter

# Computing the average height of buildings within 10 meter resolution
opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_hmean_1m") # providing another output filepath template
hmean <- pixel_metrics(ctg_norm, ~mean(Z), 
                       res=1) # resolution in meters
plot(hmean, col = height.colors(25))
terra::writeRaster(hmean, filename = "./Outputs/hmean_1m_Paris13.tif")

opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_hmax_1m") # providing another output filepath template
hmax <- pixel_metrics(ctg_norm, ~max(Z), 
                      res=1) # resolution in meters
plot(hmax, col = height.colors(25))
terra::writeRaster(hmax, filename = "./Outputs/hmax_1m_Paris13.tif")

## FILTERING THE VEGETATION CLASSES AND COMPUTING THE DENSITY
filter_class_veg <- function(chunk) { # Can take only one parameter apparently ?
  las <- readLAS(chunk)
  if (npoints(las) == 0) return(NULL)  # Must check if las is empty, pattern to respect for the function to work
  
  las <- filter_poi(las, Classification %in% c(3, 4, 5)) # 5 instead of 8 ?
  return(las)
  
  # remove the buffer of the output
  bbox <- bbox(chunk)
  output <- remove_buffer(output, bbox)
  return(output)
}

opt_chunk_size(ctg_norm) <- 0  #processing the files entirely
opt_chunk_buffer(ctg_norm) <- 0 #ok because not depending on a neighbouring effect
opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_filtered_veg")
ctg_veg <- catalog_apply(ctg_norm, filter_class_veg) # Filtering the whole catalogue with the defined function

# Merging the outputs into a single LAS file
las_list <- lapply(ctg_veg, readLAS)
las_merged <- do.call(rbind, las_list)
rm(las_list)

# Producing a 2d density plot of the vegetation classes density
## outputs to be splitted by vegetation type ??
writeLAS(las_merged, "./Outputs/veg_filtered_merged.laz")
opt_select(las_merged) <- "xyzc"
st_write(obj = las_merged %>% st_as_sf(), "./Outputs/veg_filtered.gpkg") # Alternative: writing it in geopackage format for further use
          
#zstation = 30 #height of the station in case some vertical filtering is needed
#las_merged = filter_poi(las_merged, Z >= zstation - 30, Z <= zstation + 30) #still in case of vertical filtering
grid_veg <- las_merged %>% grid_density(2)
plot(grid_veg)
terra::writeRaster(grid_veg, filename = "./Outputs/veg_density_2m_Paris13.tif")

## ALTERNATIVE attempt to filter the vegetation
# ctg_veg <- ctg_norm
# opt_filter(ctg_veg) <- "-keep_class 3 4 8"
# opt_output_files(ctg_veg) <- paste0(tempdir(), "/{*}_filtered_veg_alt")
# opt_chunk_size(ctg_veg) <- 0
# opt_chunk_buffer(ctg_veg) <- 0
# ctg_veg <- catalog_retile(ctg_veg)
# 
# las_list <- lapply(ctg_veg@data$filename, readLAS)
# las_merged <- do.call(rbind, las_list)
# writeLAS(las_merged, "./Outputs/veg_filtered_merged_alt.laz")
# grid_veg_alt <- las_merged %>% grid_density(10)
# plot(grid_veg_alt)

## Applying the same approach for built-up, and water-bodies
# filter_class_built <- function(chunk) { 
#   las <- readLAS(chunk)
#   if (npoints(las) == 0) return(NULL)  
#   
#   las <- filter_poi(las, Classification == 6)
#   return(las)
#   
#   bbox <- bbox(chunk)
#   output <- remove_buffer(output, bbox)
#   return(output)
# }
filter_class_water <- function(chunk) {
  las <- readLAS(chunk)
  if (npoints(las) == 0) return(NULL)

  las <- filter_poi(las, Classification == 9)
  if (is.empty(las)) return(NULL)
  return(las)

  bbox <- bbox(chunk)
  output <- remove_buffer(output, bbox)
  return(output)
}
opt_chunk_size(ctg_norm) <- 0  #processing the files entirely
opt_chunk_buffer(ctg_norm) <- 0 #ok because not depending on a neighbouring effect
opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_filtered_water")
ctg_water <- catalog_apply(ctg_norm, filter_class_water)

# Merging the outputs into a single LAS file
las_list <- lapply(ctg_water, readLAS)
las_merged <- do.call(rbind, las_list)
rm(las_list)
writeLAS(las_merged, "./Outputs/water_filtered_merged.laz")
grid_water <- las_merged %>% grid_density(10)
plot(grid_water)
terra::writeRaster(grid_veg, filename = "./Outputs/water_density_Paris13_10m.tif")

## COMPUTING THE SVF
# Install package horizon [raster approach] and shadow [vector approach] (not on CRAN anymore)
install.packages("shadow", repos = "https://packagemanager.posit.co/cran/2021-03-15/", verbose = T)
install.packages("horizon", repos = "https://packagemanager.posit.co/cran/2018-07-15/", verbose = T)
library(shadow)
library(horizon)

# Importing the layer of obstacles (built-up + vegetation)
batiments <- st_read("../BD_TOPO/BATIMENT.shp")
# Getting the bounding box of the region to clip
bbox_polygon <- st_as_sfc(st_bbox(hmean), crs = st_crs(hmean)) 
# Expanding the bbox to avoid border effects when computing indicators
bbox_polygon <- st_buffer(bbox_polygon, dist = 500) 
# Clipping the built-up layer with the region of interest
batiments <- st_intersection(x = batiments, y = bbox_polygon) 
# Checking the geometry types of the buildings
st_geometry_type(batiments) %>% table() 
# Keeping only the polygons (presence of points)
batiments <- batiments[st_geometry_type(batiments) == "POLYGON", ] 
# Transforming to WGS84 (EPSG:4326) and checking that the selection has worked well
leaflet(st_transform(batiments, crs = 4326)) %>%
  addProviderTiles("OpenStreetMap") %>%
  addPolygons()

# Importing the vegetation layer to combine it with the built-up
high_veg <- lidR::readLAS(files = "./Outputs/veg_filtered_merged.laz", 
                          select = "xyzc", # Keeping only the coordinates and class
                          filter = "-keep_class 5 -keep_z 3 1000") # Loading only the "vegetation haute" class
# Extracting only the tree tops
ttops <- locate_trees(high_veg, 
                      lmf(10, #lmf = local maximum filter, number of neighbours considered, max value retained to smoothen the lower values, to be optimised 
                          hmin = 3)) #height considered for trees
leaflet(st_transform(ttops, crs = 4326)) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers()
# Buffering around the tree tops (can also be segmented from the points cloud)
ttops <- st_buffer(ttops, dist=2) #dist could be adapted based on phenology ?

# Merging buildings and trees
batiments <- batiments %>% select(c("ID","HAUTEUR"))
colnames(batiments) <- c("ID", "Z", "geometry")
colnames(ttops) <- c("ID", "Z", "geometry")
ttops <- st_difference(ttops, st_union(batiments)) #removing the overlapping part of the geometries
obstacles <- rbind(ttops, batiments)
obstacles <- na.omit(obstacles)

# # Getting the SVF -> for now the vectorial approach uses to much memory, needs to be optimized ?
# # => For now: raster approach favored 
# parallel::detectCores() # to parallelize the function
# #location = raster::raster(hmean)
# #obstacles = as(obstacles, "Spatial")
# 
# # Attempt to optimize the memory use
# # Define the chunk extents (1000x1000 each)
# chunk1_extent <- raster::extent(651000, 652000, 6858000, 6859000)
# chunk2_extent <- raster::extent(652000, 653000, 6858000, 6859000)
# chunk3_extent <- raster::extent(653000, 654000, 6858000, 6859000)
# chunk4_extent <- raster::extent(654000, 655000, 6858000, 6859000)
# 
# chunk1 <- crop(location, chunk1_extent)
# chunk2 <- crop(location, chunk2_extent)
# chunk3 <- crop(location, chunk3_extent)
# chunk4 <- crop(location, chunk4_extent)
# 
# obstacles_chunk1 <- obstacles %>%
#   st_intersection(
#     st_as_sfc(as(chunk1_extent, "SpatialPolygons")) %>%
#       st_set_crs(2154) %>%
#       st_buffer(250) # Adding a 250m buffer to avoid the border effect at the margins of the chunks
#   ) %>%
#   subset(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
#   as("Spatial") # Converting back to spatial object type for the svf() function
# 
# obstacles_chunk2 <- obstacles %>%
#   st_intersection(
#     st_as_sfc(as(chunk2_extent, "SpatialPolygons")) %>%
#       st_set_crs(2154) %>%
#       st_buffer(250)
#   ) %>%
#   subset(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
#   as("Spatial")
# 
# obstacles_chunk3 <- obstacles %>%
#   st_intersection(
#     st_as_sfc(as(chunk3_extent, "SpatialPolygons")) %>%
#       st_set_crs(2154) %>%
#       st_buffer(250)
#   ) %>%
#   subset(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
#   as("Spatial")
# 
# obstacles_chunk4 <- obstacles %>%
#   st_intersection(
#     st_as_sfc(as(chunk4_extent, "SpatialPolygons")) %>%
#       st_set_crs(2154) %>%
#       st_buffer(250)
#   ) %>%
#   subset(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
#   as("Spatial")
# 
# system.time({ 
#   gc()
#   svf_chunk1 <- shadow::SVF(location = chunk1, 
#                             obstacles = obstacles_chunk1, 
#                             obstacles_height_field = "Z", 
#                             res_angle = 20, # default = 5
#                             #parallel = parallel::detectCores()/4,
#                             parallel = 1)
#   gc()
# })

# Attempt with a raster approach a the horizon package: functional for now
#hmean <- rast("./Outputs/hmean_1m_Paris13.tif")
hmean_raster <- raster::raster(hmean)
gc()
system.time({
  svf_raster <- horizon::svf(x = hmean_raster, 
                    nAngles = 16, 
                    maxDist = 500,
                    ll = FALSE,
                    blockSize = NULL,
                    verbose = TRUE)
})
raster::writeRaster(svf_raster, "./Outputs/svf_rast_1m.tif")

## COMPUTING ROUGHNESS: CV height ? Diff min-max ? Matrice voisinage 8 / 1 or distance weighted ?
# Classical roughness
roughness <- terrain(hmean, 
        v="roughness", # fonction roughness pre-existente
        neighbors = 8 # can only be 4 or 8
        ) 
plot(roughness)

# Neighboring matrix to compute the standard deviation of building heights
voisinage <- matrix(1, nrow = 3, ncol = 19) #simple matrix with               
voisinage 
elev_focal_sd <- focal(hmean, w = voisinage, fun = sd)
plot(elev_focal_sd)

size <- 3 # fixing the size of the matrix
center <- ceiling(size / 2) # finding the center of the matrix
coords <- expand.grid(x = 1:size, y = 1:size) # generating all the coordinates combination
dists <- sqrt((coords$x - center)^2 + (coords$y - center)^2) # computing the euclidian distance to each pair of coordinates
weights <- 1 / (dists + 0.00001)  # fixing weights inverting the distance, avoiding division by 0
voisinage <- matrix(weights, nrow = size, byrow = FALSE) # putting the weights into matrix format
voisinage[center,center] <- 1
voisinage <- voisinage / sum(voisinage) # normalizing the values

## COMPUTING SHADOW FOOTPRINT FOR A GIVEN TIME
obstacles
#hmean <- rast("./Outputs/hmean_1m_Paris13.tif")
hmean_4326 <- project(hmean, "EPSG:4326")
# Computing the centroid coordinates in the region of interest
centroid_coords <- c(
  (ext(hmean_4326)[1] + ext(hmean_4326)[2]) / 2,  #averaging the min max long from the extent
  (ext(hmean_4326)[3] + ext(hmean_4326)[4]) / 2   #averaging the min max lat from the extent
)
rm(hmean_4326)

# Getting the sunlight timings
sunlight_time <- suncalc::getSunlightTimes(date = as.Date("2022-07-15"),
                          lat = centroid_coords[2], 
                          lon = centroid_coords[1], tz = "Europe/Paris")
# Getting the sun position (results in radians)
#suncalc::getSunlightPosition(date = as.POSIXct("2022-07-15 14:00:00", format = "%Y-%m-%d %H:%M:%S", tz= "Europe/Paris"), #be sure that the date is post sunrise !!

date_time = sunlight_time$solarNoon + 3600

solar_position <- suncalc::getSunlightPosition(date = date_time, 
                             lat = centroid_coords[2],
                             lon = centroid_coords[1])
# Formating the solar position fort the shadow_footprint function
solar_position <- as.matrix(
  data.frame(
    altitude = solar_position$altitude * (180 / pi), # converting into degrees as per requested by the shadow::shadowFootprint() function
    azimuth = solar_position$azimuth * (180 / pi) # https://www.rdocumentation.org/packages/shadow/versions/0.7.1/topics/shadowFootprint 
  )
)
## Has to be convertied into degrees from radians.
# Computing the obstacles footprint

obstacles <- obstacles %>%
  filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
  st_make_valid()
obstacles_sp <- as(obstacles, "Spatial")

shadow_results <- list()
# Computing the shadow footprints one by one for each building and combining the whole result.
# This way, problematic buildings (if any) can be easily spotted with the error message.
system.time({
  for (i in seq_len(nrow(obstacles_sp))) {
    single_poly <- obstacles_sp[i, ]
    paste("Polygon #", i, "out of", nrow(obstacles_sp)) %>% print()
   
    result <- tryCatch({
      shadow::shadowFootprint(
        obstacles = single_poly,
        obstacles_height_field = "Z",
        solar_pos = solar_position,
        b = 0.1
      )
    }, error = function(e) {
      message("Error at polygon #", i, ": ", e$message)
      return(NULL)
    })
    
    shadow_results[[length(shadow_results) + 1]] <- result
  }
})
# Formatting and writing the output file for the shadow footprints
shadow_results <- do.call(rbind, shadow_results)
shadow_results <- st_as_sf(shadow_results)
filepath <- paste("./Outputs/shadow_footprints_", 
                  gsub(pattern = " ", replacement = "_", date_time) %>% 
                    gsub(pattern = ":", replacement = "-"), 
                  ".gpkg", 
                  sep = "")
sf::st_write(shadow_results, filepath)
