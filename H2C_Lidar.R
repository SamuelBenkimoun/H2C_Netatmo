library(lidR)
#library(plotly)
library(terra)
library(sf)

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

dtm <- rasterize_terrain(ctg, 2, tin(), pkg = "terra")
terra::writeRaster(dtm, filename = "./Outputs/dtm_Paris13.tif")

dtm_prod <- terra::terrain(dtm, v = c("slope", "aspect"), unit = "radians") # Computing the slope and aspect of each cell, can be computed in degrees
plot(dtm_prod$slope, main = "Slope (in radians)", col = terrain.colors(100))
plot(dtm_prod$aspect, main = "Aspect (in radians)", col = terrain.colors(100))
terra::writeRaster(dtm_prod, filename = "./Outputs/dtm_prod_Paris13.tif")

dtm_hillshade <- terra::shade(slope = dtm_prod$slope, aspect = dtm_prod$aspect) # Adding the shades to represent the slope and aspect
terra::writeRaster(dtm_hillshade, filename = "./Outputs/dtm_shade_Paris13.tif")
plot(dtm_hillshade, col = gray(0:50/50), legend = TRUE)

# Normalizing the heights, based on the terrain 
opt_output_files(ctg) <-  paste0(tempdir(), "/{*}_norm") # providing an output filepath template
ctg_norm <- normalize_height(ctg, dtm) 
#ctg_norm2 <- normalize_height(ctg, tin()) # or normalizing using the points cloud
opt_select(ctg_norm) <- "xyzci" # selecting only the coordinates (xyz) + class and intensity attributes to make the object lighter

# Computing the average height of buildings within 10 meter resolution
opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_hmean") # providing another output filepath template
hmean <- pixel_metrics(ctg_norm, ~mean(Z), 
                       res=10) # resolution in meters
plot(hmean, col = height.colors(25))
terra::writeRaster(hmean, filename = "./Outputs/hmean_10m_Paris13.tif")

opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_hmax") # providing another output filepath template
hmax <- pixel_metrics(ctg_norm, ~max(Z), 
                      res=10) # resolution in meters
plot(hmax, col = height.colors(25))
terra::writeRaster(hmax, filename = "./Outputs/hmax_10m_Paris13.tif")

# Locating the tree tops
opt_output_files(ctg_norm) <- paste0(tempdir(), "/{*}_treetops") # providing another output filepath template
ttops <- locate_trees(ctg_norm, 
                      lmf(4, #lmf = local maximum filter, number of neighbours considered, max value retained to smoothen the lower values 
                          hmin = 1)) #minimum height to consider a tree, usually set on 2
ttops_sf <- do.call(rbind, lapply(ttops, st_read))
head(ttops_sf)
plot(ttops_sf["Z"], pch = 19, cex = 0.4, key.pos = 1, pal = height.colors)
st_write(ttops_sf, "./Outputs/treetops_hmin1.gpkg")

# Voxel plot
vox <- voxelize_points(ctg_norm, 6)
vox <- readLAS(vox)
plot(las_merged, voxel = TRUE, bg = "white")



## FILTERING THE VEGETATION CLASSES AND COMPUTING THE DENSITY
filter_class_veg <- function(chunk) { # Can take only one parameter apparently ?
  las <- readLAS(chunk)
  if (npoints(las) == 0) return(NULL)  # Must check if las is empty, pattern to respect for the function to work
  
  las <- filter_poi(las, Classification %in% c(3, 4, 8))
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
writeLAS(las_merged, "./Outputs/veg_filtered_merged.laz")
grid_veg <- las_merged %>% grid_density(10)
plot(grid_veg)


## ALTERNATIVE attempt
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
filter_class_build <- function(chunk) { 
  las <- readLAS(chunk)
  if (npoints(las) == 0) return(NULL)  
  
  las <- filter_poi(las, Classification == 6)
  return(las)
  
  bbox <- bbox(chunk)
  output <- remove_buffer(output, bbox)
  return(output)
}
filter_class_water <- function(chunk) { 
  las <- readLAS(chunk)
  if (npoints(las) == 0) return(NULL)  
  
  las <- filter_poi(las, Classification == 9)
  return(las)
  
  bbox <- bbox(chunk)
  output <- remove_buffer(output, bbox)
  return(output)
}

ctg_build <- catalog_apply(ctg_norm, filter_class_build)
ctg_water <- catalog_apply(ctg_norm, filter_class_water)


# FILTERING BY HEIGHT ?





