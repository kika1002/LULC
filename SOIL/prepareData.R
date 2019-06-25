

# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeso, stringr, rgeos, velox, sf)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
lyr <- raster('Global_Hydrologic_Soil_Group_1566/data/HYSOGs250m.tif')
lyr <- lyr * 1
shp <- shapefile('../_soils/shp/base/basins_geo.shp')
lbl <- data.frame(value = c(1:4, 11:14), class = c('A', 'B', 'C', 'D', 'A/D', 'B/D', 'C/D', 'D/D') )

# Subsetting basins
cby_shp <- shp[shp@data$name %in% 'Cabuyaro',]
tua_shp <- shp[shp@data$name %in% 'Tua',]

# Extract by mask
lyr_cby <- raster::crop(lyr, cby_shp) %>% raster::mask(., cby_shp)
lyr_tua <- raster::crop(lyr, tua_shp) %>% raster::mask(., tua_shp)

plot(lyr_cby)
plot(lyr_tua)

# Write rasters
writeRaster(lyr_cby, 'Global_Hydrologic_Soil_Group_1566/data/hdr_cby.tif', overwrite = TRUE)
writeRaster(lyr_tua, 'Global_Hydrologic_Soil_Group_1566/data/hdr_tua.tif', overwrite = TRUE)





