

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
drs <- list.dirs('tif/isric/', full.names = F, recursive = F)
fls <- list.files(paste0('tif/isric/', drs), full.names = T, pattern = '.tif$') %>% 
  grep('_M_', ., value = T)
stk <- stack(fls)
bsn <- shapefile('shp/base/basins_geo.shp')

# Extract data ------------------------------------------------------------
stk_cut <- raster::crop(stk, bsn)
stk_msk <- raster::mask(stk_cut, bsn)
nms <- names(stk_msk)
stk_msk <- unstack(stk_msk)

Map('writeRaster', x = stk_msk, filename = paste0('tif/isric/bsn/', nms, '.tif'), overwrite = TRUE)




