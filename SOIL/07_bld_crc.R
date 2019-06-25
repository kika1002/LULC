
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
vrs <- c('BLD', 'CRF', 'ORC')
bsn <- shapefile('./shp/base/basins_geo.shp')
drs <- list.dirs('./tif/isric/wrl', full.names = TRUE, recursive = FALSE) %>%
  grep(paste(vrs, collapse = '|'), ., value = TRUE)
fls <- list.files(drs, full.names = TRUE, pattern = '_M_') 
stk <- stack(fls)

cut <- raster::crop(stk, bsn) %>%
  raster::mask(., bsn)
nms <- names(cut)
cut <- unstack(cut)
Map('writeRaster', x = cut, filename = paste0('./tif/isric/bsn/', nms, '.tif'), overwrite = TRUE)

# Units conversion --------------------------------------------------------
fls <- list.files('./tif/isric/bsn', full.names = T, pattern = '.tif')

# Bulk density kg / m3 to g / cm3
bld <- grep('BLD', fls, value = TRUE) %>% 
  stack()
bld <- bld / 1000
bld <- unstack(bld)
Map('writeRaster', bld, paste0('./tif/isric/bsn/bld_sd', 1:6, '.tif'), overwrite = TRUE)

# Soil Organic Carbon Content
soc <- grep('ORC', fls, value = TRUE) %>% 
  stack()
soc <- soc / 10
soc <- unstack(soc)
Map('writeRaster', soc, paste0('./tif/isric/bsn/soc_sd', 1:6, '.tif'), overwrite = TRUE)















