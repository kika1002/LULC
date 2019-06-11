
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, foreign)

# Initial ssetup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
lus <- shapefile('shp/lnd_use_prj.shp')
bsn <- shapefile('../_soils/shp/base/basins_geo.shp')
lus <- spTransform(x = lus, CRSobj = crs(bsn))

# Intersect basins and land use
lus_bsn <- raster::intersect(lus, bsn)
lus_bsn <- st_as_sf(lus_bsn)

lbl <- data.frame(LANDUSE = unique(lus_bsn$SWAT), value = 1:length(unique(lus_bsn$SWAT))) %>% 
  mutate(LANDUSE = as.character(LANDUSE)) %>% 
  dplyr::select(value, LANDUSE)
write.dbf(lbl, 'luc.dbf')

msk_cby <- raster('../_soils/tif/isric/base/msk_cby_soils2.tif') * 1
msk_tua <- raster('../_soils/tif/isric/base/msk_tua_soils2.tif') * 1

# Cabuyaro
myRasterize <- function(bs){
  # bs <- 'Cabuyaro'
  rsl <- lus_bsn %>% 
    dplyr::filter(name == bs) %>% 
    dplyr::select(OBJECTID_1.1, name, SWAT) %>% 
    inner_join(., lbl, by = c('SWAT' = 'LANDUSE'))
  rsl <- as(rsl, 'Spatial')
  if(bs == 'Cabuyaro'){
    lyr <- rasterize(rsl, msk_cby, field = 'value')  
  } else {
    lyr <- rasterize(rsl, msk_tua, field = 'value')  
  }
  print('Done!')
  dbf <- rsl %>% as_tibble() %>% distinct(SWAT, value)
  return(list(lyr, dbf))
}

lus_cby <- myRasterize(bs = 'Cabuyaro')
lus_tua <- myRasterize(bs = 'Tua')

dir.create('tif')
writeRaster(lus_cby[[1]], 'tif/lus_cby.tif')
writeRaster(lus_tua[[1]], 'tif/lus_tua.tif')

write.dbf(as.data.frame(lus_cby[[2]]) %>% dplyr::select(value, SWAT), './lus_cby.dbf')
write.dbf(as.data.frame(lus_tua[[2]]) %>% dplyr::select(value, SWAT), './lus_tua.dbf')


