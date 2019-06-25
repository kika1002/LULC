
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, foreign)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Cabuyaro ----------------------------------------------------------------
tbl_cby <- read.csv('D:/_request/_casanare/_workspace/_access/tbl_all_cby_smm.csv') %>% 
  dplyr::select(SNAM) %>% 
  mutate(id = 203:218)
shp_cby <- st_read('shp/edit/soils_cby.shp') %>% 
  dplyr::select(UCS_F, GID) %>% 
  as(., 'Spatial')
sls_cby <- raster('tif/isric/base/msk_cby_soils2.tif') * 1

# Rasterize Cabuyaro
myRasterize <- function(tbl, shp, sls){
  tbl <- tbl_cby; shp <- shp_cby; sls <- sls_cby
  shp <- st_as_sf(shp)
  shp <- inner_join(shp, tbl, by = c('UCS_F' = 'SNAM'))
  rst <- raster::rasterize(shp, sls_cby, field = 'id')
  tbl <- tbl %>% dplyr::select(id, SNAM) %>% setNames(c('VALUE', 'STMUID'))
  writeRaster(x = rst, filename = 'tif/cabuyaro_sls.tif', overwrite = FALSE)
  write.dbf(dataframe = tbl, file = 'tif/soilc.dbf')
}

# Tua ---------------------------------------------------------------------
sls_tua <- raster('tif/isric/base/msk_tua_soils2.tif') * 1
tbl_tua <- read.csv('D:/_request/_casanare/_workspace/_access/tbl_all_tua_smm.csv') %>% 
  dplyr::select(SNAM) %>% 
  mutate(id = 203:230)
shp_tua <- st_read('shp/bsn/SUELOS_TUA.shp') %>% 
  dplyr::select(UCS_F, GID) %>% 
  as(., 'Spatial')

vc1 <- shp_tua %>% 
  as_tibble() %>% 
  distinct(UCS_F)
mss <- st_as_sf(shp_tua) %>% 
  filter(UCS_F %in% setdiff(pull(vc1), pull(tbl_tua, SNAM))) %>% 
  as(., 'Spatial')

plot(shp_tua)
plot(mss, add = TRUE, col = 'red')

myRasterize <- function(tbl, shp, sls){
  tbl <- tbl_tua; shp <- shp_tua; sls <- sls_tua
  shp <- st_as_sf(shp)
  shp <- inner_join(shp, tbl, by = c('UCS_F' = 'SNAM'))
  rst <- raster::rasterize(as(shp, 'Spatial'), sls * 0, field = 'id')
  st_write(obj = shp, dsn = 'tmp', layer = 'shape', driver = 'ESRI Shapefile' )
  writeRaster(sls * 0, 'tmp/mask.tif')
  
  msk <- rst * 0
  plot(msk)
  tbl <- tbl %>% dplyr::select(id, SNAM) %>% setNames(c('VALUE', 'STMUID'))
  
  
  
  writeRaster(x = rst, filename = 'tif/tua_sls.tif', overwrite = TRUE)
  write.dbf(dataframe = tbl, file = 'tif/soilc_tua.dbf')
}



