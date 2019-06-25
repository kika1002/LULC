
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, stplanr)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
rst2tbl <- function(fle){
  # fle <- fls[1]
  print(paste0('To start ', fle))
  rst <- raster(fle)
  nme <- gsub('.tif', '', basename(fle))
  
  print('Cabuyaro')
  rst_cby <- raster::crop(rst, sls_cby) %>% raster::mask(sls_cby)
  pnt_cby <- rasterToPoints(rst_cby) %>% 
    as_tibble() %>% 
    setNames(c('x', 'y', 'value')) 
  coordinates(pnt_cby) <- ~ x + y
  crs(pnt_cby) <- crs(sls_cby)
  
  cby <- sls_cby[,c('UCS_F', 'GID')]
  cby <- raster::intersect(pnt_cby, cby)
  
  tbl <- as_tibble(cby) 
  colnames(tbl)[[3]] <- nme
  write.csv(tbl, paste0('tbl/cby_',  nme, '.csv'), row.names = FALSE)
  
  print('Tua')
  rst_tua <- raster::crop(rst, sls_tua) %>% raster::mask(sls_tua)
  pnt_tua <- rasterToPoints(rst_tua) %>% 
    as_tibble() %>% 
    setNames(c('x', 'y', 'value')) 
  coordinates(pnt_tua) <- ~ x + y
  crs(pnt_tua) <- crs(sls_tua)
  
  tua <- sls_tua[,c('UCS_F', 'GID')]
  tua <- raster::intersect(pnt_tua, tua)
  
  tbl <- as_tibble(tua) 
  colnames(tbl)[[3]] <- nme
  write.csv(tbl, paste0('tbl/tua_',  nme, '.csv'), row.names = FALSE)
  
  print('Done')

}

# Load data ---------------------------------------------------------------
# Masks
fls <- list.files('tif/isric/bsn', full.names = TRUE, pattern = '.tif$')
msk_cby <- raster('tif/isric/base/msk_cby_soils.tif') * 1
msk_tua <- raster('tif/isric/base/msk_tua_soils.tif') * 1

# Soils IGAC
sls_cby <- shapefile('shp/edit/soils_cby.shp')
sls_tua <- shapefile('shp/bsn/SUELOS_TUA.shp')
fls <- list.files('tif', full.names = T, pattern = '_geo.tif$')
soils <- stack(fls)
bsn <- c('tua', 'cby')

# Selecting the texture ---------------------------------------------------
map(.x = fls, .f = rst2tbl)





