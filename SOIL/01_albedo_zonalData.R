
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, gtools, stringr, sf, data.table)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
myExtract <- function(bsn){
  cut <- raster::crop(stk, bsn)
  cut <- raster::mask(cut, bsn)
  print('Done')
  return(cut)  
}
rst2plg <- function(lyr, bsn){
  # lyr <- alb_tua[[2]]
  # bsn <- 'Tua'
  alb <- rasterToPolygons(lyr)
  if(bsn == 'Tua'){
    rsl <- raster::intersect(sls_tua, alb)
    nme <- str_sub(names(lyr), start = nchar(names(lyr)) - 21, end = nchar(names(lyr)) - 14)
    sft <- st_as_sf(rsl)
    sft <- sft %>% dplyr::select(GID, names(lyr)) 
    colnames(sft) <- c('GID', nme, 'geometry')
    tbl <- as_tibble(sft) %>% dplyr::select(-geometry)  
  } else {
    rsl <- raster::intersect(sls_cby, alb)
    nme <- str_sub(names(lyr), start = nchar(names(lyr)) - 21, end = nchar(names(lyr)) - 14)
    sft <- st_as_sf(rsl)
    sft <- sft %>% dplyr::select(GID, names(lyr)) 
    colnames(sft) <- c('GID', nme, 'geometry')
    tbl <- as_tibble(sft) %>% dplyr::select(-geometry)  
  }
  return(tbl)
}
myDiv <- function(x){x/100}

# Load data ---------------------------------------------------------------
fls <- list.files('tif/ext', full.names = TRUE, pattern = '.tif$')
stk <- stack(fls)
bsn_cby <- shapefile('shp/extent_cby.shp')
bsn_tua <- shapefile('shp/extent_tua.shp')
sls_cby <- shapefile('../_soils/shp/edit/soils_cby.shp')
sls_tua <- shapefile('../_soils/shp/bsn/SUELOS_TUA.shp')

# Plot
plot(stk[[1]])
plot(bsn, add = TRUE)

# Review the coordinate system
as.character(crs(stk)) == as.character(crs(bsn_cby))

# Extract by mask ---------------------------------------------------------
alb_tua <- unstack(myExtract(bsn = bsn_tua))
alb_cby <- unstack(myExtract(bsn = bsn_cby))
Map('writeRaster', x = alb_tua, filename = paste0('tif/bsn/tua_alb_', month.abb, '.tif'))
Map('writeRaster', x = alb_cby, filename = paste0('tif/bsn/cby_alb_', month.abb, '.tif'))

# Apply the function ------------------------------------------------------
tua_alb <- map2(.x = alb_tua, .y = rep('Tua', 12), .f = rst2plg)
cby_alb <- map2(.x = alb_cby, .y = rep('Cabuyaro', 12), .f = rst2plg)

tua_al2 <- Reduce(cbind, tua_alb)[,c(1,2,4,6,8,10,12,14,16,18,20,22,24)]
cby_al2 <- Reduce(cbind, cby_alb)[,c(1,2,4,6,8,10,12,14,16,18,20,22,24)]

tua_al2 <- as_tibble(tua_al2)
cby_al2 <- as_tibble(cby_al2)

tua_al2 <- tua_al2 %>% mutate_at(vars(starts_with("MONTH")), funs(myDiv))
cby_al2 <- cby_al2 %>% mutate_at(vars(starts_with("MONTH")), funs(myDiv))

write.csv(tua_al2, 'tbl/albedo_tua.csv', row.names = F)
write.csv(cby_al2, 'tbl/albedo_cby.csv', row.names = F)



