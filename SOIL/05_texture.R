
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools)
require(dplyr)
require(purrr)
require(raster)
require(rgdal)
require(rgeos)
require(gtools)
require(tidyr)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
myJoin <- function(pos, bsn){
  # pos <- 3
  # bsn <- 'Cabuyaro'
  nme <- colnames(tbl)[pos]
  cut <- raster::crop(stk, shp[shp@data$name %in% bsn,]) %>% 
    raster::mask(., shp)
  tbl <- rasterToPoints(cut) %>% 
    as_tibble() 
  x <- merge(tbl, lbl, by.x = nme, by.y = 'value') %>%
    as_tibble() %>% 
    dplyr::select(x, y, texture) %>%
    mutate(basin = bsn)
  print('Done')
  return(x)
}

calcSoil <- function(bsn, dpt){
  # bsn <- 'cby'
  # dpt <- 'sd1'
  
  lyr <- grep(dpt, fls, value = TRUE) %>%
    stack()
  
  if(bsn == 'cby'){
    rst <- raster::crop(lyr, shp[shp@data$name %in% 'Cabuyaro',]) %>% 
      raster::mask(., shp[shp@data$name %in% 'Cabuyaro',])
    
    tbl <- rasterToPoints(rst) %>%
      as_tibble()  
    s_cby <- raster::extract(u_cby, tbl[,1:2])
    tbl$ucs <- s_cby
    tbl <- tbl %>% 
      setNames(c('x', 'y', 'clay', 'silt', 'sand', 'ucs'))
    
    smm <- tbl %>% 
      mutate(ucs = as.character(ucs)) %>% 
      group_by(ucs) %>% 
      dplyr::summarise(clay_m = mean(clay, na.rm = TRUE),
                       silt_m = mean(silt, na.rm = TRUE),
                       sand_m = mean(sand, na.rm = TRUE)) %>% 
      ungroup() %>% 
      drop_na() %>%
      mutate(basin = bsn,
             depth = dpt)
    
  } else {
    rst <- raster::crop(lyr, shp[shp@data$name %in% 'Tua',]) %>% 
      raster::mask(., shp[shp@data$name %in% 'Tua',])
    tbl <- rasterToPoints(rst) %>%
      as_tibble()  
    s_tua <- raster::extract(u_tua, tbl[,1:2])
    tbl$ucs <- s_tua
    tbl <- tbl %>% 
      setNames(c('x', 'y', 'clay', 'silt', 'sand', 'ucs'))
    
    smm <- tbl %>% 
      mutate(ucs = as.character(ucs)) %>% 
      group_by(ucs) %>% 
      dplyr::summarise(clay_m = mean(clay, na.rm = TRUE),
                       silt_m = mean(silt, na.rm = TRUE),
                       sand_m = mean(sand, na.rm = TRUE)) %>% 
      ungroup() %>% 
      drop_na()%>%
      mutate(basin = bsn,
             depth = dpt)
  }
  
  return(smm)
}

# Load data ---------------------------------------------------------------
fls <- list.files('./tif/isric/bsn', full.names = TRUE) %>%
  grep('dec', ., value = T)
lbl <- data.frame(value = 1:6, texture = c('C', 'SiC', 'SiCL', 'SC', 'SCL', 'CL'))
shp <- shapefile('shp/base/basins_geo.shp')
stk <- stack(fls) # Convert to raster
u_cby <- raster('./tif/isric/base/msk_cby_soils.tif') * 1
u_tua <- raster('./tif/isric/base/msk_tua_soils.tif') * 1


# Apply the function ------------------------------------------------------
calcSoil(bsn)
calcSoil(bsn = 'cby', dpt = 'sd2')

# Cabuyaro
cby_tbl <- lapply(1:6, function(k) calcSoil(bsn = 'cby', dpt = paste0('sd', k)))
tua_tbl <- lapply(1:6, function(k) calcSoil(bsn = 'tua', dpt = paste0('sd', k)))

Map('write.csv', cby_tbl, paste0('./tbl/tbl_cby_sd', 1:6, '.csv'), row.names = FALSE)
Map('write.csv', tua_tbl, paste0('./tbl/tbl_tua_sd', 1:6, '.csv'), row.names = FALSE)

nms <- c('Cabuyaro', 'Tua')

dfm <- lapply(1:2, function(y){
  lapply(3:8, function(k){
    myJoin(pos = k, bsn = nms[y])
  })
})

cby <- dfm[[1]]
tua <- dfm[[2]]

cby <- lapply(1:length(cby), function(k){
  setNames(cby[[k]], c('x', 'y', paste0('texture_', k), 'basin'))
})

tua <- lapply(1:length(tua), function(k){
  setNames(tua[[k]], c('x', 'y', paste0('texture_', k), 'basin'))
})


cb2 <- bind_cols(cby) %>% 
  dplyr::select(x, y, texture_1, texture_2, texture_3, texture_4, texture_5, texture_6)

