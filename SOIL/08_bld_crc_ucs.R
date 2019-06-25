
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
myFunction <- function(fle, bsn){
  # fle <- fls[1]
  # bsn <- shp[shp@data$name %in% 'Cabuyaro',]
  rst <- raster(fle)
  nme <- bsn@data$name
  lyr <- raster::crop(rst, bsn) %>%
    mask(., bsn)
  tbl <- rasterToPoints(lyr) %>% 
    as_tibble()
  
  if(nme == 'Cabuyaro'){
    s_vls <- raster::extract(u_cby, tbl[,1:2])
    tbl$ucs <- s_vls
    tbl <- setNames(tbl, c('x', 'y', 'value', 'ucs'))
    smm <- tbl %>% 
      mutate(ucs = as.character(ucs)) %>% 
      group_by(ucs) %>% 
      dplyr::summarise(value_m = mean(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      drop_na() %>% 
      mutate(basin = nme,
             depth = parse_number(fle),
             var = basename(fle))
    nm <- unique(smm$var)
    nm <- gsub('.tif', '', nm)
    smm <- smm %>% 
      dplyr::select(ucs, value_m, basin)
    colnames(smm)[2] <- nm
    
  } else {
    s_vls <- raster::extract(u_tua, tbl[,1:2])
    tbl$ucs <- s_vls
    tbl <- setNames(tbl, c('x', 'y', 'value', 'ucs'))
    smm <- tbl %>% 
      mutate(ucs = as.character(ucs)) %>% 
      group_by(ucs) %>% 
      dplyr::summarise(value_m = mean(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      drop_na() %>% 
      mutate(basin = nme,
             depth = parse_number(fle),
             var = basename(fle)) 
    nm <- unique(smm$var)
    nm <- gsub('.tif', '', nm)
    smm <- smm %>% 
      dplyr::select(ucs, value_m, basin)
    colnames(smm)[2] <- nm
  }
  print('Done!')
  return(smm)
}

# Load data ---------------------------------------------------------------
fls <- list.files('tif/isric/bsn', full.names = TRUE)
vrs <- c('bld', 'CRFVOL', 'soc')
fls <- grep(paste(vrs, collapse = '|'), fls, value = TRUE)
stk <- grep(paste0(vrs, collapse = '|'), fls, value = TRUE) %>% 
  stack()
shp <- shapefile('./shp/base/basins_geo.shp')
u_cby <- raster('./tif/isric/base/msk_cby_soils.tif') * 1
u_tua <- raster('./tif/isric/base/msk_tua_soils.tif') * 1

# Apply the function ------------------------------------------------------
cby <- lapply(1:length(fls), function(k) myFunction(fle = fls[k], bsn = shp[shp@data$name %in% 'Cabuyaro',]))
tua <- lapply(1:length(fls), function(k) myFunction(fle = fls[k], bsn = shp[shp@data$name %in% 'Tua',]))

cby <- Reduce(inner_join, cby)
tua <- Reduce(inner_join, tua)

write.csv(cby, './tbl/cby_vars_soil.csv', row.names = FALSE)
write.csv(tua, './tbl/tua_vars_soil.csv', row.names = FALSE)







