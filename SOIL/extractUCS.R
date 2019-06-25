

# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeso, stringr, rgeos, velox, sf)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use
rst2plg <- function(lyr, bsn, sls){
  # lyr <- hdr_cby
  # bsn <- 'Cby'
  # sls <- sls_cby
  pol <- rasterToPolygons(lyr, dissolve = TRUE)
  colnames(pol@data) <- 'HDR'
  rsl <- raster::intersect(sls, pol)
  rsl <- st_as_sf(rsl)
  rsl <- rsl %>% dplyr::select(GID, HDR)
  
  tbl <- as.data.frame(rsl) %>% 
    dplyr::select(-geometry) %>% 
    as_data_frame() %>% 
    setNames(c('GID', 'HDR')) %>% 
    inner_join(., lbl, by = c('HDR' = 'value'))
  tbl <- tbl %>%
    group_by(GID) %>% 
    summarise(hdr = modal(HDR)) %>% 
    ungroup()
  print('Done!')
  return(tbl)
}

# Load data
hdr_cby <- raster('Global_Hydrologic_Soil_Group_1566/data/hdr_cby.tif')
hdr_tua <- raster('Global_Hydrologic_Soil_Group_1566/data/hdr_tua.tif')

sls_cby <- shapefile('Global_Hydrologic_Soil_Group_1566/data/soils_cby.shp')
sls_tua <- shapefile('Global_Hydrologic_Soil_Group_1566/data/SUELOS_TUA.shp')

lbl <- data.frame(value = c(1:4, 11:14), class = c('A', 'B', 'C', 'D', 'A/D', 'B/D', 'C/D', 'D/D') )

# Apply the function
tb_cby <- rst2plg(lyr = hdr_cby, bsn = 'cby', sls = sls_cby)
tb_tua <- rst2plg(lyr = hdr_tua, bsn = 'tua', sls = sls_tua)

tb_cby <- tb_cby %>%
  mutate(GID = as.numeric(GID)) %>% 
  inner_join(., lbl, by = c('hdr' = 'value'))
tb_tua <- tb_tua %>%
  mutate(GID = as.numeric(GID)) %>% 
  inner_join(., lbl, by = c('hdr' = 'value'))

write.csv(tb_cby, 'Global_Hydrologic_Soil_Group_1566/data/hdr_cby.csv', row.names = FALSE)
write.csv(tb_tua, 'Global_Hydrologic_Soil_Group_1566/data/hdr_tua.csv', row.names = FALSE)


unique(tb_cby$GID)
unique(tb_tua$GID)






