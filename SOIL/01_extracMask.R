

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, stplanr)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

# Load data ---------------------------------------------------------------
fls <- list.files('shp', full.names = F, pattern = '.shp$')  
shp <- list.files('shp', full.names = T, pattern = '.shp$') %>% 
  map(.x = ., .f = shapefile)
bsn <- shapefile('../_curvaHipsometrica/shp/basins.shp')

# Coordinate Systems ------------------------------------------------------
prj <- rep(as.character(crs(bsn)), 6)
shp <- map2(.x = shp, .y = prj, .f = spTransform)

# Casanare clip -----------------------------------------------------------
bsn_tua <- bsn[bsn@data$name == 'Tua',]
bsn_cby <- bsn[bsn@data$name == 'Cabuyaro',]

# To make the clip for the basin zone -------------------------------------
sls_tua <- raster::intersect(bsn_tua, shp[[4]])
writeOGR(obj = sls_tua, dsn = 'shp/bsn', layer = 'SUELOS_TUA', driver = 'ESRI Shapefile')

