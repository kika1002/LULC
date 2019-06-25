
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
myJoin <- function(bsn){
  # bsn <- 'tua'
  tbl <- grep(bsn, fls, value = TRUE) %>% 
    map(.x = ., .f = read_csv)
  tbl <- lapply(1:length(tbl), function(k){
    tbl[[k]] <- tbl[[k]] %>% 
      dplyr::select(ucs, TEXTURE) %>% 
      setNames(c('ucs', paste0('texture_', k)))
  })
  
  tbl <- Reduce(inner_join, tbl)
  tbl <- tbl %>% 
    mutate(TEXTURE = paste(texture_1, texture_2, texture_3, texture_4, texture_5, texture_6, sep = '-'))
  return(tbl)
}

# Load data ---------------------------------------------------------------
fls <- list.files('./tbl/txt', full.names = TRUE, pattern = '.csv$') %>%
  mixedsort()

# Apply the function ------------------------------------------------------
cby <- map(.x = 'cby', .f = myJoin)
tua <- map(.x = 'tua', .f = myJoin)

write.csv(cby, './tbl/texture_cby.csv', row.names = FALSE)
write.csv(tua, './tbl/texture_tua.csv', row.names = FALSE)







