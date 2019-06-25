
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, tidyverse, gtools, stringr, sf, data.table)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
myGroup <- function(tbl){
  # tbl <- tua
  print('To start')
  smm <- tbl %>% 
    group_by(GID) %>% 
    summarise_all(.funs = mean) %>% 
    ungroup() %>% 
    gather(var, value, -GID) %>% 
    group_by(GID) %>% 
    summarise(value = mean(value)) %>% 
    ungroup()
  print('Done!')
  return(smm)
}

# Load data ---------------------------------------------------------------
tua <- read_csv('tbl/albedo_tua.csv')
cby <- read_csv('tbl/albedo_cby.csv')

# Apply the function ------------------------------------------------------
tua_prc <- myGroup(tbl = tua)
cby_prc <- myGroup(tbl = cby)

write.csv(tua_prc, './tbl/albedo_tua_smm.csv', row.names = FALSE)
write.csv(cby_prc, './tbl/albedo_cby_smm.csv', row.names = FALSE)




