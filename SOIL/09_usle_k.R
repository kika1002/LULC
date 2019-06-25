
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
selectTxt <- function(tbl){
  rsl <- tbl %>%
    dplyr::select(GID, x, y, contains('sd1')) %>%
    group_by(GID) %>% 
    summarise(sand = mean(SNDPPT_sd1_M_04_dec_2013, na.rm = T),
              silt = mean(SLTPPT_sd1_M_04_dec_2013, na.rm = T),
              clay = mean(CLYPPT_sd1_M_04_dec_2013 , na.rm = T))
  return(rsl)
  print('Done!')
}

selectVrs <- function(tbl){
  # tbl <- cby_vrs
  tbl <- tbl %>%
    dplyr::select(ucs, contains('sd1')) %>% 
    dplyr::select(ucs, soc_sd1)
  print('Done!')
  return(tbl)
}
calcVars <- function(tbl){
  # tbl <- cby
  tbl <- tbl %>% 
    mutate(cl_si = (silt / (clay + silt)) ^ 0.3,
           csand = (0.2 + 0.3 * exp(-0.256 * sand * (1 - (silt / 100)))),
           forgc = 1 - (0.0256 * soc_sd1) / soc_sd1 + exp(3.72 - 2.95 * soc_sd1),
           hisand_num = 0.7 * (1 - sand /  100),
           hisand_dnm = (1-sand/100) + exp(-5.51 + 22.9 * (1 - (sand/ 100))),
           hisand = 1 - (hisand_num / hisand_dnm),
           k_usle = cl_si * forgc * csand * hisand) %>% 
    dplyr::select(-hisand_num, -hisand_dnm)
  print('Done!')
  return(tbl)
}

# Load data ---------------------------------------------------------------
cby_txt <- read_csv('tbl/cby_txt.csv')
tua_txt <- read_csv('tbl/tua_txt.csv')
cby_vrs <- read_csv('tbl/cby_vars_soil.csv')
tua_vrs <- read_csv('tbl/tua_vars_soil.csv')

# Selecting the columns ---------------------------------------------------
cby_txt <- selectTxt(tbl = cby_txt)
tua_txt <- selectTxt(tbl = tua_txt)

cby_vrs <- selectVrs(tbl = cby_vrs)
tua_vrs <- selectVrs(tbl = tua_vrs)

# Join all the tables -----------------------------------------------------
cby <- inner_join(cby_txt, cby_vrs, by = c('GID' = 'ucs'))
tua <- inner_join(tua_txt, tua_vrs, by = c('GID' = 'ucs'))

# All the vars ------------------------------------------------------------
cby_fnl <- calcVars(tbl = cby)
tua_fnl <- calcVars(tbl = tua)

write.csv(cby_fnl, 'tbl/cby_all_vrs.csv', row.names = FALSE)
write.csv(tua_fnl, 'tbl/tua_all_vrs.csv', row.names = FALSE)

