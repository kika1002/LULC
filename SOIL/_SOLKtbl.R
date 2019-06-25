
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, stplanr)
require(tidyverse)
require(gtools)
require(stplanr)
require(readxl)
require(stringr)
require(dplyr)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

#load data-------------------------------------------------------------------
cby <- read_csv("G:/_casanare/_workspace/_soils/tbl/cby_txt.csv") 
tua <- read_csv("G:/_casanare/_workspace/_soils/tbl/tua_txt.csv")
cby.soils <- read_csv("G:/_casanare/_workspace/_soils/cby_vars_soil.csv")
tua.soils <- read_csv("G:/_casanare/_workspace/_soils/tua_vars_soil.csv") 
str(cby)
str(tua)

#Group by--------------------------------------------------------------------
df_cby <- cby %>% 
  select(GID, SNDPPT_sd1_M_04_dec_2013:SLTPPT_sd6_M_04_dec_2013) %>%
  group_by(GID) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup()

df_tua <- tua %>% 
  select(GID, SNDPPT_sd1_M_04_dec_2013:SLTPPT_sd6_M_04_dec_2013) %>%
  group_by(GID) %>% 
  summarise_all(funs(mean)) %>% 
  ungroup()

tbl_tua <- tua %>% 
  select(GID, SNDPPT_sd1_M_04_dec_2013:SLTPPT_sd6_M_04_dec_2013) %>% 
  group_by(GID) %>% 
  summarise(SNDPPT_d1 = sum(SNDPPT_sd1_M_04_dec_2013)) %>%
  ungroup()
  

B <- aggregate(SNDPPT_sd1_M_04_dec_2013 ~ GID, cby, sum)

#jointables------------------------------------------------------------------

cby.solk <- merge(x = df_cby, y = cby.soils, by.x = "GID", by.y = "ucs", all = TRUE)
tua.solk <- merge(x = df_tua, y = tua.soils, by.x = "GID", by.y = "ucs", all = TRUE)

#to obtain OM in cby.solk and tua.solk --------------------------------------

cby.solk.om <- cby.solk %>% 
  mutate(mo_sd1 = 1.72*soc_sd1, mo_sd2 = 1.72*soc_sd2, mo_sd3 = 1.72*soc_sd3, mo_sd4 = 1.72*soc_sd4, mo_sd5 = 1.72*soc_sd5, mo_sd6 = 1.72*soc_sd6 )

tua.solk.om <- tua.solk %>% 
  mutate(mo_sd1 = 1.72*soc_sd1, mo_sd2 = 1.72*soc_sd2, mo_sd3 = 1.72*soc_sd3, mo_sd4 = 1.72*soc_sd4, mo_sd5 = 1.72*soc_sd5, mo_sd6 = 1.72*soc_sd6 )

#to write the table---------------------------------------------------------

write.csv(df_cby, paste0('G:/_casanare/_workspace/_soils/tbl/df_cby.csv'), row.names = FALSE)
write.csv(df_tua, paste0('G:/_casanare/_workspace/_soils/tbl/df_tua.csv'), row.names = FALSE)
write.csv(cby.solk, paste0('G:/_casanare/_workspace/_soils/tbl/cby_solk.csv'), row.names = FALSE)
write.csv(tua.solk, paste0('G:/_casanare/_workspace/_soils/tbl/tua_solk.csv'), row.names = FALSE)
write.csv(cby.solk.om, paste0('G:/_casanare/_workspace/_soils/tbl/cby_solk_om.csv'), row.names = FALSE)
write.csv(tua.solk.om, paste0('G:/_casanare/_workspace/_soils/tbl/tua_solk_om.csv'), row.names = FALSE)



