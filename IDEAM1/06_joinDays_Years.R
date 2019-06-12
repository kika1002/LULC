
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(stringr, tidyverse, readxl, lubridate, imputeTS, reshape, purrr, readxl, reshape)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Unir las tablas de interpolado days con interpolado anios, pero eliminando primero de la tabla
# interpolada anios la tabla de problemas days, y luego si unir este resultado, 
# con la tabla de datos por day
# Como ultimo paso, llenar los NAS para los days de febrero 29, y luego escribirlo en el formato SWAT

# Load data ---------------------------------------------------------------
tbl_day <- read_csv('ideam/int/tbl_int_days.csv')
tbl_yrs <- read_csv('ideam/int/tbl_int_years.csv')
trb_day <- read_csv('ideam/int/data_nas.csv')

# Join the tables (years with day problems)
tbl_2 <- inner_join(tbl_yrs, trb_day, by = c('variable', 'stt', 'year', 'month', 'day')) %>%
  dplyr::select(-value.y) %>% 
  dplyr::rename(value = value.x) %>% 
  dplyr::select(-count)

tbl_day %>% 
  dplyr::filter(variable == 'tmax' &
                  stt == 35055010 &
                  year == 2015 &
                  month == 'Oct' &
                  day == 1)


# Join the tables with rbind ---------------------------------------------
tbl <- rbind(tbl_day, tbl_2, .keep_all = T)
tbl <- tbl %>% 
  distinct(variable, stt, year, month, day, value) %>% 
  drop_na()
write.csv(tbl, 'ideam/int/tbl_int_all.csv', row.names = FALSE)



