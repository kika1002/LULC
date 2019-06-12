
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(stringr, tidyverse, readxl, imputeTS, purrr, readxl)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
cleanData <- function(number_row){
  # number_row <- 1
  print(paste('To start', number_row))
  rm <- filter(rmv, row_number() == number_row)
  rsl <- tb1 %>% 
    filter(!variable == pull(rm, 1) |
           !stt == pull(rm, 2) |
           !year == pull(rm, 3))
  print('Done!')
  return(rsl)
}

# Load data ---------------------------------------------------------------
tb1 <- read_csv('tables_tidy_gth_v2.csv') 
tb2 <- read_csv('ideam/prc/ideam_prc.csv')
rmv <- read_csv('toRemove.csv')

# Tidy table --------------------------------------------------------------
tb1 <- tb1 %>% 
  dplyr::select(var, stt, year, month, day, value) %>% 
  setNames(colnames(tb2))
rmv <- tb2 %>%
  distinct(variable, stt, year) 
write.csv(rmv, 'toRemove.csv', row.names = FALSE)

# Removing rows -----------------------------------------------------------
for(i in 1:nrow(rmv)){
  tb1 <- cleanData(number_row = i)
}

# Join the two tables -----------------------------------------------------
tbl <- rbind(tb1, tb2)
tb2 <- mutate(tbl, value = ifelse(value < 10 | value > 45, NA, value))
write.csv(tb2, 'ideam/prc/ideam_all.csv', row.names = FALSE)

dst <- distinct(tb2, variable, stt)

# Summary the NAs ---------------------------------------------------------
nas <- tb2 %>%
  group_by(variable, stt) %>% 
  summarize(conteo = sum(is.na(value))) %>% 
  ungroup()
write.csv(nas, 'ideam/nas_count.csv', row.names = FALSE)

sum(nas$conteo) / nrow(tb2) * 100





