
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, stringr, reshape, readxl)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
tidyTbl <- function(stt){
  # st <- 35195020
  print(paste('To start', st))
  tb <- tbl %>% 
    filter(stt == st) %>%
    mutate(month = factor(month, levels = month.abb)) %>%
    arrange(year, month, day) %>% 
    spread(variable, value) %>% 
    dplyr::select(tmax, tmin) %>% 
    setNames(c('19950101',''))
  
  unique(tb[,1] > tb[,2])
  tb$cmp <- tb[,1] > tb[,2]
  
  tb[which(tb[,3] == FALSE),]
  
  write.table(tb, paste0('ideam/swt/tmp_', st, '.csv'), row.names = F, col.names = T, sep = ',', quote = FALSE)
  
}

# Load data ---------------------------------------------------------------
tbl <- read_csv('ideam/int/tbl_int_all.csv') %>% filter(stt != 1)
bss <- read_excel('bisiestos.xlsx') %>% mutate(Month = factor(Month, levels = month.abb))
stt <- unique(tbl$stt)

# Review all the days -----------------------------------------------------
smm <- tbl %>% 
  group_by(variable, stt, year, month) %>% 
  summarize(count = n()) %>%
  ungroup() %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  arrange(stt, year, month)
rvw <- inner_join(smm, bss, by = c('year' = 'Year', 'month' = 'Month')) %>%
  mutate(comparison = count == Days)
rvw %>% filter(comparison == F)

# Apply the function ------------------------------------------------------
ini <- read_csv('ideam/prc/ideam_all.csv')
ini %>% 
  filter(variable == 'tmin' &
         stt == 35095110 &
         year == 2004 &
         month == 'Feb') %>%
  View()

tbl %>% 
  filter()



